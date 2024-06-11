library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(forcats)
library(emmeans)


# Options -----------------------------------------------------------------
options(warn = -1)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

relationship.colors <- c("#B87FFF", "#35ACFF", "#00A08A")

# Load data ---------------------------------------------------------------

d <-
  read.csv(here('data/2b_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  rename("higher" = "more",
         "lower" = "less") %>%
  mutate(
    social_interaction = case_when(
      social_interaction == "precedent" ~ "repeating",
      social_interaction == "reciprocity" ~ "alternating",
      .default = social_interaction
    )
  ) %>%
  pivot_longer(
    cols = c("higher", "equal", "lower"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    social_interaction = fct_relevel(social_interaction,
                                     "repeating", "alternating", "no_interaction"),
    relationship = fct_relevel(relationship,
                               "higher", "lower", "equal")
  ) %>%
  group_by(subject_id, story, social_interaction) %>%
  mutate(total_rating = sum(likert_rating, na.rm = T),
         normalized_likert_rating = likert_rating / total_rating) %>%
  select(-total_rating)

write.csv(d, here('data/2b_tidy_data.csv'), row.names=FALSE)


# Demographics ------------------------------------------------------------

d.demographics <- read.csv(here('data/2b_demographics.csv'))
print(length(unique(d.demographics$subject_id)))

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

print(length(unique(d$subject_id)))



# Plots -------------------------------------------------------------------


d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, social_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)

# Control

f = ggplot(data = d %>% filter(social_interaction == "no_interaction"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "no_interaction"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "no_interaction"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = relationship.colors,
    name = "power/status of altruistic person"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of altruistic person") +
  theme(legend.position = "bottom")

f

ggsave(here("figures/outputs/2b_violin_control.pdf"),
       width = 6,
       height = 7.5)


f = ggplot(data = d %>% filter(social_interaction == "repeating"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "repeating"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "repeating"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = relationship.colors,
    name = "power/status of generous person"
    # breaks = c("higher", "equal", "lower")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of generous person") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/outputs/2b_violin_prec.pdf"),
       width = 6,
       height = 7.5)



f = ggplot(data = d %>% filter(social_interaction == "alternating"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "alternating"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "alternating"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = relationship.colors,
    name = "power/status of generous person"
    # breaks = c("higher", "equal", "lower")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of generous person") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/outputs/2b_violin_rec.pdf"),
       width = 6,
       height = 7.5)




# Stats -------------------------------------------------------------------

mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                                  Sum Sq Mean Sq NumDF  DenDF  F value Pr(>F)
# social_interaction                 7.14    3.57     2 3058.0   1.9195 0.1469
# relationship                    2065.54 1032.77     2 3057.9 555.2910 <2e-16 ***
# social_interaction:relationship 1612.49  403.12     4 3057.9 216.7470 <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Pairwise contrasts (main preregistered analysis)
emmeans(mod, pairwise ~ relationship | social_interaction) %>%
  summary(infer = T)

# $emmeans
# social_interaction = repeating:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# higher         4.07 0.0894 310     3.89     4.24  45.536  <.0001
# lower          3.69 0.0893 309     3.51     3.86  41.322  <.0001
# equal          3.53 0.0892 308     3.36     3.71  39.612  <.0001
#
# social_interaction = alternating:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# higher         2.55 0.0891 307     2.38     2.73  28.676  <.0001
# lower          2.57 0.0892 308     2.39     2.75  28.820  <.0001
# equal          5.82 0.0891 307     5.64     5.99  65.319  <.0001
#
# social_interaction = no_interaction:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# higher         3.02 0.0892 308     2.84     3.20  33.870  <.0001
# lower          2.93 0.0891 307     2.75     3.10  32.869  <.0001
# equal          5.22 0.0891 307     5.05     5.40  58.610  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# social_interaction = repeating:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# higher - lower   0.3802 0.104 3041   0.1369    0.624   3.664  0.0007
# higher - equal   0.5363 0.104 3041   0.2932    0.779   5.172  <.0001
# lower - equal    0.1561 0.104 3041  -0.0869    0.399   1.506  0.2881
#
# social_interaction = alternating:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# higher - lower  -0.0154 0.103 3041  -0.2580    0.227  -0.149  0.9878
# higher - equal  -3.2644 0.103 3041  -3.5068   -3.022 -31.574  <.0001
# lower - equal   -3.2490 0.103 3041  -3.4916   -3.006 -31.402  <.0001
#
# social_interaction = no_interaction:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# higher - lower   0.0922 0.103 3041  -0.1504    0.335   0.891  0.6461
# higher - equal  -2.2009 0.103 3041  -2.4435   -1.958 -21.273  <.0001
# lower - equal   -2.2931 0.103 3041  -2.5355   -2.051 -22.180  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

# relationship social_interaction emmean     SE  df lower.CL upper.CL
# more         precedent            4.07 0.0894 310     3.89     4.24
# less         precedent            3.69 0.0893 309     3.51     3.86
# equal        precedent            3.53 0.0892 308     3.36     3.71

# contrast                                   estimate    SE   df t.ratio p.value
# more precedent - less precedent              0.3802 0.104 3041   3.664  0.0077
# more precedent - equal precedent             0.5363 0.104 3041   5.172  <.0001
# less precedent - equal precedent             0.1561 0.104 3041   1.506  0.8530


# Interaction contrasts

# Group by asymmetric/symmetric
emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("asymmetric", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ asymmetric | social_interaction) %>%
  summary(infer = T)

# $emmeans
# social_interaction = repeating:
#   asymmetric emmean     SE  df lower.CL upper.CL t.ratio p.value
# no           3.53 0.0892 308     3.36     3.71  39.612  <.0001
# yes          3.88 0.0727 139     3.73     4.02  53.358  <.0001
#
# social_interaction = alternating:
#   asymmetric emmean     SE  df lower.CL upper.CL t.ratio p.value
# no           5.82 0.0891 307     5.64     5.99  65.319  <.0001
# yes          2.56 0.0726 139     2.42     2.71  35.303  <.0001
#
# social_interaction = no_interaction:
#   asymmetric emmean     SE  df lower.CL upper.CL t.ratio p.value
# no           5.22 0.0891 307     5.05     5.40  58.610  <.0001
# yes          2.97 0.0726 139     2.83     3.12  40.979  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# social_interaction = repeating:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes   -0.346 0.0897 3041   -0.522    -0.17  -3.858  0.0001
#
# social_interaction = alternating:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes    3.257 0.0896 3041    3.081     3.43  36.364  <.0001
#
# social_interaction = no_interaction:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes    2.247 0.0896 3041    2.071     2.42  25.090  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95

contrast(emmeans(emm, pairwise ~ asymmetric * social_interaction), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# asymmetric_pairwise social_interaction_pairwise  estimate    SE   df lower.CL upper.CL t.ratio p.value
# no - yes            repeating - alternating         -3.60 0.127 3041   -3.851    -3.35 -28.419  <.0001
# no - yes            repeating - no_interaction      -2.59 0.127 3041   -2.842    -2.34 -20.455  <.0001
# no - yes            alternating - no_interaction     1.01 0.127 3041    0.761     1.26   7.972  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95


# Repeat with normalized values -------------------------------------------


mod <- lmer(normalized_likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)


anova(mod, type = "III")

# Pairwise contrasts (main preregistered analysis)
emmeans(mod, pairwise ~ relationship | social_interaction) %>%
  summary(infer = T)

# Interaction contrasts

# Group by asymmetric/symmetric
emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("asymmetric", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ asymmetric | social_interaction) %>%
  summary(infer = T)


contrast(emmeans(emm, pairwise ~ asymmetric * social_interaction), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)


# Extra plots -------------------------------------------------------------

# Aggregated results
f = ggplot(data = d,
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = relationship.colors,
    name = "power/status of generous person"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of generous person") +
  theme(legend.position = "bottom")

f
