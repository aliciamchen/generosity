library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(afex)
library(forcats)
library(emmeans)


# Options -----------------------------------------------------------------

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
theme_set(theme_classic(base_size = 30))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

relationship.colors <- c("#7F9CFF", "#00A08A", "#D6D6D6")


# Load data ---------------------------------------------------------------

d <-
  read.csv(here('data/2a_data.csv')) %>%
  filter(pass_attention == T, understood == "yes") %>%
  rename("none" = "no_relationship") %>%
  mutate(
    social_interaction = case_when(
      social_interaction == "precedent" ~ "repeating",
      social_interaction == "reciprocity" ~ "alternating",
      .default = social_interaction
    )
  ) %>%
  pivot_longer(
    cols = c("symmetric", "asymmetric", "none"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    relationship = fct_relevel(relationship, "asymmetric", "symmetric", "none"),
    social_interaction = fct_relevel(
      social_interaction,
      "repeating",
      "alternating",
      "no_interaction"
    )
  ) %>%
  group_by(subject_id, story, social_interaction) %>%
  mutate(
    total_rating = sum(likert_rating, na.rm = T),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating)


write.csv(d, here('data/2a_tidy_data.csv'), row.names = FALSE)


# Demographics ------------------------------------------------------------

d.demographics <- read.csv(here('data/2a_demographics.csv')) 

print(length(unique(d.demographics$subject_id)))

d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))


# Plots -------------------------------------------------------------------


d.means.all <-
  d %>% drop_na() %>%
  group_by(social_interaction, relationship) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


# Individual plots


f = ggplot(
  data = d %>% filter(social_interaction == "no_interaction"),
  aes(x = social_interaction, y = likert_rating, fill = relationship)
) +
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
    width = 0.06
  ) +
  scale_fill_manual(values = relationship.colors, name = "relationship") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/2a_violin_control.pdf"),
  width = 6,
  height = 7.5
)


f = ggplot(
  data = d %>% filter(social_interaction == "repeating"),
  aes(x = social_interaction, y = likert_rating, fill = relationship)
) +
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
    width = 0.06
  ) +
  scale_fill_manual(values = relationship.colors, name = "relationship") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/2a_violin_control_prec.pdf"),
  width = 6,
  height = 7.5
)



f = ggplot(
  data = d %>% filter(social_interaction == "alternating"),
  aes(x = social_interaction, y = likert_rating, fill = relationship)
) +
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
    width = 0.06
  ) +
  scale_fill_manual(values = relationship.colors, name = "relationship") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/2a_violin_control_rec.pdf"),
  width = 6,
  height = 7.5
)



# Stats -------------------------------------------------------------------

# Set levels
d$social_interaction <-
  factor(d$social_interaction,
         levels = c("repeating", "alternating", "no_interaction"))
d$relationship <-
  factor(d$relationship, levels = c("asymmetric", "symmetric", "none"))

# With all levels
mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 |
                                                                                   subject_id),
            data = d)

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                                 Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# social_interaction               121.4   60.69     2 2915.8  29.146  2.93e-13 ***
# relationship                    1078.3  539.13     2 2916.6 258.934 < 2.2e-16 ***
# social_interaction:relationship 3824.6  956.16     4 2919.1 459.224 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Pairwise contrasts
emmeans(mod, revpairwise ~ relationship | social_interaction) %>%
  summary(infer = T)


# $emmeans
# social_interaction = repeating:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# asymmetric     5.21 0.0917 376     5.03     5.39  56.820  <.0001
# symmetric      3.23 0.0920 380     3.05     3.41  35.126  <.0001
# none           2.45 0.0923 383     2.27     2.63  26.569  <.0001
#
# social_interaction = alternating:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# asymmetric     2.26 0.0922 381     2.08     2.44  24.548  <.0001
# symmetric      6.03 0.0916 374     5.85     6.21  65.833  <.0001
# none           2.22 0.0922 381     2.04     2.40  24.088  <.0001
#
# social_interaction = no_interaction:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# asymmetric     3.11 0.0924 385     2.92     3.29  33.609  <.0001
# symmetric      4.28 0.0920 380     4.10     4.46  46.548  <.0001
# none           4.55 0.0917 376     4.37     4.73  49.685  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# social_interaction = repeating:
#   contrast               estimate    SE   df lower.CL upper.CL t.ratio p.value
# symmetric - asymmetric  -1.9753 0.112 2904 -2.23784   -1.713 -17.644  <.0001
# none - asymmetric       -2.7565 0.112 2905 -3.01944   -2.493 -24.579  <.0001
# none - symmetric        -0.7811 0.112 2901 -1.04456   -0.518  -6.953  <.0001
#
# social_interaction = alternating:
#   contrast               estimate    SE   df lower.CL upper.CL t.ratio p.value
# symmetric - asymmetric   3.7649 0.112 2907  3.50222    4.028  33.613  <.0001
# none - asymmetric       -0.0424 0.112 2901 -0.30584    0.221  -0.378  0.9244
# none - symmetric        -3.8073 0.112 2907 -4.06993   -3.545 -33.992  <.0001
#
# social_interaction = no_interaction:
#   contrast               estimate    SE   df lower.CL upper.CL t.ratio p.value
# symmetric - asymmetric   1.1780 0.112 2902  0.91431    1.442  10.477  <.0001
# none - asymmetric        1.4496 0.112 2904  1.18647    1.713  12.918  <.0001
# none - symmetric         0.2716 0.112 2902  0.00918    0.534   2.427  0.0405
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast(
  emmeans(mod, ~ relationship * social_interaction),
  interaction = c("pairwise", "pairwise")
) %>%
  summary(infer = T)

# relationship_pairwise  social_interaction_pairwise  estimate    SE   df lower.CL upper.CL t.ratio p.value
# asymmetric - symmetric repeating - alternating          5.74 0.158 2909    5.430     6.05  36.228  <.0001
# asymmetric - none      repeating - alternating          2.71 0.159 2903    2.403     3.03  17.098  <.0001
# symmetric - none       repeating - alternating         -3.03 0.159 2903   -3.337    -2.72 -19.079  <.0001
# asymmetric - symmetric repeating - no_interaction       3.15 0.159 2904    2.842     3.46  19.870  <.0001
# asymmetric - none      repeating - no_interaction       4.21 0.159 2908    3.895     4.52  26.501  <.0001
# symmetric - none       repeating - no_interaction       1.05 0.159 2902    0.742     1.36   6.639  <.0001
# asymmetric - symmetric alternating - no_interaction    -2.59 0.159 2903   -2.898    -2.28 -16.305  <.0001
# asymmetric - none      alternating - no_interaction     1.49 0.159 2903    1.181     1.80   9.397  <.0001
# symmetric - none       alternating - no_interaction     4.08 0.158 2906    3.768     4.39  25.754  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95



# Repeat analyses without all levels --------------------------------------

d_filtered <- d %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none")

mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 |
                                                                               subject_id),
            data = d_filtered)

summary(mod)

# Pairwise contrasts
emmeans(mod, revpairwise ~ relationship | social_interaction) %>%
  summary(infer = T)

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast(
  emmeans(mod, ~ relationship * social_interaction),
  interaction = c("pairwise", "pairwise")
) %>%
  summary(infer = T)



# Repeat with normalized values -------------------------------------------


# With all levels
mod <- lmer(
  normalized_likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                        story) + (1 |
                                                                                    subject_id),
  data = d
)

summary(mod)

anova(mod, type = "III")

# Pairwise contrasts
emmeans(mod, revpairwise ~ relationship | social_interaction) %>%
  summary(infer = T)

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast(
  emmeans(mod, ~ relationship * social_interaction),
  interaction = c("pairwise", "pairwise")
) %>%
  summary(infer = T)


# Without all levels
mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 |
                                                                               subject_id),
            data = d_filtered)

summary(mod)


# Extra plots -------------------------------------------------------------


# Aggregated results on one plot

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
    name = "relationship",
    breaks = c("asymmetric", "symmetric", "none")
  ) +
  scale_x_discrete(
    limits = c("repeating", "alternating", "no_interaction"),
    labels = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")

f
