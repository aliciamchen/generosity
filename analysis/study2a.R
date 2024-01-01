library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(afex)
library(forcats)
library(emmeans)
library(wesanderson)

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
theme_set(theme_classic(base_size = 30))

## Plot

d <-
  read.csv(here('data/2a_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  rename("none" = "no_relationship") %>%
  pivot_longer(
    cols = c("symmetric", "asymmetric", "none"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    relationship = fct_relevel(relationship,
                               "asymmetric", "symmetric", "none"),
    social_interaction = fct_relevel(
      social_interaction,
      "precedent",
      "reciprocity",
      "no_interaction"
    )
  ) %>% 
  group_by(subject_id, story, social_interaction) %>%
  mutate(total_rating = sum(likert_rating),
         normalized_likert_rating = likert_rating / total_rating) %>%
  select(-total_rating) 


write.csv(d, here('data/2a_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/2a_demographics.csv')) %>% filter(pass_attention == T, understood == "yes")

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

print(length(unique(d$subject_id)))


################## PLOTS


d.means.all <-
  d %>% drop_na() %>%
  group_by(social_interaction, relationship) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)



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
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship",
    breaks = c("symmetric", "asymmetric", "none")
  ) +
  scale_x_discrete(
    limits = c("precedent", "reciprocity", "no_interaction"),
    labels = c("precedent", "reciprocity", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")

f


# Individual plots 
f = ggplot(data = d %>% filter(social_interaction == "precedent"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "precedent"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "precedent"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/outputs/2a_violin_control_prec.pdf"),
       width = 6,
       height = 7.5)



f = ggplot(data = d %>% filter(social_interaction == "reciprocity"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/outputs/2a_violin_control_rec.pdf"),
       width = 6,
       height = 7.5)

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
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/outputs/2a_violin_control.pdf"),
       width = 6,
       height = 7.5)



#################### STATS

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# Set levels
d$social_interaction <-
  factor(d$social_interaction, levels = c("precedent", "reciprocity", "no_interaction"))
d$relationship <-
  factor(d$relationship, levels = c("asymmetric", "symmetric", "none"))

# With all levels
mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

# Pairwise contrasts
emm <- emmeans(mod, pairwise ~ relationship * social_interaction)
emm

# relationship social_interaction emmean     SE  df lower.CL upper.CL
# asymmetric   no_interaction       3.11 0.0924 385     2.92     3.29
# symmetric    no_interaction       4.28 0.0920 380     4.10     4.46

# contrast                                             estimate    SE   df t.ratio p.value
# asymmetric no_interaction - symmetric no_interaction  -1.1780 0.112 2902 -10.477  <.0001



# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# relationship_pairwise  social_interaction_pairwise  estimate    SE   df t.ratio p.value
# asymmetric - symmetric precedent - no_interaction       3.15 0.159 2904  19.870  <.0001
# asymmetric - symmetric reciprocity - no_interaction    -2.59 0.159 2903 -16.305  <.0001

# asymmetric - symmetric precedent - reciprocity          5.74 0.158 2909  36.228  <.0001

emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("interaction_present",
               "social_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


# Without all levels
d_filtered <- d %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none")

mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 | subject_id),
            data = d_filtered)

summary(mod)


##################################################

## Repeat all analyses with normalized values

# With all levels
mod <- lmer(normalized_likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

# Pairwise contrasts
emm <- emmeans(mod, pairwise ~ relationship * social_interaction)
emm




# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test


emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("interaction_present",
               "social_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


# Without all levels
d_filtered <- d %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none")

mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 | subject_id),
            data = d_filtered)

summary(mod)


