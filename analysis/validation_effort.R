library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(afex)
library(brms)
library(forcats)
library(emmeans)
library(wesanderson)

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

theme_set(theme_few(base_size = 15))

## Plot

d <-
  read.csv(here('data/validation_effort_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  pivot_longer(
    cols = c("expected_high_benefit", "expected_low_benefit"),
    names_to = "benefit",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention"))

# write.csv(d, here('data/validation_effort_tidy_data.csv'), row.names=FALSE)

# d.demographics <- read.csv(here('data/validation_effort_demographics.csv'))

# d.demographics %>% count(gender)
# d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))

d.means.all <-
  d %>% drop_na() %>%
  group_by(benefit) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


f = ggplot(data = d,
           aes(x = benefit, y = likert_rating)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = benefit, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = benefit, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "benefit", y = "how much effort?") +
  theme(legend.position = "bottom")

f

d.means.all <-
  d %>% drop_na() %>%
  group_by(story, benefit) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)

f = ggplot(data = d,
           aes(x = benefit, y = likert_rating)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = benefit, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = benefit, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  scale_x_discrete(labels = c("high", "low")) +
  labs(x = "expected benefit", y = "perceived effort", title = 
         "how much effort?") +
  theme(legend.position = "bottom") + 
  facet_wrap(~story)


f

# Calculate mean differences and save
# calculate difference in means
diff <- d %>%
  group_by(story, benefit) %>%
  summarise(mean_rating = mean(likert_rating, na.rm = TRUE)) %>%
  spread(benefit, mean_rating) %>%
  mutate(diff = expected_low_benefit - expected_high_benefit)

diff
write.csv(diff, here("data/validation_effort_diff.csv"), row.names = FALSE)
## Analysis


# Without all levels

mod <- lmer(likert_rating ~ benefit + 
                                                               (1 | subject_id),
            data = d %>% filter(story == "meeting prep"))

summary(mod)

emm <- mod %>% emmeans(pairwise ~ benefit)
emm

# With all levels
mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("interaction_present",
               "social_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


##################################################

## Repeat all analyses with normalized values

d <-
  read.csv(here('data/exp1_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  mutate(newSum = select_if(., is.numeric) %>%
           reduce(`+`)) %>%
  mutate_if(is.numeric, list( ~ . / newSum)) %>%
  select(-newSum) %>%
  rename("none" = "no_relationship") %>%
  pivot_longer(
    cols = c("asymmetric", "symmetric", "none"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    relationship = fct_relevel(relationship,
                               "symmetric", "asymmetric", "none"),
    social_interaction = fct_relevel(
      social_interaction,
      "precedent",
      "reciprocity",
      "no_interaction"
    )
  )

d.means.all <-
  d %>% drop_na() %>%
  group_by(social_interaction, relationship) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)

d_filtered <- d %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none")

mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 | subject_id),
            data = d_filtered)

summary(mod)


mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("interaction_present",
               "social_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)
