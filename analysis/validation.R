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

theme_set(theme_classic(base_size = 9))

## Plot

d <-
  read.csv(here('data/validation_benefit_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  pivot_longer(
    cols = c("expected_high_benefit", "expected_low_benefit"),
    names_to = "benefit",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention"))

write.csv(d, here('data/validation_benefit_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/validation_benefit_demographics.csv'))

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

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
  labs(x = "benefit", y = "how likely?") +
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
  labs(x = "expected benefit", y = "perceived benefit") +
  theme(legend.position = "bottom") + 
  facet_wrap(~story)


f

### 

diff <- d %>%
  group_by(story, benefit) %>%
  summarise(mean_rating = mean(likert_rating, na.rm = TRUE)) %>%
  spread(benefit, mean_rating) %>%
  mutate(diff = expected_high_benefit - expected_low_benefit, 
         diff = abs(diff))

diff

write.csv(diff, here("data/validation_benefit_diff.csv"), row.names = FALSE)


## Pretty plot
all.diffs.benefit <- d %>% 
  group_by(subject_id, story) %>% 
  spread(benefit, likert_rating) %>% 
  mutate(diff = expected_high_benefit - expected_low_benefit) %>% 
  mutate(type = "benefit")

all.diffs.effort <- read.csv(here('data/validation_effort_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  pivot_longer(
    cols = c("expected_high_benefit", "expected_low_benefit"),
    names_to = "benefit",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>% 
  group_by(subject_id, story) %>% 
  spread(benefit, likert_rating) %>% 
  mutate(diff = expected_high_benefit - expected_low_benefit, 
         type = "effort")

all.diffs.benefit.means <- all.diffs.benefit %>% 
  group_by(story) %>% 
  tidyboot_mean(diff, na.rm = TRUE) %>% 
  mutate(type = "benefit")

all.diffs.effort.means <- all.diffs.effort %>% 
  group_by(story) %>% 
  tidyboot_mean(diff, na.rm = TRUE) %>% 
  mutate(type = "effort")


# df.stacked.means <- bind_rows(all.diffs.benefit.means, all.diffs.effort.means)
df.stacked.all <- bind_rows(all.diffs.benefit, all.diffs.effort) 

df.stacked.means <- df.stacked.all %>% 
  group_by(story, type) %>% 
  tidyboot_mean(diff, na.rm = TRUE) %>% 
  rename(diff = empirical_stat)

# arrange descending
df.temp <- df.stacked.means %>% 
  filter(type == "benefit") %>% 
  arrange(desc(diff)) 

levs <- unique(df.temp$story)

df.stacked.all$story <- factor(df.stacked.all$story, levels=levs)

f <- ggplot(df.stacked.all, aes(x = story, y = diff, fill = type)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4)) +
  geom_point(
    data = df.stacked.means,
    aes(x = story, y = diff),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = df.stacked.means,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  labs(x = "story", y = "A minus B", title = 'relative cost/benefit for all scenarios') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
f

# ggsave(here("figures/validation_.pdf"), width = 10, height = 3)

  

# Figure out what 2 stories to exclude for study 4
# using abs of sum of benefit and effort diff
smallest_diffs <- df.stacked.means %>% 
  pivot_wider(names_from = type, values_from = diff, id_cols = story) %>% 
  mutate(sum_abs_diff = abs(benefit) + abs(effort)) %>% 
  arrange(sum_abs_diff) 


print(smallest_diffs)
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
