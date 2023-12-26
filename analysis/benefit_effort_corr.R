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
library(broom.mixed)

theme_set(theme_classic(base_size = 20))

d.1a <- read.csv(here('data/exp1_tidy_data.csv'))
d.1b <- read.csv(here('data/exp3_tidy_data.csv'))
d.2a <- read.csv(here('data/exp2_tidy_data.csv'))
d.2b <- read.csv(here('data/exp4_tidy_data.csv'))

d.4d <- read.csv(here('data/study4d_data.csv'))

validation.benefit <- read.csv(here('data/validation_benefit_diff.csv'))
validation.effort <- read.csv(here('data/validation_effort_diff.csv'))

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

# Study 4d


d.4d <- d.4d %>% 
  left_join(validation.benefit %>% select(story, diff), by = "story") %>% 
  rename(benefit_diff = diff) %>% 
  left_join(validation.effort %>% select(story, diff), by = "story") %>% 
  rename(effort_diff = diff) %>% 
  filter(strategy == "repeating" & altruistic_status_second != "less" & altruistic_status_second != "just_met")

ggplot(d.4d, aes(x = benefit_diff, y = response, color = factor(altruistic_status_second))) + 
  geom_jitter(alpha=0.8) + 
  labs(x = "benefit difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

ggplot(d.1b, aes(x = effort_diff, y = response, color = factor(altruistic_status_second))) + 
  geom_jitter(alpha=0.8) + 
  labs(x = "effort difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 


#### Todo: EDIT
d.4d.benefit.means <- d.4d %>% 
  group_by(story, relationship, benefit_diff) %>% 
  tidyboot_mean(likert_rating, na.rm = T)

f <- ggplot(d.1b.benefit.means, aes(x = benefit_diff, y = empirical_stat, color = factor(relationship))) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  labs(x = "benefit difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

f

d.1b.effort.means <- d.1b %>% 
  group_by(story, relationship, effort_diff) %>% 
  tidyboot_mean(likert_rating, na.rm = T)

f <- ggplot(d.1b.effort.means, aes(x = effort_diff, y = empirical_stat, color = factor(relationship))) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  labs(x = "effort difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

f

# Study 1b

d.1b <- d.1b %>% 
  left_join(validation.benefit %>% select(story, diff), by = "story") %>% 
  rename(benefit_diff = diff) %>% 
  left_join(validation.effort %>% select(story, diff), by = "story") %>% 
  rename(effort_diff = diff) %>% 
  filter(social_interaction == "precedent" & relationship != "equal")


mod <- lmer(likert_rating ~ 1 +  relationship * benefit_diff + (1 | story) + (1 | subject_id),
            data = d.1b)

summary(mod)

mod <- lmer(likert_rating ~ 1 +  relationship * effort_diff + (1 | story) + (1 | subject_id),
            data = d.1b)

summary(mod)

# Study 2b

d.2b <- d.2b %>% 
  left_join(validation.benefit %>% select(story, diff), by = "story") %>% 
  rename(benefit_diff = diff) %>% 
  left_join(validation.effort %>% select(story, diff), by = "story") %>% 
  rename(effort_diff = diff) %>% 
  filter(next_interaction == "repeating" & relationship != "equal")

mod <- lmer(likert_rating ~ 1 +  relationship * benefit_diff + (1 | story) + (1 | subject_id),
            data = d.2b)

summary(mod)

mod <- lmer(likert_rating ~ 1 +  relationship * effort_diff + (1 | story) + (1 | subject_id),
            data = d.2b)

summary(mod)

# Plots NOT USEFUL

ggplot(d.1b, aes(x = benefit_diff, y = likert_rating, color = factor(relationship))) + 
  geom_jitter(alpha=0.8) + 
  labs(x = "benefit difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

ggplot(d.1b, aes(x = effort_diff, y = likert_rating, color = factor(relationship))) + 
  geom_jitter(alpha=0.8) + 
  labs(x = "effort difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

ggplot(d.2b, aes(x = benefit_diff, y = likert_rating, color = factor(relationship))) + 
  geom_jitter(alpha=0.8) + 
  labs(x = "benefit difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 2b (relationship -> behavior) precedent + asymmetric") 

ggplot(d.2b, aes(x = effort_diff, y = likert_rating, color = factor(relationship))) + 
  geom_jitter(alpha=0.8) + 
  labs(x = "effort difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 2b (relationship -> behavior) precedent + asymmetric") 

# Grouped

#1b
d.1b.benefit.means <- d.1b %>% 
  group_by(story, relationship, benefit_diff) %>% 
  tidyboot_mean(likert_rating, na.rm = T)

f <- ggplot(d.1b.benefit.means, aes(x = benefit_diff, y = empirical_stat, color = factor(relationship))) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  labs(x = "benefit difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

f

d.1b.effort.means <- d.1b %>% 
  group_by(story, relationship, effort_diff) %>% 
  tidyboot_mean(likert_rating, na.rm = T)

f <- ggplot(d.1b.effort.means, aes(x = effort_diff, y = empirical_stat, color = factor(relationship))) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  labs(x = "effort difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 1b (behavior -> relationship) precedent + asymmetric") 

f


#2b

d.2b.benefit.means <- d.2b %>% 
  group_by(story, relationship, benefit_diff) %>% 
  tidyboot_mean(likert_rating, na.rm = T)

f <- ggplot(d.2b.benefit.means, aes(x = benefit_diff, y = empirical_stat, color = factor(relationship))) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  labs(x = "benefit difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 2b (relationship -> behavior) precedent + asymmetric") 

f

d.2b.effort.means <- d.2b %>% 
  group_by(story, relationship, effort_diff) %>% 
  tidyboot_mean(likert_rating, na.rm = T)

f <- ggplot(d.2b.effort.means, aes(x = effort_diff, y = empirical_stat, color = factor(relationship))) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  labs(x = "effort difference in scenario", 
       y = "likert", 
       color = "status of altruistic person",
       title = "study 2b (relationship -> behavior) precedent + asymmetric") 

f

# The 4 pltos rebecca asked for for the coglunch talk

d.2b.coglunch <- read.csv(here('data/exp4_tidy_data.csv')) %>% 
  left_join(validation.benefit %>% select(story, diff), by = "story") %>% 
  rename(benefit_diff = diff) %>% 
  left_join(validation.effort %>% select(story, diff), by = "story") %>% 
  rename(effort_diff = diff) %>% 
  filter(relationship != "equal", next_interaction != "none")

d.2b.coglunch <- d.2b.coglunch %>% 
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(rep_minus_alt = repeating - alternating) %>%
  ungroup()

d.2b.coglunch.means <- d.2b.coglunch %>% 
  # filter(!is.na(rep_minus_alt)) %>% 
  group_by(story, relationship, benefit_diff, effort_diff) %>% 
  tidyboot_mean(rep_minus_alt, na.rm = TRUE)


## benefit and effort
f <- ggplot(d.2b.coglunch.means %>% filter(relationship == "less"), aes(x = benefit_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative benefit to recipient", 
       y = "repeated minus alternated",
       title = "study 1b, A is lower status") 

f

ggsave(here("figures/coglunch/1b_benefit_lower.pdf"),
       width = 8.7,
       height = 4)

f <- ggplot(d.2b.coglunch.means %>% filter(relationship == "more"), aes(x = benefit_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative benefit to recipient", 
       y = "repeated minus alternated",
       title = "study 1b, A is higher status") 

f

ggsave(here("figures/coglunch/1b_benefit_higher.pdf"),
       width = 8.7,
       height = 4)


f <- ggplot(d.2b.coglunch.means %>% filter(relationship == "less"), aes(x = effort_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative effort", 
       y = "repeated minus alternated",
       title = "study 1b, A is lower status") 

f

ggsave(here("figures/coglunch/1b_effort_lower.pdf"),
       width = 8.7,
       height = 4)


f <- ggplot(d.2b.coglunch.means %>% filter(relationship == "more"), aes(x = effort_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative effort", 
       y = "repeated minus alternated",
       title = "study 1b, A is higher status") 

f

ggsave(here("figures/coglunch/1b_effort_higher.pdf"),
       width = 8.7,
       height = 4)


### For cogsci poster 

# Calculate relative probability of more
d.2b.prob.benefit <- d.2b.benefit.means %>% 
  pivot_wider(names_from = relationship, values_from = empirical_stat, id_cols = c(story, benefit_diff)) %>% 
  mutate(prob_more = more - less)

f <- ggplot(d.2b.prob.benefit, aes(x = benefit_diff, y = prob_more)) +
  geom_point(size = 5.3, alpha = 0.9, color = "#F8766D") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(limits = c(0, 4))
  

f

ggsave(here("figures/cogsci_poster/2b_exploratory_benefit.pdf"),
       width = 8.7,
       height = 4)

d.2b.prob.effort <- d.2b.effort.means %>% 
  pivot_wider(names_from = relationship, values_from = empirical_stat, id_cols = c(story, effort_diff)) %>% 
  mutate(prob_more = more - less)

f <- ggplot(d.2b.prob.effort, aes(x = effort_diff, y = prob_more)) +
  geom_point(size = 5.3, alpha = 0.9, color = "#00BFC4") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 4))

f

ggsave(here("figures/cogsci_poster/2b_exploratory_effort.pdf"),
       width = 8.7,
       height = 4)

