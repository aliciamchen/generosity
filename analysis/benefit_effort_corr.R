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

theme_set(theme_few(base_size = 15))

d.1a <- read.csv(here('data/exp1_tidy_data.csv'))
d.1b <- read.csv(here('data/exp3_tidy_data.csv'))
d.2a <- read.csv(here('data/exp2_tidy_data.csv'))
d.2b <- read.csv(here('data/exp4_tidy_data.csv'))

validation.benefit <- read.csv(here('data/validation_benefit_diff.csv'))
validation.effort <- read.csv(here('data/validation_effort_diff.csv'))

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

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

# Plots

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
