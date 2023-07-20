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

# Create function to fit model and extract interaction coefficient
get_interaction_coefficient <- function(df) {
  model <- lmer(likert_rating ~ social_interaction * relationship + (1 | subject_id), data = df)
  tidy_model <- broom.mixed::tidy(model)
  interaction_coefficient <- tidy_model %>% 
    filter(str_detect(term, "social_interaction1:relationship1")) %>% 
    pull(estimate)
  data.frame(interaction_coefficient = interaction_coefficient)
}

# Apply function to each story

# Study 1
d.1a.interactions <- d.1a %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none") %>% 
  group_by(story) %>%
  do(get_interaction_coefficient(.))

d.1a.interactions

d.1b.interactions <- d.1b %>%
  filter(social_interaction != "no_interaction" &
           relationship != "equal") %>% 
  group_by(story) %>%
  do(get_interaction_coefficient(.))

d.1b.interactions

# Study 2
d.2a <- d.2a %>% rename(social_interaction = next_interaction)
d.2b <- d.2b %>% rename(social_interaction = next_interaction) # renaming kind of jank so i can just have one function but whatever


d.2a.interactions <- d.2a %>%
  filter(social_interaction != "none" &
           relationship != "no_info") %>% 
  group_by(story) %>%
  do(get_interaction_coefficient(.))

d.2a.interactions

d.2b.interactions <- d.2b %>%
  filter(social_interaction != "no_interaction" &
           relationship != "equal") %>% 
  group_by(story) %>%
  do(get_interaction_coefficient(.))

d.2b.interactions

##### make plots

d.1a.benefit <- inner_join(validation.benefit, d.1a.interactions, by = "story")

ggplot(d.1a.benefit, aes(x = diff, y = interaction_coefficient)) +
  geom_point() +
  labs(title = "study 1a", x = "benefit difference")
  
d.1a.cost <- inner_join(validation.effort, d.1a.interactions, by = "story")

ggplot(d.1a.cost, aes(x = diff, y = interaction_coefficient)) +
  geom_point() +
  labs(title = "study 1a", x = "effort difference")

# study 1b

d.1b.benefit <- inner_join(validation.benefit, d.1b.interactions, by = "story")

ggplot(d.1b.benefit, aes(x = diff, y = interaction_coefficient)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "study 1b", x = "benefit difference")

d.1b.cost <- inner_join(validation.effort, d.1b.interactions, by = "story")

ggplot(d.1b.cost, aes(x = diff, y = interaction_coefficient)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "study 1b", x = "effort difference")

# study 2b

d.2b.benefit <- inner_join(validation.benefit, d.2b.interactions, by = "story")

ggplot(d.2b.benefit, aes(x = diff, y = interaction_coefficient)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "study 2b", x = "benefit difference")

d.2b.cost <- inner_join(validation.effort, d.2b.interactions, by = "story")

ggplot(d.2b.cost, aes(x = diff, y = interaction_coefficient)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "study 2b", x = "effort difference")
