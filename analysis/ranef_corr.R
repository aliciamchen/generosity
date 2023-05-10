library(here)
library(tidyverse)
library(ggthemes)
library(lme4)
library(lmerTest)

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
theme_set(theme_few(base_size = 30))

d.exp1 <- read.csv(here("data/exp1_tidy_data.csv"))
d.exp2 <- read.csv(here("data/exp2_tidy_data.csv"))
d.exp3 <- read.csv(here("data/exp3_tidy_data.csv"))
d.exp4 <- read.csv(here("data/exp4_tidy_data.csv"))

# exp1
mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                 story) + (1 | subject_id),
            data = d.exp1 %>%
              filter(social_interaction != "no_interaction" &
                        relationship != "none"))

summary(mod)
ranef(mod)$story

# exp2
mod <- lmer(likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 | subject_id),
            data = d.exp2 %>%
  filter(next_interaction != "none" & relationship != "no_info"))

summary(mod)
ranef(mod)$story

# exp3
mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d.exp3)

summary(mod)
ranef(mod)$story

# exp4
mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                   story) + (1 | subject_id),
            data = d.exp3)

summary(mod)
ranef(mod)$story