library(here)
library(tidyverse)

d.1a <-
  read.csv(here('data/1a_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("repeating", "alternating", "none"),
    names_to = "next_interaction",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    next_interaction = fct_relevel(next_interaction,
                                   "repeating", "alternating", "none"),
    relationship = fct_relevel(relationship,
                               "asymmetric", "symmetric", "no_info")
  )

write.csv(d.1a, here('data/tidy/1a_tidy_data.csv'), row.names=FALSE)