library(here)
library(tidyverse)
library(digest)

d <- read_csv(here('data/validation_effort/non_anonymized/validation_effort_data.csv'))
d.dem <- read_csv(here('data/validation_effort/non_anonymized/validation_effort_demographics.csv'))

# Anonymize participants
d$subject_id <- sapply(d$subject_id, digest)
d.dem$subject_id <- sapply(d.dem$subject_id, digest)

# Switch coding of expected benefit for conversation
d %>%
  mutate(
    temp_high = ifelse(story == "conversation" & !is.na(expected_high_benefit), expected_low_benefit, expected_high_benefit),
    temp_low = ifelse(story == "conversation" & !is.na(expected_low_benefit), expected_high_benefit, expected_low_benefit)
  ) %>%
  select(-expected_high_benefit, -expected_low_benefit) %>%
  rename(
    expected_high_benefit = temp_high,
    expected_low_benefit = temp_low
  ) %>%
  write_csv(here('data/validation_effort_data.csv'))

d.dem %>%
  write_csv(here('data/validation_effort_demographics.csv'))