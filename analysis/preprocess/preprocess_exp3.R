library(here)
library(tidyverse)
library(digest)

d <- read_csv(here('data/exp3/non_anonymized/exp3_data.csv'))
d.dem <- read_csv(here('data/exp3/non_anonymized/exp3_demographics.csv'))

# Anonymize participants
d$subject_id <- sapply(d$subject_id, digest)
d.dem$subject_id <- sapply(d.dem$subject_id, digest)

d %>%
  mutate(
    temp_high = ifelse(story == "conversation" & !is.na(more), less, more),
    temp_low = ifelse(story == "conversation" & !is.na(less), more, less)
  ) %>%
  select(-more, -less) %>%
  rename(
    more = temp_high,
    less = temp_low
  ) %>%
  write_csv(here('data/exp3_data.csv'))

d.dem %>%
  write_csv(here('data/exp3_demographics.csv'))