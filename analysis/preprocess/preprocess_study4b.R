library(here)
library(tidyverse)
library(digest)

d <- read_csv(here('data/study4b/non_anonymized/study4b_data.csv'))
d.dem <- read_csv(here('data/study4b/non_anonymized/study4b_demographics.csv'))

# Anonymize participants
d$subject_id <- sapply(d$subject_id, digest)
d.dem$subject_id <- sapply(d.dem$subject_id, digest)

d %>%
  write_csv(here('data/study4b_data.csv'))

d.dem %>%
  write_csv(here('data/study4b_demographics.csv'))
