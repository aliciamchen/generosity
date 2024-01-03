library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(glue)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

d <-
  read.csv(here('data/3a_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  rename(likert_rating = response) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    altruistic_status_second = case_when(
      altruistic_status_second == "more" ~ "higher",
      altruistic_status_second == "less" ~ "lower",
      TRUE ~ altruistic_status_second
    )
  ) %>%
  mutate(
    strategy = fct_relevel(strategy,
                           "repeating", "alternating"),
    altruistic_status_second = fct_relevel(
      altruistic_status_second,
      "higher",
      "lower",
      "equal",
      "just_met"
    )
  )


write.csv(d, here('data/3a_tidy_data.csv'), row.names = FALSE)

d.demographics <- read.csv(here('data/3a_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>%
  summarize(
    mean_age = mean(age, na.rm = T),
    sd_age = sd(age),
    min_age = min(age),
    max_age = max(age)
  )

print(length(unique(d$subject_id)))

###### PLOTS

d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, altruistic_status_second) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)

f = ggplot(data = d,
           aes(x = strategy, y = likert_rating, fill = altruistic_status_second)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = strategy, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "strategy", y = "how moral?") +
  theme(legend.position = "bottom")

f

# grouped by story

d.means.all <-
  d %>% drop_na() %>%
  group_by(story, strategy, altruistic_status_second) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


f = ggplot(data = d,
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of second time altruistic person", y = "how moral?") +
  theme(legend.position = "bottom") +
  facet_wrap( ~ story)

f


# study 3a for presentation

strategies = c("repeating", "alternating")

for (strat in strategies) {
  f = ggplot(
    data = d %>% filter(strategy == strat),
    aes(x = strategy, y = likert_rating, fill = altruistic_status_second)
  ) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(strategy == strat),
      mapping = aes(x = strategy, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(strategy == strat),
      mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(values = wes_palette(n = 4, name = "Darjeeling1"),
                      name = "social relationship") +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "strategy", y = "how moral", fill = "social relationship") +
    theme(legend.position = "bottom")
  
  f
  
  ggsave(here(glue("figures/outputs/3a_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}

############ STATS


# Main preregistered analysis: without "just met" condition 

mod <-
  lmer(
    data = d %>% filter(altruistic_status_second != "just_met"),
    likert_rating ~ strategy * altruistic_status_second + (1 |
                                                             subject_id) + (1 | story)
  )

summary(mod)


# Pairwise contrasts 
emm <- emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm

# altruistic_status_second strategy    emmean     SE  df lower.CL upper.CL
# higher                   repeating     4.21 0.0760 111     4.06     4.36
# lower                    repeating     3.51 0.0760 111     3.36     3.66
# equal                    repeating     3.69 0.0761 111     3.54     3.84
# higher                   alternating   5.34 0.0760 111     5.19     5.49
# lower                    alternating   5.08 0.0761 111     4.93     5.23
# equal                    alternating   5.59 0.0760 111     5.44     5.74

# contrast                               estimate     SE   df t.ratio p.value
# higher repeating - lower repeating        0.697 0.0848 1880   8.213  <.0001
# higher alternating - lower alternating    0.260 0.0849 1880   3.059  0.0273

# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# altruistic_status_second_pairwise strategy_pairwise       estimate   SE   df t.ratio p.value
# higher - lower                    repeating - alternating    0.437 0.12 1881   3.640  0.0003
 

# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm

# asymmetric strategy    emmean     SE    df lower.CL upper.CL
# no         repeating     3.69 0.0761 111.0     3.54     3.84
# yes        repeating     3.86 0.0631  53.0     3.74     3.99
# no         alternating   5.59 0.0760 111.0     5.44     5.74
# yes        alternating   5.21 0.0631  53.1     5.08     5.34

# contrast                         estimate     SE   df t.ratio p.value
# no repeating - yes repeating       -0.170 0.0735 1881  -2.310  0.0960
# no alternating - yes alternating    0.382 0.0735 1880   5.190  <.0001

contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test


# Repeat analyses with `just met` condition
mod <-
  lmer(data = d,
       likert_rating ~ strategy * altruistic_status_second + (1 |
                                                                subject_id) + (1 | story))

summary(mod)

# Pairwise contrasts 
emm <- emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm

# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no", "NA"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm


contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

