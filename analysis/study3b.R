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
  read.csv(here('data/3b_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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

write.csv(d, here('data/3b_tidy_data.csv'), row.names = FALSE)

d.demographics <- read.csv(here('data/3b_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))

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
  labs(x = "strategy", y = "how fair?") +
  theme(legend.position = "bottom")

f

# Compare with study 1c 
# This needs d.first.response from the study 1c data

d.first.response <-
  read.csv(here('data/1c_tidy_data.csv')) %>%
  mutate(first_response_higher = recode(
    first_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(story, effort_diff, benefit_diff) %>%
  tidyboot_mean(first_response_higher, na.rm = TRUE)

d.fairness.story <-
  d %>% drop_na() %>%
  group_by(strategy, story, altruistic_status_second) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE)

d.fairness.with.1c <-
  left_join(
    d.first.response,
    d.fairness.story,
    suffix = c("_1c", "_3"),
    by = (c("story"))
  )


f <-
  ggplot(
    d.fairness.with.1c %>% filter(
      altruistic_status_second != "equal" &
        altruistic_status_second != "just_met"
    ),
    aes(x = empirical_stat_1c, y = empirical_stat_3, color = altruistic_status_second)
  ) +
  geom_point(size = 3.3,
             alpha = 0.7,
             stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1c, ymin = ci_lower_3, ymax = ci_upper_3),
    size = 1.5,
    width = 0.06,
    alpha = 0.6
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_3, xmin = ci_lower_1c, xmax = ci_upper_1c),
    size = 1.5,
    height = 0.1,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 4,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  # scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "study 1c first time higher", y = "study 3 how fair", color = "who is generous")

f

# 3b for presentation
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
    labs(x = "strategy", y = "how fair", fill = "social relationship") +
    theme(legend.position = "bottom")
  
  print(f)
  
  ggsave(here(glue("figures/3b_{strat}.pdf")),
         width = 4.3,
         height = 7.5)

}



########## STATS

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

# altruistic_status_second strategy    emmean     SE   df lower.CL upper.CL
# higher                   repeating     4.18 0.0865 92.5     4.00     4.35
# lower                    repeating     3.19 0.0866 92.8     3.01     3.36
# equal                    repeating     3.42 0.0865 92.5     3.25     3.59
# higher                   alternating   6.16 0.0864 92.2     5.99     6.33
# lower                    alternating   5.98 0.0864 92.2     5.81     6.15
# equal                    alternating   6.55 0.0864 92.2     6.37     6.72
# 
# contrast                               estimate     SE   df t.ratio p.value
# higher repeating - lower repeating        0.991 0.0928 1903  10.675  <.0001
# higher alternating - lower alternating    0.179 0.0926 1902   1.930  0.3839


# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# altruistic_status_second_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# higher - lower                    repeating - alternating    0.812 0.131 1902   6.193  <.0001


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm

# asymmetric strategy    emmean     SE   df lower.CL upper.CL
# no         repeating     3.42 0.0865 92.5     3.25     3.59
# yes        repeating     3.68 0.0731 47.4     3.53     3.83
# no         alternating   6.55 0.0864 92.2     6.37     6.72
# yes        alternating   6.07 0.0730 47.2     5.92     6.22
# 
# contrast                         estimate     SE   df t.ratio p.value
# no repeating - yes repeating       -0.259 0.0803 1902  -3.221  0.0071
# no alternating - yes alternating    0.475 0.0802 1902   5.925  <.0001

contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# no - yes            repeating - alternating   -0.734 0.113 1902  -6.468  <.0001



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

