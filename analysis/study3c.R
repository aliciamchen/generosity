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
  read.csv(here('data/3c_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("annoyed", "satisfied"), 
    names_to = "response", 
    values_to = "likert_rating"
  ) %>% 
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>% 
  mutate(altruistic_status_second = case_when(
    altruistic_status_second == "more" ~ "higher",
    altruistic_status_second == "less" ~ "lower",
    TRUE ~ altruistic_status_second
  )) %>% 
  mutate(
    strategy = fct_relevel(strategy,
                           "repeating", "alternating"),
    altruistic_status_second = fct_relevel(altruistic_status_second,
                                           "higher", "lower", "equal", "just_met")
  )

write.csv(d, here('data/3c_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/3c_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age),   min_age = min(age),
                             max_age = max(age))

print(length(unique(d$subject_id)))

d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, altruistic_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 

f = ggplot(data = d %>% filter(response == "annoyed"),
           aes(x = strategy, y = likert_rating, fill = altruistic_status_second)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = strategy, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 3c", x = "status of second time altruistic person (A)", y = "how annoyed was A?") +
  theme(legend.position = "bottom")

f

f = ggplot(data = d %>% filter(response == "satisfied"),
           aes(x = strategy, y = likert_rating, fill = altruistic_status_second)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = strategy, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 3c", x = "status of second time altruistic person (A)", y = "how satisfied was A with outcome?") +
  theme(legend.position = "bottom")

f

# 3c for presentation
strategies = c("repeating", "alternating")


# annoyed
for (strat in strategies) {
  
  
  f = ggplot(data = d %>% filter(strategy == strat, response == "annoyed"),
             aes(x = strategy, y = likert_rating, fill = altruistic_status_second)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(strategy == strat, response == "annoyed"),
      mapping = aes(x = strategy, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(strategy == strat, response == "annoyed"),
      mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 4, name = "Darjeeling1"),
      name = "social relationship"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "strategy", y = "how annoyed", fill = "social relationship") +
    theme(legend.position = "bottom")  
  
  print(f)
  
  ggsave(here(glue("figures/outputs/3c_annoyed_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}

for (strat in strategies) {
  
  f = ggplot(data = d %>% filter(strategy == strat, response == "satisfied"),
             aes(x = strategy, y = likert_rating, fill = altruistic_status_second)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(strategy == strat, response == "satisfied"),
      mapping = aes(x = strategy, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(strategy == strat, response == "satisfied"),
      mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 4, name = "Darjeeling1"),
      name = "social relationship"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "strategy", y = "how satisfied", fill = "social relationship") +
    theme(legend.position = "bottom")  
  
  print(f)
  
  ggsave(here(glue("figures/outputs/3c_satisfied_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}


########## STATS

######### ANNOYED RESPONSE

# Main preregistered analysis: without "just met" condition 

mod <-
  lmer(
    data = d %>% filter(altruistic_status_second != "just_met", response == "annoyed"),
    likert_rating ~ strategy * altruistic_status_second + (1 |
                                                             subject_id) + (1 | story)
  )

summary(mod)

# Average of strategy
emm <- emmeans(mod, "strategy")
emm
pairs(emm)

# strategy    emmean   SE   df lower.CL upper.CL
# repeating     3.01 0.12 22.4     2.76     3.26
# alternating   1.49 0.12 22.4     1.24     1.74
# 
# contrast                estimate     SE   df t.ratio p.value
# repeating - alternating     1.51 0.0561 1870  26.972  <.0001


# Pairwise contrasts 
emm <- emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm

# altruistic_status_second strategy    emmean    SE   df lower.CL upper.CL
# higher                   repeating     2.59 0.133 33.3     2.32     2.86
# lower                    repeating     3.54 0.133 33.3     3.27     3.81
# equal                    repeating     2.90 0.133 33.3     2.63     3.16
# higher                   alternating   1.53 0.133 33.3     1.26     1.80
# lower                    alternating   1.56 0.133 33.3     1.29     1.83
# equal                    alternating   1.39 0.133 33.3     1.12     1.66
# # 
# 
# contrast                               estimate     SE   df t.ratio p.value
# higher repeating - lower repeating       -0.944 0.0972 1870  -9.711  <.0001
# higher alternating - lower alternating   -0.035 0.0972 1870  -0.360  0.9992


# Check alternating > repeating
emm <- emmeans(emm, pairwise ~ strategy | altruistic_status_second)
emm

# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# altruistic_status_second_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# higher - lower                    repeating - alternating   -0.909 0.138 1870  -6.613  <.0001


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm

# asymmetric strategy    emmean    SE   df lower.CL upper.CL
# no         repeating     2.90 0.133 33.3     2.63     3.16
# yes        repeating     3.06 0.123 25.0     2.81     3.32
# no         alternating   1.39 0.133 33.3     1.12     1.66
# yes        alternating   1.55 0.123 25.0     1.29     1.80
# # 
# 
# contrast                         estimate     SE   df t.ratio p.value
# no repeating - yes repeating       -0.168 0.0842 1870  -1.998  0.1891
# no alternating - yes alternating   -0.160 0.0842 1871  -1.901  0.2280


contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# no - yes            repeating - alternating -0.00814 0.119 1870  -0.068  0.9455





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


######### Satisfied response

# Main preregistered analysis: without "just met" condition 

mod <-
  lmer(
    data = d %>% filter(altruistic_status_second != "just_met", response == "satisfied"),
    likert_rating ~ strategy * altruistic_status_second + (1 |
                                                             subject_id) + (1 | story)
  )

summary(mod)

# Average of strategy
emm <- emmeans(mod, "strategy")
emm
pairs(emm)


# Pairwise contrasts 
emm <- emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm

# altruistic_status_second strategy    emmean    SE   df lower.CL upper.CL
# higher                   repeating     3.83 0.162 35.6     3.51     4.16
# lower                    repeating     2.85 0.162 35.6     2.52     3.18
# equal                    repeating     3.45 0.162 35.6     3.12     3.78
# higher                   alternating   5.19 0.162 35.7     4.86     5.51
# lower                    alternating   5.20 0.162 35.6     4.87     5.53
# equal                    alternating   5.61 0.162 35.6     5.28     5.93
# 
# contrast                               estimate    SE   df t.ratio p.value
# higher repeating - lower repeating       0.9809 0.116 1869   8.429  <.0001
# higher alternating - lower alternating  -0.0134 0.116 1869  -0.115  1.0000



# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# altruistic_status_second_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# higher - lower                    repeating - alternating    0.994 0.165 1869   6.044  <.0001


emm <- emmeans(emm, pairwise ~ strategy | altruistic_status_second)
emm


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm

# asymmetric strategy    emmean    SE   df lower.CL upper.CL
# no         repeating     3.45 0.162 35.6     3.12     3.78
# yes        repeating     3.34 0.151 27.0     3.03     3.65
# no         alternating   5.61 0.162 35.6     5.28     5.93
# yes        alternating   5.19 0.151 27.0     4.88     5.50
# # 
# 
# contrast                         estimate     SE   df t.ratio p.value
# no repeating - yes repeating        0.105 0.1007 1870   1.046  0.7222
# no alternating - yes alternating    0.412 0.1007 1870   4.094  0.0003


contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# no - yes            repeating - alternating   -0.307 0.142 1869  -2.156  0.0312



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

