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
  read.csv(here('data/3d_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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

write.csv(d, here('data/3d_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/3d_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age),  min_age = min(age),
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
  labs(title = "study 3d", x = "status of second time altruistic person (A)", y = "how annoyed was B?") +
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
  labs(title = "study 3d", x = "status of second time altruistic person (A)", y = "how satisfied was B with outcome?") +
  theme(legend.position = "bottom")

f


strategies = c("repeating", "alternating")
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
  
  ggsave(here(glue("figures/outputs/3d_annoyed_{strat}.pdf")),
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
  
  ggsave(here(glue("figures/3d_satisfied_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}



########## STATS

emm_options(lmerTest.limit = 5614)
emm_options(pbkrtest.limit = 5614)

######### ANNOYED RESPONSE 

# Main preregistered analysis: without "just met" condition 

mod <-
  lmer(
    data = d %>% filter(altruistic_status_second != "just_met", response == "annoyed"),
    likert_rating ~ strategy * altruistic_status_second + (1 |
                                                             subject_id) + (1 | story)
  )

summary(mod)


# Pairwise contrasts 
emm <- emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm

# altruistic_status_second strategy    emmean    SE   df lower.CL upper.CL
# higher                   repeating     4.95 0.154 35.5     4.63     5.26
# lower                    repeating     4.97 0.154 35.4     4.66     5.28
# equal                    repeating     5.13 0.154 35.4     4.82     5.44
# higher                   alternating   5.75 0.154 35.4     5.44     6.06
# lower                    alternating   5.58 0.154 35.5     5.27     5.89
# equal                    alternating   6.09 0.154 35.5     5.77     6.40
# # # 
# # 
# 
# contrast                               estimate    SE   df t.ratio p.value
# higher repeating - lower repeating      -0.0205 0.108 1912  -0.191  1.0000
# higher alternating - lower alternating   0.1707 0.108 1913   1.584  0.6090


# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# altruistic_status_second_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# higher - lower                    repeating - alternating   -0.191 0.152 1912  -1.256  0.2094


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm

# asymmetric strategy    emmean    SE   df lower.CL upper.CL
# no         repeating     5.13 0.154 35.4     4.82     5.44
# yes        repeating     4.96 0.144 27.3     4.66     5.25
# no         alternating   6.09 0.154 35.5     5.77     6.40
# yes        alternating   5.66 0.144 27.3     5.37     5.96

# contrast                         estimate     SE   df t.ratio p.value
# no repeating - yes repeating        0.170 0.0932 1912   1.826  0.2612
# no alternating - yes alternating    0.423 0.0933 1913   4.534  <.0001


contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# no - yes            repeating - alternating   -0.253 0.132 1912  -1.918  0.0552



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


# Pairwise contrasts 
emm <- emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm

# altruistic_status_second strategy    emmean    SE   df lower.CL upper.CL
# higher                   repeating     4.95 0.154 35.5     4.63     5.26
# lower                    repeating     4.97 0.154 35.4     4.66     5.28
# equal                    repeating     5.13 0.154 35.4     4.82     5.44
# higher                   alternating   5.75 0.154 35.4     5.44     6.06
# lower                    alternating   5.58 0.154 35.5     5.27     5.89
# equal                    alternating   6.09 0.154 35.5     5.77     6.40
# # 

# contrast                               estimate    SE   df t.ratio p.value
# higher repeating - lower repeating      -0.0205 0.108 1912  -0.191  1.0000
# higher alternating - lower alternating   0.1707 0.108 1913   1.584  0.6090




# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# altruistic_status_second_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# higher - lower                    repeating - alternating   -0.191 0.152 1912  -1.256  0.2094


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>%
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emm <- emmeans(emm, pairwise ~ asymmetric * strategy)
emm

# asymmetric strategy    emmean    SE   df lower.CL upper.CL
# no         repeating     5.13 0.154 35.4     4.82     5.44
# yes        repeating     4.96 0.144 27.3     4.66     5.25
# no         alternating   6.09 0.154 35.5     5.77     6.40
# yes        alternating   5.66 0.144 27.3     5.37     5.96
# # 
# 
# contrast                         estimate     SE   df t.ratio p.value
# no repeating - yes repeating        0.170 0.0932 1912   1.826  0.2612
# no alternating - yes alternating    0.423 0.0933 1913   4.534  <.0001


contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# no - yes            repeating - alternating   -0.253 0.132 1912  -1.918  0.0552



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

# contrast                         estimate    SE  df z.ratio p.value
# no repeating - yes repeating     -0.01506 0.113 5420  -0.134  1.0000
# no alternating - yes alternating  0.12947 0.113 5424   1.150  0.8602

contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric_pairwise strategy_pairwise       estimate    SE   df t.ratio p.value
# no - yes            repeating - alternating  -0.1445 0.159 5420  -0.908  0.3640

