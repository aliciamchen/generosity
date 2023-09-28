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
  read.csv(here('data/study4a_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  rename(likert_rating = response) %>% 
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

write.csv(d, here('data/study4a_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/study4a_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))

d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, altruistic_status_second) %>%
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
  theme(legend.position = "bottom")

f

# switch axes to see if it visualizes data better
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
  facet_wrap(~story)

f


# Stats for study 4a
mod <- lmer(data = d, likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)



# grouping

mod <- lmer(data = d %>% filter(altruistic_status_second != "just_met"), likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emm <- mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>% 
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emmeans(emm, pairwise ~ asymmetric | strategy)

# study 4a for presentation

stati = c("higher", "lower", "equal", "just_met")

for (status in stati) {
  
  
  f = ggplot(data = d %>% filter(altruistic_status_second == status),
             aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "Darjeeling1"),
      name = "strategy"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of 2nd time altruistic person", y = "how moral", fill = "strategy") +
    theme(legend.position = "bottom")  
  
  f
  
  ggsave(here(glue("figures/study4/4a_{status}.pdf")),
         width = 4.5,
         height = 7.5)
  
}





# Study 4b

d.4b <-
  read.csv(here('data/study4b_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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

write.csv(d, here('data/study4b_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/study4b_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d.4b$subject_id)))

d.4b.means.all <-
  d.4b %>% drop_na() %>%
  group_by(strategy, altruistic_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 

f = ggplot(data = d.4b %>% filter(response == "annoyed"),
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.4b.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.4b.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 4b", x = "status of second time altruistic person (A)", y = "how annoyed was A?") +
  theme(legend.position = "bottom")

f

f = ggplot(data = d.4b %>% filter(response == "satisfied"),
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.4b.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.4b.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 4b", x = "status of second time altruistic person (A)", y = "how satisfied was A with outcome?") +
  theme(legend.position = "bottom")

f

# 4b for presentation

# d.4b.annoyed <- d.4b %>% filter(response == "annoyed")
# d.4b.satisfied <- d.4b %>% filter(response == "satisfied")

# 4b annoyed for presentation
for (status in stati) {
  
  f = ggplot(data = d.4b %>% filter(response == "annoyed", altruistic_status_second == status),
             aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.4b.means.all %>% filter(response == "annoyed", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.4b.means.all %>% filter(response == "annoyed", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "Darjeeling1"),
      name = "strategy"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of 2nd time altruistic person", y = "how annoyed?", fill = "strategy") +
    theme(legend.position = "bottom")  
  
  f
  
  ggsave(here(glue("figures/study4/4b_annoyed_{status}.pdf")),
         width = 4.5,
         height = 7.5)
  
}

# 4b satisfied for presentation
for (status in stati) {
  
  f = ggplot(data = d.4b %>% filter(response == "satisfied", altruistic_status_second == status),
             aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.4b.means.all %>% filter(response == "satisfied", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.4b.means.all %>% filter(response == "satisfied", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "Darjeeling1"),
      name = "strategy"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of 2nd time altruistic person", y = "how satisfied?", fill = "strategy") +
    theme(legend.position = "bottom")  
  
  f
  
  ggsave(here(glue("figures/study4/4b_satisfied_{status}.pdf")),
         width = 4.5,
         height = 7.5)
  
}

# Stats for 4b

mod <- lmer(data = d.4b %>% filter(response == "annoyed"), likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)

mod <- lmer(data = d.4b %>% filter(response == "satisfied"), likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)



# Study 4c

d.4c <-
  read.csv(here('data/study4c_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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

write.csv(d, here('data/study4c_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/study4c_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d.4c$subject_id)))

d.4c.means.all <-
  d.4c %>% drop_na() %>%
  group_by(strategy, altruistic_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 

f = ggplot(data = d.4c %>% filter(response == "annoyed"),
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.4c.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.4c.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 4c", x = "status of second time altruistic person (A)", y = "how annoyed was B?") +
  theme(legend.position = "bottom")

f

f = ggplot(data = d.4c %>% filter(response == "satisfied"),
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.4c.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.4c.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 4c", x = "status of second time altruistic person (A)", y = "how satisfied was B with outcome?") +
  theme(legend.position = "bottom")

f

# 4c for presentation

# 4c annoyed for presentation
for (status in stati) {
  
  f = ggplot(data = d.4c %>% filter(response == "annoyed", altruistic_status_second == status),
             aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.4c.means.all %>% filter(response == "annoyed", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.4c.means.all %>% filter(response == "annoyed", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "Darjeeling1"),
      name = "strategy"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of 2nd time altruistic person (A)", y = "how annoyed was B?", fill = "strategy") +
    theme(legend.position = "bottom")  
  
  f
  
  ggsave(here(glue("figures/study4/4c_annoyed_{status}.pdf")),
         width = 4.5,
         height = 7.5)
  
}

# 4c satisfied for presentation
for (status in stati) {
  
  f = ggplot(data = d.4c %>% filter(response == "satisfied", altruistic_status_second == status),
             aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.4c.means.all %>% filter(response == "satisfied", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.4c.means.all %>% filter(response == "satisfied", altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "Darjeeling1"),
      name = "strategy"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of 2nd time altruistic person (A)", y = "how satisfied was B?", fill = "strategy") +
    theme(legend.position = "bottom")  
  
  f
  
  ggsave(here(glue("figures/study4/4c_satisfied_{status}.pdf")),
         width = 4.5,
         height = 7.5)
  
}

mod <- lmer(data = d.4c %>% filter(response == "annoyed"), likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)

mod <- lmer(data = d.4c %>% filter(response == "satisfied"), likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)

# 4d - fairness

d <-
  read.csv(here('data/study4d_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  rename(likert_rating = response) %>% 
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

write.csv(d, here('data/study4d_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/study4d_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))

d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, altruistic_status_second) %>%
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
  labs(x = "status of second time altruistic person", y = "how fair?") +
  theme(legend.position = "bottom")

f

# 4d for presentation


for (status in stati) {
  
  
  f = ggplot(data = d %>% filter(altruistic_status_second == status),
             aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(altruistic_status_second == status),
      mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "Darjeeling1"),
      name = "strategy"
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of 2nd time altruistic person", y = "how fair?", fill = "strategy") +
    theme(legend.position = "bottom")  
  
  f
  
  ggsave(here(glue("figures/study4/4d_{status}.pdf")),
         width = 4.5,
         height = 7.5)
  
}



# stats

mod <- lmer(data = d, likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)

# Without the jsut met
mod <- lmer(data = d %>% filter(altruistic_status_second != "just_met"), likert_rating ~ strategy * altruistic_status_second + (1 | subject_id) + (1 | story))

summary(mod)
emmeans(mod, pairwise ~ altruistic_status_second * strategy)
emm <- mod %>% emmeans(pairwise ~ altruistic_status_second * strategy) %>% 
  add_grouping("asymmetric",
               "altruistic_status_second",
               c("yes", "yes", "no"))

emmeans(emm, pairwise ~ asymmetric | strategy)

# Look at correlation between results for A and B

d.4b.means.story <-
  d.4b %>% drop_na() %>%
  group_by(strategy, altruistic_status_second, response, story) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 

d.4c.means.story <-
  d.4c %>% drop_na() %>%
  group_by(strategy, altruistic_status_second, response, story) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 

f = ggplot(data = d.4b %>% filter(response == "annoyed"),
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.4b.means.story %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.4b.means.story %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 4b", x = "status of second time altruistic person (A)", y = "how annoyed was A?") +
  theme(legend.position = "bottom") + 
  facet_wrap(~story)

f

f = ggplot(data = d.4c %>% filter(response == "annoyed"),
           aes(x = altruistic_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.4c.means.story %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.4c.means.story %>% filter(response == "annoyed"),
    mapping = aes(x = altruistic_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(title = "study 4c", x = "status of second time altruistic person (A)", y = "how annoyed was B?") +
  theme(legend.position = "bottom") +
  facet_wrap(~story)

f

## scatter plot
d.means.all <- d.4b.means.story %>% rename(A_response = likert_rating) %>% 
  left_join(d.4c.means.story %>% rename(B_response = likert_rating), 
            by = c("strategy", "altruistic_status_second", "response", "story"))

f <- ggplot(data = d.means.all, aes(x = A_response, y = B_response, 
                                    color = response, shape = altruistic_status_second)) +
  geom_point() +
  facet_wrap(~strategy)

f


# ggsave(here("figures/exp2b_violin.pdf"),
#        width = 8,
#        height = 7.8)

## Cogsci poster


f = ggplot(data = d %>% filter(relationship == "more", next_interaction != "none"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "more", next_interaction != "none"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "more", next_interaction != "none"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/cogsci_poster/2b_violin_higher.pdf"),
       width = 4.5,
       height = 7.5)


f = ggplot(data = d %>% filter(relationship == "equal", next_interaction != "none"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "equal", next_interaction != "none"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "equal", next_interaction != "none"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/cogsci_poster/2b_violin_equal.pdf"),
       width = 4.5,
       height = 7.5)

# For talk

f = ggplot(data = d %>% filter(relationship == "less", next_interaction != "none"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "less", next_interaction != "none"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "less", next_interaction != "none"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/cogsci_poster/2b_violin_lower.pdf"),
       width = 6,
       height = 7.5)


## Extra stuff


f = ggplot(data = d %>% filter(relationship == "more"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "more"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "more"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/sloan_talk/1b_violin_higher_cont.pdf"),
       width = 6,
       height = 7.5)


f = ggplot(data = d %>% filter(relationship == "equal"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "equal"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "equal"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/sloan_talk/1b_violin_equal_cont.pdf"),
       width = 6,
       height = 7.5)


f = ggplot(data = d %>% filter(relationship == "less"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "less"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "less"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/sloan_talk/1b_violin_lower_cont.pdf"),
       width = 6,
       height = 7.5)



## Stats


# With all levels
mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

emm <- mod %>% emmeans(pairwise ~ relationship * next_interaction)
emm


emm <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("relationship_symmetry", "relationship", c("yes", "no", "yes"))


emmeans(emm, pairwise ~ relationship_symmetry | next_interaction)


emm <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("all_relationships", "relationship", c("yes", "yes", "yes"))

emmeans(emm, pairwise ~ interaction_present | all_relationships)


