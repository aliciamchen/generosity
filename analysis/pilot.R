library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(afex)
library(brms)
library(forcats)
library(emmeans)
library(wesanderson)

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

theme_set(theme_classic(base_size = 15))


d <-
  read.csv(here('data/validation_benefit_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  pivot_longer(
    cols = c("expected_high_benefit", "expected_low_benefit"),
    names_to = "benefit",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention"))

write.csv(d, here('data/validation_benefit_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/validation_benefit_demographics.csv'))

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))



### 

diff <- d %>%
  group_by(story, benefit) %>%
  summarise(mean_rating = mean(likert_rating, na.rm = TRUE)) %>%
  spread(benefit, mean_rating) %>%
  mutate(diff = expected_high_benefit - expected_low_benefit, 
         diff = abs(diff))

diff

write.csv(diff, here("data/validation_benefit_diff.csv"), row.names = FALSE)


all.diffs.benefit <- d %>% 
  group_by(subject_id, story) %>% 
  spread(benefit, likert_rating) %>% 
  mutate(diff = expected_high_benefit - expected_low_benefit) %>% 
  mutate(type = "benefit")

# Load in effort data
all.diffs.effort <- read.csv(here('data/validation_effort_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  pivot_longer(
    cols = c("expected_high_benefit", "expected_low_benefit"),
    names_to = "benefit",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>% 
  group_by(subject_id, story) %>% 
  spread(benefit, likert_rating) %>% 
  mutate(diff = expected_high_benefit - expected_low_benefit, 
         type = "effort")

d.demographics <- read.csv(here('data/validation_effort_demographics.csv'))

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))



all.diffs.benefit.means <- all.diffs.benefit %>% 
  group_by(story) %>% 
  tidyboot_mean(diff, na.rm = TRUE) %>% 
  mutate(type = "benefit")

all.diffs.effort.means <- all.diffs.effort %>% 
  group_by(story) %>% 
  tidyboot_mean(diff, na.rm = TRUE) %>% 
  mutate(type = "effort")


df.stacked.all <- bind_rows(all.diffs.benefit, all.diffs.effort) 

df.stacked.means <- df.stacked.all %>% 
  group_by(story, type) %>% 
  tidyboot_mean(diff, na.rm = TRUE) %>% 
  rename(diff = empirical_stat)

# arrange descending
df.temp <- df.stacked.means %>% 
  filter(type == "benefit") %>% 
  arrange(desc(diff)) 

levs <- unique(df.temp$story)

df.stacked.all$story <- factor(df.stacked.all$story, levels=levs)

## Pretty plot
f <- ggplot(df.stacked.all, aes(x = story, y = diff, fill = type)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4)) +
  geom_point(
    data = df.stacked.means,
    aes(x = story, y = diff),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = df.stacked.means,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  labs(x = "story", y = "A minus B", title = 'relative cost/benefit for all scenarios') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

# ggsave(here("figures/validation.pdf"), width = 10, height = 3)



# Are these scenarios coordination or zero sum? 
all.diffs.benefit.long <- all.diffs.benefit %>% pivot_longer(
  names_to = "which_one", 
  cols = c("expected_high_benefit", "expected_low_benefit")
)

all.diffs.effort.long <- all.diffs.effort %>% pivot_longer(
  names_to = "which_one", 
  cols = c("expected_high_benefit", "expected_low_benefit")
)


all.diffs.benefit.long$story <- factor(all.diffs.benefit.long$story, levels=levs)
all.diffs.effort.long$story <- factor(all.diffs.effort.long$story, levels=levs)
my.means <- all.diffs.benefit.long %>% 
  group_by(story, which_one) %>% 
  tidyboot_mean(value, na.rm = T) %>% 
  rename(value = empirical_stat)


f <- ggplot(all.diffs.benefit.long %>% filter(which_one == "expected_high_benefit"), aes(x = story, y = value)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4), 
              fill = "#F7A19F") +
  geom_point(
    data = my.means %>% filter(which_one == "expected_high_benefit"),
    aes(x = story, y = value),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = my.means %>% filter(which_one == "expected_high_benefit"),
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "story", y = "B benefit compared to not interacting") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

ggsave(here("figures/outputs/B_benefit.pdf"), width = 10, height = 3)

f <- ggplot(all.diffs.benefit.long %>% filter(which_one == "expected_low_benefit"), aes(x = story, y = value)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4), 
              fill = "#F7A19F") +
  geom_point(
    data = my.means %>% filter(which_one == "expected_low_benefit"),
    aes(x = story, y = value),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = my.means %>% filter(which_one == "expected_low_benefit"),
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "story", y = "A benefit compared to not interacting") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

ggsave(here("figures/outputs/A_benefit.pdf"), width = 10, height = 3)

# effort
my.means <- all.diffs.effort.long %>% 
  group_by(story, which_one) %>% 
  tidyboot_mean(value, na.rm = T) %>% 
  rename(value = empirical_stat)


f <- ggplot(all.diffs.effort.long %>% filter(which_one == "expected_low_benefit"), aes(x = story, y = value)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4),
              fill = "#99CCCC") +
  geom_point(
    data = my.means %>% filter(which_one == "expected_low_benefit"),
    aes(x = story, y = value),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = my.means %>% filter(which_one == "expected_low_benefit"),
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "story", y = "A effort compared to not interacting") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

ggsave(here("figures/outputs/A_effort.pdf"), width = 10, height = 3)

f <- ggplot(all.diffs.effort.long %>% filter(which_one == "expected_high_benefit"), aes(x = story, y = value)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4),
              fill = "#99CCCC") +
  geom_point(
    data = my.means %>% filter(which_one == "expected_high_benefit"),
    aes(x = story, y = value),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = my.means %>% filter(which_one == "expected_high_benefit"),
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "story", y = "B effort compared to not interacting") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

ggsave(here("figures/outputs/B_effort.pdf"), width = 10, height = 3)



# Relative cost and benefit

f <- ggplot(df.stacked.all %>% filter(type == "benefit"), aes(x = story, y = diff)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4),
              fill = "#F7A19F") +
  geom_point(
    data = df.stacked.means %>% filter(type == "benefit"),
    aes(x = story, y = diff),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = df.stacked.means %>% filter(type == "benefit"),
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "story", y = "B minus A") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f
ggsave(here("figures/outputs/B_benefits_more.pdf"), width = 10, height = 3)


f <- ggplot(df.stacked.all %>% filter(type == "effort"), aes(x = story, y = diff)) + 
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4),
              fill = "#99CCCC") +
  geom_point(
    data = df.stacked.means %>% filter(type == "effort"),
    aes(x = story, y = diff),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = df.stacked.means %>% filter(type == "effort"), # so jank but whatever
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "story", y = "B minus A") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f
ggsave(here("figures/outputs/A_effort_more.pdf"), width = 10, height = 3)



# Figure out what 2 stories to exclude for study 3
# using abs of sum of benefit and effort diff
smallest_diffs <- df.stacked.means %>% 
  pivot_wider(names_from = type, values_from = diff, id_cols = story) %>% 
  mutate(sum_abs_diff = abs(benefit) + abs(effort)) %>% 
  arrange(sum_abs_diff) 


print(smallest_diffs)
