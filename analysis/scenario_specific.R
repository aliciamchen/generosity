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

theme_set(theme_few(base_size = 15))

## Experiment 1

d <-
  read.csv(here('data/exp1_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  rename("none" = "no_relationship") %>%
  pivot_longer(
    cols = c("asymmetric", "symmetric", "none"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    relationship = fct_relevel(relationship,
                               "symmetric", "asymmetric", "none"),
    social_interaction = fct_relevel(
      social_interaction,
      "precedent",
      "reciprocity",
      "no_interaction"
    )
  )

d.means.all <-
  d %>% drop_na() %>%
  group_by(social_interaction, relationship, story) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


# Violins
ggplot(data = d,
       aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 1.5,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1,
    width = 0.15
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship",
    breaks = c("symmetric", "asymmetric", "none")
  ) +
  scale_x_discrete(
    limits = c("precedent", "reciprocity", "no_interaction"),
    labels = c("precedent", "reciprocity", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom") +
  facet_wrap(~story) + 
  labs(title = "exp1")

# Individual points
ggplot(data = d,
       aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3, stackratio = .7, position = "dodge", alpha = 0.8, stroke = 0.1) +
  # geom_jitter(aes(color = relationship), position = position_jitterdodge(jitter.width = 0.7, dodge.width = 0.9, seed = 123)) +
  # geom_line(position = position_jitterdodge(jitter.width = 0.7, dodge.width = 0.9, seed = 123), alpha = 0.1, group = "") + 
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship",
    breaks = c("symmetric", "asymmetric", "none")
  ) +
  scale_x_discrete(
    limits = c("precedent", "reciprocity", "no_interaction"),
    labels = c("precedent", "reciprocity", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom") +
  facet_wrap(~story) + 
  labs(title = "exp1")


ggplot(data = d %>% filter(social_interaction == "precedent"),
       aes(x = relationship, y = likert_rating, group = subject_id))  + 
  geom_point(position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.1) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  facet_wrap(~story) + 
  labs(title = "exp1 precedent condition")

ggplot(data = d %>% filter(social_interaction == "reciprocity"),
       aes(x = relationship, y = likert_rating, group = subject_id))  + 
  geom_point(position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.1) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  facet_wrap(~story) + 
  labs(title = "exp1 reciprocity condition")

ggplot(data = d %>% filter(social_interaction == "no_interaction"),
       aes(x = relationship, y = likert_rating, group = subject_id))  + 
  geom_point(position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.1) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  facet_wrap(~story) + 
  labs(title = "exp1 no interaction condition")


## Exp 2

d <-
  read.csv(here('data/exp2_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
                               "symmetric", "asymmetric", "no_info")
  )

d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction, story) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"))


ggplot(data = d,
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = relationship, y = likert_rating),
    size = 1.5,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1,
    width = 0.15
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_x_discrete(limits = c("symmetric", "asymmetric", "no_info")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom") + 
  facet_wrap(~story) + 
  labs(title = "exp2")


# Individual points
ggplot(data = d,
       aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3, stackratio = .7, position = "dodge", alpha = 0.7, stroke = 0.1) +
  # geom_jitter(aes(color = relationship), position = position_jitterdodge(jitter.width = 0.7, dodge.width = 0.9, seed = 123)) +
  # geom_line(position = position_jitterdodge(jitter.width = 0.7, dodge.width = 0.9, seed = 123), alpha = 0.1, group = "") + 
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_x_discrete(limits = c("symmetric", "asymmetric", "no_info")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom") +
  facet_wrap(~story) + 
  labs(title = "exp2")


ggplot(data = d %>% filter(relationship == "asymmetric"),
       aes(x = next_interaction, y = likert_rating, group = subject_id))  + 
  geom_point(position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.1) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  facet_wrap(~story) + 
  labs(title = "exp2 asymmetric condition")

ggplot(data = d %>% filter(relationship == "symmetric"),
       aes(x = next_interaction, y = likert_rating, group = subject_id))  + 
  geom_point(position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.1) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  facet_wrap(~story) + 
  labs(title = "exp2 symmetric condition")

ggplot(data = d %>% filter(relationship == "no_info"),
       aes(x = next_interaction, y = likert_rating, group = subject_id))  + 
  geom_point(position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.1) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  facet_wrap(~story) + 
  labs(title = "exp2 no relationship info condition")

