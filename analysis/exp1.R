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

theme_set(theme_classic(base_size = 30))

## Plot

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
                               "asymmetric", "symmetric", "none"),
    social_interaction = fct_relevel(
      social_interaction,
      "precedent",
      "reciprocity",
      "no_interaction"
    )
  )

write.csv(d, here('data/exp1_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/exp1_demographics.csv'))

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))

d.means.all <-
  d %>% drop_na() %>%
  group_by(social_interaction, relationship) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)

# Plot with all the stuff together
f = ggplot(data = d,
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
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
  theme(legend.position = "bottom")

f
# ggsave(here("figures/exp1a_violin.pdf"),
#        width = 8,
#        height = 7.8)

## Plots for cogsci poster

f = ggplot(data = d %>% filter(relationship != "none", social_interaction == "precedent"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship != "none", social_interaction == "precedent"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship != "none", social_interaction == "precedent"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/cogsci_poster/1a_violin_prec.pdf"),
       width = 4.5,
       height = 7.5)



f = ggplot(data = d %>% filter(relationship != "none", social_interaction == "reciprocity"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship != "none", social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship != "none", social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/cogsci_poster/1a_violin_rec.pdf"),
       width = 4.5,
       height = 7.5)

# With controls

f = ggplot(data = d %>% filter(social_interaction == "precedent"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "precedent"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "precedent"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/sloan_talk/2a_violin_control_prec.pdf"),
       width = 6,
       height = 7.5)



f = ggplot(data = d %>% filter(social_interaction == "reciprocity"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/sloan_talk/2a_violin_control_rec.pdf"),
       width = 6,
       height = 7.5)

f = ggplot(data = d %>% filter(social_interaction == "no_interaction"),
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "no_interaction"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "no_interaction"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.06
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "Darjeeling1"),
    name = "relationship"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "relationship") +
  theme(legend.position = "bottom")  

f

ggsave(here("figures/sloan_talk/2a_violin_control.pdf"),
       width = 6,
       height = 7.5)

## Analysis

# Without all levels
d_filtered <- d %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none")

mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 | subject_id),
            data = d_filtered)

summary(mod)

# With all levels
mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("interaction_present",
               "social_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


##################################################

## Repeat all analyses with normalized values

d <-
  read.csv(here('data/exp1_data.csv')) %>% filter(pass_attention == T, understood == "yes") %>%
  mutate(newSum = select_if(., is.numeric) %>%
           reduce(`+`)) %>%
  mutate_if(is.numeric, list( ~ . / newSum)) %>%
  select(-newSum) %>%
  rename("none" = "no_relationship") %>%
  pivot_longer(
    cols = c("asymmetric", "symmetric", "none"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
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
  group_by(social_interaction, relationship) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)

d_filtered <- d %>%
  filter(social_interaction != "no_interaction" &
           relationship != "none")

mod <- lmer(likert_rating ~ social_interaction * relationship + (1 |
                                                                   story) + (1 | subject_id),
            data = d_filtered)

summary(mod)


mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                       story) + (1 | subject_id),
            data = d)

summary(mod)

emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("interaction_present",
               "social_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)
