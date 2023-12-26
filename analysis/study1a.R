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
  read.csv(here('data/1a_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
                               "asymmetric", "symmetric", "no_info")
  ) %>% # Then, normalize likert rating
  group_by(subject_id, story, relationship) %>%
  mutate(total_rating = sum(likert_rating),
         normalized_likert_rating = likert_rating / total_rating) %>%
  select(-total_rating) 

d.demographics <- read.csv(here('data/1a_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))


################## PLOTS


# Calculate means for plotting
d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"), 
         relationship = fct_relevel(relationship,
                                    "asymmetric", "symmetric", "no_info")) 


# Plot aggregated results on one plot
f = ggplot(data = d,
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1a_violin_all.pdf"),
       width = 8,
       height = 7.8)



# Plot grouped by story
d.means.story <-
  d %>% drop_na() %>%
  group_by(story, relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"))


f = ggplot(data = d,
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.story,
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.story,
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom") + 
  facet_wrap(~story)


f


# Individual conditions (to go into paper) 

f = ggplot(data = d %>% filter(relationship == "asymmetric"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "asymmetric"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "asymmetric"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/outputs/1a_violin_asym_cont.pdf"),
       width = 6,
       height = 7.5)


f = ggplot(data = d %>% filter(relationship == "symmetric"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "symmetric"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "symmetric"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/outputs/1a_violin_sym_cont.pdf"),
       width = 6,
       height = 7.5)



f = ggplot(data = d %>% filter(relationship == "no_info"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "no_info"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "no_info"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/outputs/1a_violin_noinfo_cont.pdf"),
       width = 6,
       height = 7.5)


# Look at sample individual scenarios

stories = c('concerts', 'restaurant', 'family meals', 'meeting location')

for (s in stories) {
  f = ggplot(data = d %>% filter(relationship == "asymmetric", story == s),
             aes(x = relationship, y = likert_rating, fill = next_interaction)) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.story %>% filter(relationship == "asymmetric", story == s),
      mapping = aes(x = relationship, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.story %>% filter(relationship == "asymmetric", story == s),
      mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.09
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "FantasticFox1"),
      name = "next interaction",
      breaks = c("repeating", "alternating", "none")
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "relationship", y = "how likely?", fill = "next interaction", title = s) +
    theme(legend.position = "bottom")
  
  f
  
  
  ggsave(here(glue("figures/outputs/1a_asymmetric_{s}.pdf")),
         width = 6,
         height = 7.5)
}

for (s in stories) {
  f = ggplot(data = d %>% filter(relationship == "symmetric", story == s),
             aes(x = relationship, y = likert_rating, fill = next_interaction)) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.story %>% filter(relationship == "symmetric", story == s),
      mapping = aes(x = relationship, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.story %>% filter(relationship == "symmetric", story == s),
      mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.09
    ) +
    scale_fill_manual(
      values = wes_palette(n = 3, name = "FantasticFox1"),
      name = "next interaction",
      breaks = c("repeating", "alternating", "none")
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "relationship", y = "how likely?", fill = "next interaction", title = s) +
    theme(legend.position = "bottom")
  
  f
  
  
  ggsave(here(glue("figures/outputs/1a_symmetric_{s}.pdf")),
         width = 6,
         height = 7.5)
}


################## STATS

# With all levels
mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# Pairwise contrasts
emm <- mod %>% emmeans(pairwise ~ relationship * next_interaction)
emm

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast_test <- contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# asymmetric - no_info   repeating - alternating     1.6302 0.145 3095  11.259  <.0001
# symmetric - no_info    repeating - alternating    -0.4058 0.145 3095  -2.803  0.0051

# Do people expect a continued social interaction, both with and without the relationship? 
emm <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


# Without all levels - main preregistered analysis
d_filtered <- d %>%
  filter(next_interaction != "none" & relationship != "no_info")

mod <- lmer(likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 | subject_id),
            data = d_filtered)

summary(mod)

##################################################

## Repeat all analyses with normalized values

# With all levels
mod <- lmer(normalized_likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)


# Pairwise contrasts
emm <- mod %>% emmeans(pairwise ~ relationship * next_interaction)
emm

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast_test <- contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# Do people expect a continued social interaction, both with and without the relationship? 
emm <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


# Without all levels - main preregistered analysis
d_filtered <- d %>%
  filter(next_interaction != "none" & relationship != "no_info")

mod <- lmer(normalized_likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 | subject_id),
            data = d_filtered)

summary(mod)
