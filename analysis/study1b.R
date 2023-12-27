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

# Pilot experiment benefit/effort data
validation.benefit <- read.csv(here('data/validation_benefit_diff.csv'))
validation.effort <- read.csv(here('data/validation_effort_diff.csv'))

d <-
  read.csv(here('data/1b_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
                               "more", "equal", "less")
  ) %>% 
  group_by(subject_id, story, relationship) %>%
  mutate(total_rating = sum(likert_rating),
         normalized_likert_rating = likert_rating / total_rating) %>%
  select(-total_rating) %>% 
  left_join(validation.benefit %>% select(story, diff), by = "story") %>% 
  rename(benefit_diff = diff) %>% 
  left_join(validation.effort %>% select(story, diff), by = "story") %>% 
  rename(effort_diff = diff) 


d.demographics <- read.csv(here('data/1b_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

print(length(unique(d$subject_id)))

################## PLOTS

d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"))


# Aggregated results on one plot
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
  scale_x_discrete(limits = c("more", "less", "equal")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

# grouped by story

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
  scale_x_discrete(limits = c("more", "less", "equal")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom") + 
  facet_wrap(~story)

f



## For paper (each condition one plot)


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
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1b_violin_higher_cont.pdf"),
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
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1b_violin_equal_cont.pdf"),
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
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1b_violin_lower_cont.pdf"),
       width = 6,
       height = 7.5)


## Look at sample individual scenarios 
stories = c('concerts', 'restaurant', 'family meals', 'meeting location')

for (s in stories) {
  
  f = ggplot(data = d %>% filter(relationship == "more", story == s),
             aes(x = relationship, y = likert_rating, fill = next_interaction)) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.story %>% filter(relationship == "more", story == s),
      mapping = aes(x = relationship, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.story %>% filter(relationship == "more", story == s),
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
    labs(x = "status of generous person", y = "how likely?", fill = "next interaction", title = s) +
    theme(legend.position = "bottom")
  
  f
  
  ggsave(here(glue("figures/outputs/1b_higher_{s}.pdf")),
         width = 6,
         height = 7.5)
  
}


for (s in stories) {
  
  f = ggplot(data = d %>% filter(relationship == "less", story == s),
             aes(x = relationship, y = likert_rating, fill = next_interaction)) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.story %>% filter(relationship == "less", story == s),
      mapping = aes(x = relationship, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.story %>% filter(relationship == "less", story == s),
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
    labs(x = "status of generous person", y = "how likely?", fill = "next interaction", title = s) +
    theme(legend.position = "bottom")
  
  f
  
  ggsave(here(glue("figures/outputs/1b_lower_{s}.pdf")),
         width = 6,
         height = 7.5)
  
}

# Benefit / effort plots

d.benefit.effort <- d %>% filter(relationship != "equal", next_interaction != "none") %>% select(-normalized_likert_rating) %>% 
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(rep_minus_alt = repeating - alternating) %>%
  ungroup()

d.b.e.means <- d.benefit.effort %>% 
  group_by(story, relationship, benefit_diff, effort_diff) %>% 
  tidyboot_mean(rep_minus_alt, na.rm = TRUE)

f <- ggplot(d.b.e.means %>% filter(relationship == "less"), aes(x = benefit_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative benefit to recipient", 
       y = "repeated minus alternated",
       title = "study 1b, A is lower status") 

f

ggsave(here("figures/outputs/1b_benefit_lower.pdf"),
       width = 8.7,
       height = 4)

f <- ggplot(d.b.e.means %>% filter(relationship == "more"), aes(x = benefit_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative benefit to recipient", 
       y = "repeated minus alternated",
       title = "study 1b, A is higher status") 

f

ggsave(here("figures/outputs/1b_benefit_higher.pdf"),
       width = 8.7,
       height = 4)


f <- ggplot(d.b.e.means %>% filter(relationship == "less"), aes(x = effort_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative effort", 
       y = "repeated minus alternated",
       title = "study 1b, A is lower status") 

f

ggsave(here("figures/outputs/1b_effort_lower.pdf"),
       width = 8.7,
       height = 4)


f <- ggplot(d.b.e.means %>% filter(relationship == "more"), aes(x = effort_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "relative effort", 
       y = "repeated minus alternated",
       title = "study 1b, A is higher status") 

f

ggsave(here("figures/outputs/1b_effort_higher.pdf"),
       width = 8.7,
       height = 4)

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

# replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emm_symmetry
emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction)

contrast(emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction), interaction = c("pairwise", "pairwise"))


# benefit / effort

mod <- lmer(rep_minus_alt ~ 1 + effort_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "more"))

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + effort_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "less"))

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + benefit_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "more"))

summary(mod)


mod <- lmer(rep_minus_alt ~ 1 + benefit_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "less"))

summary(mod)




##################################################

## Repeat all analyses with normalized values

# With all levels
mod <- lmer(normalized_likert_rating ~ 1 + next_interaction * relationship + (1 |
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

# replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emm_symmetry
emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction)

contrast(emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction), interaction = c("pairwise", "pairwise"))



