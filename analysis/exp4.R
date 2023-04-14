library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)

theme_set(theme_few(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

d <-
  read.csv(here('data/exp4_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
  )

d.demographics <- read.csv(here('data/exp4_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))

d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
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
  # scale_x_discrete(limits = c("symmetric", "asymmetric", "no_info")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of altruistic person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
# ggsave(here("figures/exp2_violin.pdf"),
#        width = 8,
#        height = 7.8)



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


