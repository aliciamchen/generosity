library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(emmeans)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

d <-
  read.csv(here('data/exp3_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("more", "equal", "less"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    social_interaction = fct_relevel(social_interaction,
                                   "precedent", "reciprocity", "no_interaction"),
    relationship = fct_relevel(relationship,
                               "more", "equal", "less")
  )

write.csv(d, here('data/exp3_tidy_data.csv'), row.names=FALSE)

d.demographics <- read.csv(here('data/exp3_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))


d$relationship <- factor(d$relationship, levels = c("more", "less", "equal")) # for plotting
d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, social_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


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
    values = c(wes_palette(name = "Cavalcanti1")[1], wes_palette(name = "Cavalcanti1")[3], wes_palette(name = "Cavalcanti1")[2]),
    name = "power/status of altruistic person"
    # breaks = c("more", "equal", "less")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of altruistic person") +
  theme(legend.position = "bottom")

f
# ggsave(here("figures/exp1b_violin.pdf"),
#        width = 8,
#        height = 7.8)



# For cogsci poster

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
    width = 0.09
  ) +
  scale_fill_manual(
    values = c(wes_palette(name = "Cavalcanti1")[1], wes_palette(name = "Cavalcanti1")[3], wes_palette(name = "Cavalcanti1")[2]),
    name = "power/status of altruistic person"
    # breaks = c("more", "equal", "less")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of altruistic person") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/cogsci_poster/1b_violin_prec.pdf"),
       width = 4.5,
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
    width = 0.09
  ) +
  scale_fill_manual(
    values = c(wes_palette(name = "Cavalcanti1")[1], wes_palette(name = "Cavalcanti1")[3], wes_palette(name = "Cavalcanti1")[2]),
    name = "power/status of altruistic person"
    # breaks = c("more", "equal", "less")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of altruistic person") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/cogsci_poster/1b_violin_rec.pdf"),
       width = 4.5,
       height = 7.5)

# Control

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
    width = 0.09
  ) +
  scale_fill_manual(
    values = c(wes_palette(name = "Cavalcanti1")[1], wes_palette(name = "Cavalcanti1")[3], wes_palette(name = "Cavalcanti1")[2]),
    name = "power/status of altruistic person"
    # breaks = c("more", "equal", "less")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of altruistic person") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/sloan_talk/2b_violin_control.pdf"),
       width = 4.5,
       height = 7.5)



# For talk


## Stats


# With all levels
mod <- lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

emm <- mod %>% emmeans(pairwise ~ relationship * social_interaction)
emm



emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("relationship_symmetry", "relationship", c("yes", "no", "yes"))

emmeans(emm, pairwise ~ relationship_symmetry | social_interaction)

