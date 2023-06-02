library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)

theme_set(theme_few(base_size = 20))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

d <-
  read.csv(here('data/tacit_data.csv')) %>% filter(understood == 'yes', story != 'fundraising') %>% # add attention check back later, and also add fundraising story
  mutate(symmetry = ifelse(altruistic_status == "equal", "symmetric", "asymmetric"),
         strategy = ifelse(first_meeting == response, "repeating", "alternating")) # again fix later, this might not need to act on whole df

# write.csv(d, here('data/exp2_tidy_data.csv'), row.names=FALSE)

# d.demographics <- read.csv(here('data/exp2_demographics.csv'))
# d.demographics %>% count(gender)
# d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

# print(length(unique(d$subject_id)))

## What happens the first stage? In asymmetric relationships, do people expect the lower-status person to act the first time?

stage.one <- d %>%  filter(symmetry == "asymmetric", stage == "first") %>% 
  mutate(response_status_ = ifelse(response_status == "more", 1, 0))

stage.one.mean <-
  stage.one %>% 
  tidyboot_mean(response_status_, na.rm = TRUE)

f = ggplot(data = stage.one, aes(x = stage, y = 2)) +
           geom_point(
             stage.one.mean,
             mapping = aes(x = 'first time', y = empirical_stat),
             size = 2.3,
             alpha = 1,
             position = position_dodge(width = 0.8)
           ) +
             geom_errorbar(
               stage.one.mean,
               mapping = aes(x = "first time", ymin = ci_lower, ymax = ci_upper),
               position = position_dodge(width = 0.8),
               size = 1.5,
               width = 0.09
             ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(y = "expectations for high status", title = "asymmetric relationship first meeting") 
           

f

effort_diff <- read.csv(here("data/validation_effort_diff.csv")) %>% rename(effort_diff = diff) %>% select(c(story, effort_diff))
benefit_diff <- read.csv(here("data/validation_benefit_diff.csv")) %>% rename(benefit_diff = diff) %>% select(c(story, benefit_diff))


stage.one.mean.story <- 
  stage.one %>% group_by(story) %>% 
  tidyboot_mean(response_status_, na.rm = TRUE) %>% 
  left_join(effort_diff, id = story) %>% 
  left_join(benefit_diff, id = story)

f = ggplot(data = stage.one, aes(x = stage, y = 0)) +
  geom_point(
    stage.one.mean.story,
    mapping = aes(x = 'first time', y = empirical_stat),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    stage.one.mean.story,
    mapping = aes(x = "first time", ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  facet_wrap(~story) + 
  labs(y = "expectations for high status", title = "asymmetric relationship first meeting") 

f

# correlation expectations for high status with relative cost for each scenario.


f <- ggplot(stage.one.mean.story, aes(x = effort_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(x = "perceived effort difference in scenario", y = "expectations for high status", title = "asymmetric relationships first meeting")
  

f

f <- ggplot(stage.one.mean.story, aes(x = benefit_diff, y = empirical_stat)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(x = "perceived benefit difference in scenario", y = "expectations for high status", title = "asymmetric relationships first meeting")


f

f <- ggplot(stage.one.mean.story, aes(x = effort_diff, y = benefit_diff)) + 
  geom_point(size = 3, alpha = 0.7) + 
  labs(x = "perceived effort difference", y = "perceived benefit difference")

f


# What happens in the second stage? 

# d.temp <- d %>% pivot_wider(names_from = stage, values_from = c(first_meeting, response))

d.temp <- d %>% 
  mutate(correct_first_response = ifelse(stage == "first",
                                         ifelse(first_meeting == response, "correct", "incorrect"),
                                         NA)) %>% 
  arrange(subject_id, story, stage) %>% 
  group_by(subject_id, story) %>% 
  fill(correct_first_response)

d.second <- d.temp %>% filter(stage == "second") %>% 
  mutate(strategy_ = ifelse(strategy == "repeating", 0, 1)) 


d.second.means <- d.second %>% 
  group_by(symmetry, correct_first_response) %>% 
  tidyboot_mean(strategy_, na.rm = T)

d.second.scenarios <- d.second %>% 
  group_by(story, symmetry, correct_first_response) %>% 
  tidyboot_mean(strategy_, na.rm = T)

f = ggplot(data = d.second.means, aes(x = correct_first_response, y = empirical_stat, color = symmetry)) +
  geom_point(
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = correct_first_response, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(y = "expectation for alternating strategy", title = "second time") 



f

f = ggplot(data = d.second.scenarios, aes(x = correct_first_response, y = empirical_stat, color = symmetry)) +
  geom_point(
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = correct_first_response, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.3,
    width = 0.3
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(y = "expectation for alternating strategy", title = "second time") +
  facet_wrap(~story)

f

d.second.means <- d.second %>% 
  group_by(altruistic_status, correct_first_response) %>% 
  tidyboot_mean(strategy_, na.rm = T)

f = ggplot(data = d.second.means, aes(x = correct_first_response, y = empirical_stat, color = altruistic_status)) +
  geom_point(
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = correct_first_response, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(y = "expectation for alternating strategy", title = "second time") 

f

d.second.scenarios <- d.second %>% 
  group_by(story, altruistic_status, correct_first_response) %>% 
  tidyboot_mean(strategy_, na.rm = T)

f = ggplot(data = d.second.scenarios, aes(x = correct_first_response, y = empirical_stat, color = altruistic_status)) +
  geom_point(
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = correct_first_response, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.3,
    width = 0.3
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(y = "expectation for alternating strategy", title = "second time") +
  facet_wrap(~story)

f

#########
ggplot(d, aes(x = stage, fill = strategy)) +
  geom_bar(position = "fill") +
  labs(x = "stage", y = "Proportion", fill = "Response") +
  ggtitle("2AFC Task Results")


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
  scale_x_discrete(limits = c("symmetric", "asymmetric", "no_info")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/exp2_violin.pdf"),
       width = 8,
       height = 7.8)



## Stats

# Without all levels
d_filtered <- d %>%
  filter(next_interaction != "none" & relationship != "no_info")

mod <- lmer(likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 | subject_id),
            data = d_filtered)

summary(mod)

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
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


emm <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("all_relationships", "relationship", c("yes", "yes", "yes"))

emmeans(emm, pairwise ~ interaction_present | all_relationships)



##################################################

## Repeat all analyses with normalized values

d <-
  read.csv(here('data/exp2_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  mutate(newSum = select_if(., is.numeric) %>%
           reduce(`+`)) %>%
  mutate_if(is.numeric, list( ~ . / newSum)) %>%
  select(-newSum) %>%
  pivot_longer(
    cols = c("repeating", "alternating", "none"),
    names_to = "next_interaction",
    values_to = "likert_rating"
  ) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    next_interaction = fct_relevel(next_interaction,
                                   "repeating", "alternating", "none"),
    relationship = fct_relevel(relationship,
                               "symmetric", "asymmetric", "no_info")
  )



d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"))

# Without all levels
d_filtered <- d %>%
  filter(next_interaction != "none" & relationship != "no_info")

mod <- lmer(likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 | subject_id),
            data = d_filtered)

summary(mod)

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
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, pairwise ~ relationship_present | interaction_present)
emmeans(emm, pairwise ~ interaction_present | relationship_present)


emm <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("all_relationships", "relationship", c("yes", "yes", "yes"))

emmeans(emm, pairwise ~ interaction_present | all_relationships)
