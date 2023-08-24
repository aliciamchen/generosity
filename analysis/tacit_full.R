library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(emmeans)

theme_set(theme_classic(base_size = 20))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

# 
d <-
  read.csv(here('data/tacit_full_data.csv'))

d <-
  read.csv(here('data/tacit_full_data.csv')) %>% filter(understood == 'yes', story != 'attention') %>% 
  mutate(strategy = case_when(stage == "second" & first_meeting == response ~ 'repeating',
                              stage == "second" & first_meeting != response ~ 'alternating')) %>%
  select(-response,-understood,-first_meeting,-pass_attention) %>%
  mutate_all( ~ case_when(. == 'less' ~ 'lower',
                          . == 'more' ~ 'higher',
                          TRUE ~ .)) %>%
  rename(first_actual_higher = altruistic_status, strategy_repeating = strategy) %>%
  group_by(subject_id, story) %>% 
  fill(strategy_repeating, .direction = "up") %>% 
  ungroup() %>% 
  pivot_wider(names_from = stage,
              values_from = response_status,
              names_prefix = "response_higher_") %>%
  rename(first_response_higher = response_higher_first,
         second_response_higher = response_higher_second) %>%
  mutate(
    symmetric = ifelse(first_actual_higher == "equal", "symmetric", "asymmetric")
  )  

# Set levels for categorical variables
d$first_actual_higher <-
  factor(d$first_actual_higher, levels = c("higher", "equal", "lower"))
d$first_response_higher <-
  factor(d$first_response_higher, levels = c("higher", "equal", "lower"))
d$second_response_higher <-
  factor(d$second_response_higher, levels = c("higher", "equal", "lower"))
d$strategy_repeating <- 
  factor(d$strategy_repeating, levels = c("repeating", "alternating"))


write.csv(d, here('data/tacit_full_tidy_data.csv'), row.names = FALSE)

# Deal with demographics csv
d.demographics <- read.csv(here('data/tacit_full_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))

## Stats


# Main hypothesis: in asymmetric relationships, people’s expectations for what 
# happens the second time are explained by (1) expectations of tacit coordination 
# (what they thought would happen the first time), and (2) expectations of precedent.

mod <- glmer(data = d %>% filter(symmetric == 'asymmetric'),
  second_response_higher ~ first_response_higher * first_actual_higher + (1 |
                                                                            subject_id) + (1 | story),
  family =  'binomial'
)

summary(mod)

# Plot

h1.means <- d %>% 
  filter(symmetric == 'asymmetric') %>%
  mutate(second_response_higher = recode(second_response_higher, "higher" = 1, "lower" = -1)) %>% 
  group_by(first_response_higher, first_actual_higher) %>% 
  tidyboot_mean(second_response_higher, na.rm = T)

p1 <- ggplot(h1.means, aes(x = first_response_higher, y = empirical_stat, color = first_actual_higher)) +
  geom_point(
    mapping = aes(x = first_response_higher, y = empirical_stat),
    size = 2.5,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = first_response_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 2.2,
    width = 0
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "second response asymmetric relationships", y = "second_response_higher") 

p1
ggsave(here("figures/sloan_talk/study3_main.pdf"),
       width = 6.2,
       height = 4)
# first_response_higher = recode(first_response_higher, "higher" = 1, "lower" = 0), 
# first_actual_higher = recode(first_actual_higher, "higher" = 1, "lower" = 0), 

## Secondary hypotheses

# H2: People expect alternation when the relationship is symmetric, and repetition when the relationship is asymmetric.

mod <- glmer(data = d, 
             strategy_repeating ~ first_actual_higher + (1 | subject_id) + (1 | story),
             family =  'binomial'
)

summary(mod)

emmeans(mod, pairwise ~ first_actual_higher)

emm <- mod %>% emmeans(pairwise ~ first_actual_higher) %>% 
  add_grouping("asymmetric",
               "first_actual_higher",
               c("yes", "no", "yes"))

emmeans(emm, pairwise ~ asymmetric)


# Plot for H2


h2.means <- d %>% 
  mutate(strategy_repeating = recode(strategy_repeating, "repeating" = 1, "alternating" = -1)) %>% 
  group_by(first_response_higher, first_actual_higher) %>% 
  tidyboot_mean(strategy_repeating, na.rm = T)


# This is the plot for the cogsci poster
p2 <- ggplot(h2.means, aes(x = first_actual_higher, y = empirical_stat, color = first_response_higher)) +
  geom_point(
    mapping = aes(x = first_actual_higher, y = empirical_stat),
    size = 2.5,
    alpha = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    mapping = aes(x = first_actual_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.3),
    size = 1.8,
    width = 0.12
  ) +
  scale_color_manual(
    values = wes_palette(n = 3, name = "Cavalcanti1"), 
    name = "first_response_higher",
    breaks = c("higher", "lower", "equal")) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(limits = c("higher", "lower", "equal")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "strategy (repeating vs alternating)", y = "strategy_repeating") 

p2

# ggsave(here("figures/tacit_cogsci_poster.pdf"), width = 7, height = 4)

h2.means.asym <- d %>% 
  mutate(strategy_repeating = recode(strategy_repeating, "repeating" = 1, "alternating" = -1)) %>% 
  group_by(symmetric) %>% 
  tidyboot_mean(strategy_repeating, na.rm = T)
  
p3 <- ggplot(h2.means.asym, aes(x = symmetric, y = empirical_stat)) +
  geom_point(
    mapping = aes(x = symmetric, y = empirical_stat),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = symmetric, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "strategy marginalized over first responses", y = "strategy_repeating") 

p3


h2.means.asym <- d %>% 
  mutate(strategy_repeating = recode(strategy_repeating, "repeating" = 1, "alternating" = -1)) %>% 
  group_by(first_actual_higher) %>% 
  tidyboot_mean(strategy_repeating, na.rm = T)

p4 <- ggplot(h2.means.asym, aes(x = first_actual_higher, y = empirical_stat)) +
  geom_point(
    mapping = aes(x = first_actual_higher, y = empirical_stat),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = first_actual_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "strategy marginalized over first responses", y = "strategy_repeating") 

p4 # Interesting ... the stats say that the only diff here is that higher is different from equal

## H3: In asymmetric relationships, people have strong intuitions of tacit coordination. 

mod <- glmer(data = d %>% filter(first_actual_higher != "equal"), first_response_higher ~ (1 | subject_id) + (1 | story), family = 'binomial')

summary(mod)

emm <- emmeans(mod, specs = ~ 1)
summary(emm, null = 0, infer = c(TRUE, TRUE))

stage.one.mean <-
  d %>% filter(first_actual_higher != "equal") %>% 
  mutate(first_response_higher = recode(first_response_higher, "higher" = 1, "lower" = -1)) %>% 
  tidyboot_mean(first_response_higher, na.rm = TRUE)

f = ggplot(data = stage.one.mean, aes(x = 'first time', y = 2)) +
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
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "expectations for high status", title = "asymmetric relationship first meeting") 


f

# H4: relative cost / benefit

effort_diff <- read.csv(here("data/validation_effort_diff.csv")) %>% rename(effort_diff = diff) %>% select(c(story, effort_diff))
benefit_diff <- read.csv(here("data/validation_benefit_diff.csv")) %>% rename(benefit_diff = diff) %>% select(c(story, benefit_diff))

d.with.diffs <- d %>% group_by(story) %>% 
  left_join(effort_diff, id = story) %>% 
  left_join(benefit_diff, id = story)

mod <- glmer(data = d.with.diffs %>% filter(first_actual_higher != "equal"), first_response_higher ~ effort_diff + (1 | subject_id) + (1 | story), family = 'binomial')
summary(mod)

mod <- glmer(data = d.with.diffs %>% filter(first_actual_higher != "equal"), first_response_higher ~ benefit_diff + (1 | subject_id) + (1 | story), family = 'binomial')
summary(mod)


# H4 plots
stage.one.mean.story <- 
  d.with.diffs %>% 
  mutate(first_response_higher = recode(first_response_higher, "higher" = 1, "lower" = -1)) %>% 
  group_by(story, effort_diff, benefit_diff) %>% 
  tidyboot_mean(first_response_higher, na.rm = TRUE) 


f <- ggplot(stage.one.mean.story, aes(x = effort_diff, y = empirical_stat)) + 
  geom_point(size = 3.3, alpha = 0.7, color = "#00BFC4") + 
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 2.5, width = 0, alpha = 0.7, color = "#00BFC4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "perceived effort difference in scenario", y = "expectations for high status", title = "asymmetric relationships first meeting")

f

ggsave(here("figures/sloan_talk/study3_effort_corr.pdf"),
       width = 6.2,
       height = 4)

f <- ggplot(stage.one.mean.story, aes(x = benefit_diff, y = empirical_stat)) + 
  geom_point(size = 3.3, alpha = 0.7, color = "#F8766D") + 
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 2.5, width = 0, alpha = 0.7, color = "#F8766D") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "perceived benefit difference in scenario", y = "expectations for high status", title = "asymmetric relationships first meeting")


f

ggsave(here("figures/sloan_talk/study3_benefit_corr.pdf"),
       width = 6.2,
       height = 4)

f <- ggplot(stage.one.mean.story, aes(x = effort_diff, y = benefit_diff)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = lm) +
  labs(x = "perceived effort difference", y = "perceived benefit difference")

f








###### BELOW IS OLD STUFF






glmer(first_response_higher ~ benefit_diff + (1 | participant) + (1 | scenario), family = 'binomial')


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

# Stage 1 stats
m <- glmer(data = d %>% filter(stage == "first"), factor(response_status) ~ symmetry + (1 | subject_id) + (1 | story), family = "binomial")
emm <- emmeans(m, specs = ~ 1)
summary(emm, null = 0, infer = c(TRUE, TRUE))
# m <- lmer(data = stage.one, response_status_ ~ (1 | subject_id) + (1 | story))

summary(m)
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
