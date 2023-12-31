library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(emmeans)
library(glue)

theme_set(theme_classic(base_size = 16))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

effort_diff <-
  read.csv(here("data/validation_effort_diff.csv")) %>% rename(effort_diff = diff) %>% select(c(story, effort_diff))
benefit_diff <-
  read.csv(here("data/validation_benefit_diff.csv")) %>% rename(benefit_diff = diff) %>% select(c(story, benefit_diff))

d <-
  read.csv(here('data/1c_data.csv')) %>% filter(understood == 'yes', story != 'attention') %>%
  mutate(
    strategy = case_when(
      stage == "second" & first_meeting == response ~ 'repeating',
      stage == "second" &
        first_meeting != response ~ 'alternating'
    )
  ) %>%
  select(-response, -understood, -first_meeting, -pass_attention) %>%
  mutate_all(~ case_when(. == 'less' ~ 'lower',
                         . == 'more' ~ 'higher',
                         TRUE ~ .)) %>%
  rename(first_actual_higher = altruistic_status,
         strategy_repeating = strategy) %>%
  group_by(subject_id, story) %>%
  fill(strategy_repeating, .direction = "up") %>%
  ungroup() %>%
  pivot_wider(names_from = stage,
              values_from = response_status,
              names_prefix = "response_higher_") %>%
  rename(first_response_higher = response_higher_first,
         second_response_higher = response_higher_second) %>%
  mutate(symmetric = ifelse(first_actual_higher == "equal", "symmetric", "asymmetric")) %>% 
  group_by(story) %>%
  left_join(effort_diff, id = story) %>%
  left_join(benefit_diff, id = story)

# Set levels for categorical variables
d$first_actual_higher <-
  factor(d$first_actual_higher, levels = c("lower", "higher", "equal"))
d$first_response_higher <-
  factor(d$first_response_higher, levels = c("lower", "higher", "equal"))
d$second_response_higher <-
  factor(d$second_response_higher, levels = c("higher", "lower", "equal"))
d$strategy_repeating <-
  factor(d$strategy_repeating, levels = c("alternating", "repeating"))



write.csv(d, here('data/tacit_full_tidy_data.csv'), row.names = FALSE)

# Deal with demographics csv
d.demographics <- read.csv(here('data/tacit_full_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age))

print(length(unique(d$subject_id)))


################## PLOTS


### Plot with scenario on x axis, implicit coordination on y axis, x axis ordered by y axis

d.first.response <-
  d %>%
  mutate(first_response_higher = recode(
    first_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(story, effort_diff, benefit_diff) %>%
  tidyboot_mean(first_response_higher, na.rm = TRUE)


d.temp <- d.first.response %>%
  arrange(desc(mean))

levs <- unique(d.temp$story)

d.first.response$story <-
  factor(d.first.response$story, levels = levs)



f <-
  ggplot(d.first.response, aes(x = story, y = empirical_stat)) +
  geom_point(
    data = d.first.response,
    aes(x = story, y = empirical_stat),
    size = 2.4,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = d.first.response,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1.8,
    width = 0.3
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "story", y = "expectation of higher", title = 'implicit coordination expectations') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

ggsave(
  here("figures/outputs/1c_scenarios_ordered.pdf"),
  width = 8,
  height = 3.5 
)


## Is implicit coordination based on effort/benefit?

f <-
  ggplot(d.first.response, aes(x = effort_diff, y = empirical_stat)) +
  geom_point(size = 2.4,
             alpha = 0.7,
             color = "#00BFC4", stroke = 0) +
  geom_errorbar(
    mapping = aes(x = effort_diff, ymin = ci_lower, ymax = ci_upper),
    size = 1.8,
    width = 0.07,
    alpha = 0.7,
    color = "#00BFC4"
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "perceived effort difference in scenario", y = "expectations for high status", title = "implicit coordination effort")

f

ggsave(here("figures/outputs/1c_effort_corr.pdf"),
       width = 8,
       height = 2.8)

f <-
  ggplot(d.first.response, aes(x = benefit_diff, y = empirical_stat)) +
  geom_point(size = 2.4,
             alpha = 0.7,
             color = "#F8766D", stroke = 0) +
  geom_errorbar(
    mapping = aes(x = benefit_diff, ymin = ci_lower, ymax = ci_upper),
    size = 1.8,
    width = 0.07,
    alpha = 0.7,
    color = "#F8766D"
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "perceived benefit difference in scenario", y = "expectations for high status", title = "implicit coordination benefit")


f

ggsave(here("figures/outputs/1c_benefit_corr.pdf"),
       width = 8,
       height = 2.8)


## First time on x axis, second time on y axis, colored by actual first

d.second.response <-
  d %>% filter(symmetric == "asymmetric") %>%
  mutate(
    second_response_higher = recode(
      second_response_higher,
      "higher" = 1,
      "lower" = -1
    )
  ) %>%
  group_by(story, first_actual_higher) %>%
  tidyboot_mean(second_response_higher, na.rm = TRUE)

d.first.second <-
  left_join(
    d.first.response,
    d.second.response,
    suffix = c("_first", "_second"),
    by = (c("story"))
  )

f <-
  ggplot(
    d.first.second,
    aes(x = empirical_stat_first, y = empirical_stat_second, color = first_actual_higher)
  ) +
  geom_smooth(method="lm", fill = "lightgray", linewidth = 1.3) +
  geom_point(size = 3.3, alpha = 0.3, stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_first, ymin = ci_lower_second, ymax = ci_upper_second),
    size = 1.5,
    width = 0.043,
    alpha = 0.3
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_second, xmin = ci_lower_first, xmax = ci_upper_first),
    size = 1.5,
    height = 0.043,
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("higher" = "#B87FFF", "lower" = "#35ACFF")) +
  labs(x = "first time higher", y = "second time higher", color = "observed_higher")

f

ggsave(here("figures/outputs/1c_first_second.pdf"),
       width = 6.75,
       height = 4)


## Are the effects of scenario consistent between 1b and 1c?

# import study 1b data
d.1b <- read.csv(here('data/1b_tidy_data.csv')) %>%
  filter(relationship != "equal", next_interaction != 'none') %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  ungroup() %>%
  rename(observed_higher = relationship) %>%
  mutate(
    observed_higher = ifelse(observed_higher == 'less', 'lower', 'higher'),
    second_response_higher = case_when(
      next_interaction == 'repeating' ~ observed_higher,
      next_interaction == 'alternating' ~ ifelse(observed_higher == "higher", "lower", "higher"),
      next_interaction == 'none' ~ 'none'
    )
  )


d.1b.means <- d.1b %>%
  filter(second_response_higher == 'higher') %>%
  group_by(story, observed_higher) %>%
  tidyboot_mean(normalized_likert_rating, na.rm = T)


d.1b.1c <-
  left_join(
    d.1b.means,
    d.first.response,
    suffix = c("_1b", "_1c"),
    by = (c("story"))
  )

f <-
  ggplot(d.1b.1c,
         aes(x = empirical_stat_1c, y = empirical_stat_1b, color = observed_higher)) +
  geom_point(size = 3.3, alpha = 0.3, stroke = 0) +
  geom_smooth(method="lm", fill = "lightgray", linewidth = 1.3) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1c, ymin = ci_lower_1b, ymax = ci_upper_1b),
    size = 1.5,
    width = 0.043,
    alpha = 0.3
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_1b, xmin = ci_lower_1c, xmax = ci_upper_1c),
    size = 1.5,
    height = 0.014,
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(0.2, 0.8)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("higher" = "#B87FFF", "lower" = "#35ACFF")) +
  labs(x = "study 1c first time higher", y = "study 1b P(higher)")

f

ggsave(here("figures/outputs/1c_1b.pdf"),
       width = 6.6,
       height = 4)


######################## STATS 

# Main preregistered hypothesis: in asymmetric relationships, people’s expectations for what
# happens the second time are explained by (1) expectations of tacit coordination
# (what they thought would happen the first time), and (2) expectations of precedent.

mod <- glmer(
  data = d %>% filter(symmetric == 'asymmetric'),
  second_response_higher ~ first_response_higher * first_actual_higher + (1 |
                                                                            subject_id) + (1 |
                                                                                             story),
  family =  'binomial'
)

summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: second_response_higher ~ first_response_higher * first_actual_higher +      (1 | subject_id) + (1 | story)
# Data: d %>% filter(symmetric == "asymmetric")
# 
# AIC      BIC   logLik deviance df.resid 
# 1859.3   1891.0   -923.7   1847.3     1434 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7892 -0.8961  0.5636  0.8536  1.6598 
# 
# Random effects:
#   Groups     Name        Variance  Std.Dev. 
# subject_id (Intercept) 2.144e-10 1.464e-05
# story      (Intercept) 5.123e-02 2.263e-01
# Number of obs: 1440, groups:  subject_id, 120; story, 18
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                  0.09519    0.07737   1.230    0.219    
# first_response_higher1                       0.53064    0.06194   8.567  < 2e-16 ***
#   first_actual_higher1                         0.31583    0.05605   5.635 1.75e-08 ***
#   first_response_higher1:first_actual_higher1 -0.03806    0.05617  -0.678    0.498    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) frst_r_1 frst_c_1
# frst_rspn_1 0.014                   
# frst_ctl_h1 0.005  0.068            
# frst__1:__1 0.060  0.011    0.028   
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


# H2: People expect alternation when the relationship is symmetric, and repetition when the relationship is asymmetric.

mod <- glmer(
  data = d,
  strategy_repeating ~ first_actual_higher + (1 |
                                                subject_id) + (1 | story),
  family =  'binomial'
)

summary(mod)

emmeans(mod, pairwise ~ first_actual_higher)

# first_actual_higher emmean    SE  df asymp.LCL asymp.UCL
# lower                0.469 0.194 Inf    0.0891     0.849
# higher               0.254 0.193 Inf   -0.1253     0.633
# equal               -1.276 0.199 Inf   -1.6661    -0.886
# 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

# contrast       estimate    SE  df z.ratio p.value
# lower - higher    0.215 0.120 Inf   1.791  0.1726
# lower - equal     1.745 0.131 Inf  13.291  <.0001
# higher - equal    1.530 0.130 Inf  11.802  <.0001
# 
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 3 estimates 

emm <- mod %>% emmeans(pairwise ~ first_actual_higher) %>%
  add_grouping("asymmetric",
               "first_actual_higher",
               c("yes", "yes", "no"))

emmeans(emm, pairwise ~ asymmetric)

# asymmetric emmean    SE  df asymp.LCL asymp.UCL
# no         -1.276 0.199 Inf -1.666147    -0.886
# yes         0.361 0.184 Inf  0.000683     0.722
# 
# Results are averaged over the levels of: first_actual_higher 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

# contrast estimate    SE  df z.ratio p.value
# no - yes    -1.64 0.116 Inf -14.141  <.0001
# 
# Results are averaged over the levels of: first_actual_higher 
# Results are given on the log odds ratio (not the response) scale. 


## H3: In asymmetric relationships, people have strong intuitions of tacit coordination.

mod <-
  glmer(
    data = d %>% filter(first_actual_higher != "equal"),
    first_response_higher ~ (1 |
                               subject_id) + (1 | story),
    family = 'binomial'
  )

summary(mod)

emm <- emmeans(mod, specs = ~ 1)
summary(emm, null = 0, infer = c(TRUE, TRUE))

# across scenarios, they average out
# 1       emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# overall 0.0187 0.266 Inf    -0.503      0.54   0.070  0.9440
# 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 


# H4: can people's expectations be predicted by relative cost / benefit? 

mod <-
  glmer(
    data = d %>% filter(first_actual_higher != "equal"),
    first_response_higher ~ effort_diff + (1 |
                                             subject_id) + (1 | story),
    family = 'binomial'
  )
summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: first_response_higher ~ effort_diff + (1 | subject_id) + (1 |      story)
# Data: d %>% filter(first_actual_higher != "equal")
# 
# AIC      BIC   logLik deviance df.resid 
# 1729.0   1750.1   -860.5   1721.0     1436 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5131 -0.6821  0.3896  0.6988  2.4894 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.1577   0.3971  
# story      (Intercept) 1.1660   1.0798  
# Number of obs: 1440, groups:  subject_id, 120; story, 18
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -0.2335     0.5566  -0.420    0.675
# effort_diff   0.1323     0.2571   0.515    0.607
# 
# Correlation of Fixed Effects:
#   (Intr)
# effort_diff -0.880

mod <-
  glmer(
    data = d %>% filter(first_actual_higher != "equal"),
    first_response_higher ~ benefit_diff + (1 |
                                              subject_id) + (1 | story),
    family = 'binomial'
  )
summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: first_response_higher ~ benefit_diff + (1 | subject_id) + (1 |      story)
# Data: d %>% filter(first_actual_higher != "equal")
# 
# AIC      BIC   logLik deviance df.resid 
# 1727.5   1748.6   -859.8   1719.5     1436 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5594 -0.6773  0.3846  0.7077  2.4170 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.1586   0.3983  
# story      (Intercept) 1.0684   1.0336  
# Number of obs: 1440, groups:  subject_id, 120; story, 18
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)    0.6571     0.5416   1.213    0.225
# benefit_diff  -0.3457     0.2591  -1.334    0.182
# 
# Correlation of Fixed Effects:
#   (Intr)
# benefit_dff -0.883




######################## EXTRA PLOTS

# Plot showing main hypothesis

h1.means <- d %>%
  filter(symmetric == 'asymmetric') %>%
  mutate(second_response_higher = recode(
    second_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(first_response_higher, first_actual_higher) %>%
  tidyboot_mean(second_response_higher, na.rm = T)

p1 <-
  ggplot(
    h1.means,
    aes(x = first_response_higher, y = empirical_stat, color = first_actual_higher)
  ) +
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
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "second response asymmetric relationships", y = "second_response_higher")

p1


# Do people expect repetition in asymmetric relationships, and alternating in asymmetric relationships? 

h2.means <- d %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(first_response_higher, first_actual_higher) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)


p2 <-
  ggplot(
    h2.means,
    aes(x = first_response_higher, y = empirical_stat, color = first_actual_higher)
  ) +
  geom_point(
    mapping = aes(x = first_response_higher, y = empirical_stat),
    size = 2.5,
    alpha = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    mapping = aes(x = first_response_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.3),
    size = 1.8,
    width = 0.12
  ) +
  scale_color_manual(
    values = wes_palette(n = 3, name = "Cavalcanti1"),
    name = "first_actual_higher",
    breaks = c("higher", "lower", "equal")
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(limits = c("higher", "lower", "equal")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "strategy (repeating vs alternating)", y = "strategy_repeating")

p2

# Strategy marginalized over first responses

h2.means.asym <- d %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(symmetric) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)

p3 <-
  ggplot(h2.means.asym, aes(x = symmetric, y = empirical_stat)) +
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
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "strategy marginalized over first responses", y = "strategy_repeating")

p3

h2.means.asym <- d %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(first_actual_higher) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)

p4 <-
  ggplot(h2.means.asym, aes(x = first_actual_higher, y = empirical_stat)) +
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
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "strategy marginalized over first responses", y = "strategy_repeating")

p4 

# What happens in equal status condition in 1b and 1c?
# Do they correspond with each other? 
# Does this correlate with first time predicted in 1c? 

# import study 1b data
d.1b.equal <- read.csv(here('data/1b_tidy_data.csv')) %>%
  filter(relationship == "equal", next_interaction != 'none') %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  ungroup() %>%
  rename(observed_higher = relationship)

d.1c.equal.means <- d %>% 
  filter(first_actual_higher == "equal") %>% 
  mutate(strategy_repeating = recode(
    strategy_repeating, 
    "repeating" = 1, 
    "alternating" = -1
  )) %>% 
  group_by(story) %>% 
  tidyboot_mean(strategy_repeating, na.rm = T)


d.1b.equal.means <- d.1b.equal %>%
  filter(next_interaction == "repeating") %>% 
  rename(strategy_repeating = next_interaction) %>% 
  group_by(story) %>%
  tidyboot_mean(normalized_likert_rating, na.rm = T)


d.1b.1c.equal <-
  left_join(
    d.1b.equal.means,
    d.first.response,
    suffix = c("_1b", "_1c"),
    by = (c("story"))
  )

d.1c.equal <-
  left_join(
    d.1c.equal.means,
    d.first.response,
    suffix = c("_2", "_1"),
    by = (c("story"))
  )


# Plots

f <-
  ggplot(d.1c.equal,
         aes(x = empirical_stat_1, y = empirical_stat_2)) +
  geom_point(size = 3.3, alpha = 0.7, stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1, ymin = ci_lower_2, ymax = ci_upper_2),
    size = 1.5,
    width = 0.043,
    alpha = 0.6
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_2, xmin = ci_lower_1, xmax = ci_upper_1),
    size = 1.5,
    height = 0.03,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "study 1c first time higher", y = "study 1c repeating expectation", title = "equal status")

f

f <-
  ggplot(d.1b.1c.equal,
         aes(x = empirical_stat_1c, y = empirical_stat_1b)) +
  geom_point(size = 3.3, alpha = 0.7, stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1c, ymin = ci_lower_1b, ymax = ci_upper_1b),
    size = 1.5,
    width = 0.043,
    alpha = 0.6
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_1b, xmin = ci_lower_1c, xmax = ci_upper_1c),
    size = 1.5,
    height = 0.014,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(0.2, 0.8)) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "study 1c first time higher", y = "study 1b P(repeating)", title = "equal status")

f



