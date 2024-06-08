library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(glue)


# Options -----------------------------------------------------------------

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

relationship.colors <- c("#B87FFF", "#35ACFF", "#00A08A", "#D6D6D6")

# Load data ---------------------------------------------------------------

d <-
  read.csv(here('data/3a_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  rename(likert_rating = response, 
         generous_status_second = altruistic_status_second) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    generous_status_second = case_when(
      generous_status_second == "more" ~ "higher",
      generous_status_second == "less" ~ "lower",
      TRUE ~ generous_status_second
    )
  ) %>%
  mutate(
    strategy = fct_relevel(strategy,
                           "repeating", "alternating"),
    generous_status_second = fct_relevel(
      generous_status_second,
      "higher",
      "lower",
      "equal",
      "just_met"
    )
  )


write.csv(d, here('data/3a_tidy_data.csv'), row.names = FALSE)


# Demographics ------------------------------------------------------------


d.demographics <- read.csv(here('data/3a_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>%
  summarize(
    mean_age = mean(age, na.rm = T),
    sd_age = sd(age),
    min_age = min(age),
    max_age = max(age)
  )

print(length(unique(d$subject_id)))


# Plots -------------------------------------------------------------------


d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, generous_status_second) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


strategies = c("repeating", "alternating")

for (strat in strategies) {
  f = ggplot(
    data = d %>% filter(strategy == strat),
    aes(x = strategy, y = likert_rating, fill = generous_status_second)
  ) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(strategy == strat),
      mapping = aes(x = strategy, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(strategy == strat),
      mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(values = relationship.colors,
                      name = "social relationship") +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "strategy", y = "how moral", fill = "social relationship") +
    theme(legend.position = "bottom")
  
  f
  
  ggsave(here(glue("figures/outputs/3a_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}


# Stats -------------------------------------------------------------------


# Model with all levels of each factor
mod <-
  lmer(data = d,
       likert_rating ~ strategy * generous_status_second + (1 |
                                                              subject_id) + (1 | story))

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                                  Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# strategy                        1787.56 1787.56     1 2567.7 1472.493 < 2.2e-16 ***
# generous_status_second            84.28   28.09     3 2568.1   23.142 8.870e-15 ***
# strategy:generous_status_second   65.20   21.73     3 2569.0   17.903 1.691e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>% 
  summary(infer = T)

# $emmeans
# generous_status_second = higher:
#   strategy    emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating     4.21 0.0773 115     4.06     4.36  54.475  <.0001
# alternating   5.34 0.0773 115     5.18     5.49  69.040  <.0001
# 
# generous_status_second = lower:
#   strategy    emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating     3.52 0.0772 115     3.36     3.67  45.510  <.0001
# alternating   5.08 0.0773 115     4.92     5.23  65.649  <.0001
# 
# generous_status_second = equal:
#   strategy    emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating     3.69 0.0773 115     3.54     3.84  47.740  <.0001
# alternating   5.59 0.0773 115     5.44     5.74  72.306  <.0001
# 
# generous_status_second = just_met:
#   strategy    emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating     3.64 0.0772 115     3.49     3.80  47.181  <.0001
# alternating   5.50 0.0773 115     5.34     5.65  71.075  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# generous_status_second = higher:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -1.13 0.0839 2568    -1.29   -0.962 -13.419  <.0001
# 
# generous_status_second = lower:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -1.56 0.0839 2569    -1.73   -1.396 -18.604  <.0001
# 
# generous_status_second = equal:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -1.90 0.0839 2569    -2.06   -1.734 -22.621  <.0001
# 
# generous_status_second = just_met:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -1.85 0.0839 2569    -2.02   -1.687 -22.072  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 


# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# strategy_pairwise       generous_status_second_pairwise estimate    SE   df t.ratio p.value
# repeating - alternating higher - lower                    0.4345 0.119 2569   3.661  0.0003
# repeating - alternating higher - equal                    0.7727 0.119 2569   6.509  <.0001
# repeating - alternating higher - just_met                 0.7255 0.119 2569   6.113  <.0001
# repeating - alternating lower - equal                     0.3382 0.119 2570   2.848  0.0044
# repeating - alternating lower - just_met                  0.2909 0.119 2570   2.452  0.0143
# repeating - alternating equal - just_met                 -0.0472 0.119 2569  -0.398  0.6907
# 
# Degrees-of-freedom method: kenward-roger 


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ generous_status_second * strategy) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

emmeans(emm, pairwise ~ asymmetric * strategy) %>% 
  summary(infer = T)

# $emmeans
# asymmetric strategy    emmean     SE    df lower.CL upper.CL t.ratio p.value
# NA         repeating     3.64 0.0772 114.6     3.49     3.80  47.181  <.0001
# no         repeating     3.69 0.0773 115.0     3.54     3.84  47.740  <.0001
# yes        repeating     3.86 0.0649  57.6     3.73     3.99  59.519  <.0001
# NA         alternating   5.50 0.0773 115.0     5.34     5.65  71.075  <.0001
# no         alternating   5.59 0.0773 115.0     5.44     5.74  72.306  <.0001
# yes        alternating   5.21 0.0649  57.7     5.08     5.34  80.184  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                         estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA repeating - no repeating       -0.0469 0.0839 2569  -0.2861   0.1923  -0.559  0.9935
# NA repeating - yes repeating      -0.2191 0.0726 2568  -0.4261  -0.0121  -3.018  0.0308
# NA repeating - NA alternating     -1.8517 0.0839 2569  -2.0909  -1.6124 -22.072  <.0001
# NA repeating - no alternating     -1.9458 0.0839 2568  -2.1850  -1.7067 -23.204  <.0001
# NA repeating - yes alternating    -1.5626 0.0726 2568  -1.7696  -1.3555 -21.520  <.0001
# no repeating - yes repeating      -0.1722 0.0727 2569  -0.3795   0.0351  -2.369  0.1677
# no repeating - NA alternating     -1.8048 0.0840 2569  -2.0442  -1.5653 -21.497  <.0001
# no repeating - no alternating     -1.8989 0.0839 2569  -2.1383  -1.6595 -22.621  <.0001
# no repeating - yes alternating    -1.5157 0.0727 2568  -1.7229  -1.3084 -20.853  <.0001
# yes repeating - NA alternating    -1.6326 0.0727 2568  -1.8398  -1.4253 -22.465  <.0001
# yes repeating - no alternating    -1.7267 0.0727 2568  -1.9340  -1.5195 -23.764  <.0001
# yes repeating - yes alternating   -1.3435 0.0593 2568  -1.5127  -1.1743 -22.645  <.0001
# NA alternating - no alternating   -0.0941 0.0839 2569  -0.3335   0.1452  -1.122  0.8726
# NA alternating - yes alternating   0.2891 0.0727 2569   0.0817   0.4965   3.976  0.0010
# no alternating - yes alternating   0.3833 0.0727 2569   0.1759   0.5906   5.272  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 6 estimates 
# P value adjustment: tukey method for comparing a family of 6 estimates 


contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# asymmetric_pairwise strategy_pairwise       estimate    SE   df lower.CL upper.CL t.ratio p.value
# NA - no             repeating - alternating   0.0472 0.119 2569   -0.185    0.280   0.398  0.6907
# NA - yes            repeating - alternating  -0.5082 0.103 2569   -0.710   -0.307  -4.945  <.0001
# no - yes            repeating - alternating  -0.5554 0.103 2569   -0.757   -0.354  -5.402  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 



# Redo without the just met condition -------------------------------------


mod <-
  lmer(
    data = d %>% filter(generous_status_second != "just_met"),
    likert_rating ~ strategy * generous_status_second + (1 |
                                                             subject_id) + (1 | story)
  )

summary(mod)

anova(mod, type = "III")

# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>% 
  summary(infer = T)


# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ generous_status_second * strategy) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

emmeans(emm, pairwise ~ asymmetric * strategy) %>% 
  summary(infer = T)


contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)


# Extra plots -------------------------------------------------------------

# all conditions on one plot
f = ggplot(data = d,
           aes(x = strategy, y = likert_rating, fill = generous_status_second)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = strategy, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "strategy", y = "how moral?") +
  theme(legend.position = "bottom")

f


# grouped by story

d.means.all <-
  d %>% drop_na() %>%
  group_by(story, strategy, generous_status_second) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


f = ggplot(data = d,
           aes(x = generous_status_second, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = generous_status_second, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = generous_status_second, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of second time generous person", y = "how moral?") +
  theme(legend.position = "bottom") +
  facet_wrap( ~ story)

f
