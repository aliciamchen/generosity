library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
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
  read.csv(here('data/3c_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  rename(generous_status_second = altruistic_status_second) %>%
  pivot_longer(
    cols = c("annoyed", "satisfied"), 
    names_to = "response", 
    values_to = "likert_rating"
  ) %>% 
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>% 
  mutate(generous_status_second = case_when(
    generous_status_second == "more" ~ "higher",
    generous_status_second == "less" ~ "lower",
    TRUE ~ generous_status_second
  )) %>% 
  mutate(
    strategy = fct_relevel(strategy,
                           "repeating", "alternating"),
    generous_status_second = fct_relevel(generous_status_second,
                                           "higher", "lower", "equal", "just_met")
  )

write.csv(d, here('data/3c_tidy_data.csv'), row.names=FALSE)


# Demographics ------------------------------------------------------------

d.demographics <- read.csv(here('data/3c_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age),   min_age = min(age),
                             max_age = max(age))

print(length(unique(d$subject_id)))


# Plots -------------------------------------------------------------------

d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, generous_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 


# 3c for presentation
strategies = c("repeating", "alternating")


# annoyed
for (strat in strategies) {
  
  
  f = ggplot(data = d %>% filter(strategy == strat, response == "annoyed"),
             aes(x = strategy, y = likert_rating, fill = generous_status_second)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(strategy == strat, response == "annoyed"),
      mapping = aes(x = strategy, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(strategy == strat, response == "annoyed"),
      mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(values = relationship.colors,
                      name = "social relationship") +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "strategy", y = "how annoyed", fill = "social relationship") +
    theme(legend.position = "bottom")  
  
  print(f)
  
  ggsave(here(glue("figures/outputs/3c_annoyed_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}

# satisfied

for (strat in strategies) {
  
  f = ggplot(data = d %>% filter(strategy == strat, response == "satisfied"),
             aes(x = strategy, y = likert_rating, fill = generous_status_second)) +
    geom_violin(width = 1.4,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.all %>% filter(strategy == strat, response == "satisfied"),
      mapping = aes(x = strategy, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.all %>% filter(strategy == strat, response == "satisfied"),
      mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.06
    ) +
    scale_fill_manual(values = relationship.colors,
                      name = "social relationship") +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "strategy", y = "how satisfied", fill = "social relationship") +
    theme(legend.position = "bottom")  
  
  print(f)
  
  ggsave(here(glue("figures/outputs/3c_satisfied_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}

# Stats -------------------------------------------------------------------





# SATISFIED response --------------------------------------------------------

# Model with all levels of each factor
mod <-
  lmer(data = d %>% filter(response == "satisfied"),
       likert_rating ~ strategy * generous_status_second + (1 |
                                                              subject_id) + (1 | story))

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                                  Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# strategy                        2876.78 2876.78     1 2551.7 1272.329 < 2.2e-16 ***
# generous_status_second           111.79   37.26     3 2551.5   16.481 1.318e-10 ***
# strategy:generous_status_second  114.36   38.12     3 2551.3   16.859 7.637e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>% 
  summary(infer = T)

# $emmeans
# generous_status_second = higher:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.83 0.16 37.5     3.51     4.16  23.982  <.0001
# alternating   5.19 0.16 37.5     4.86     5.51  32.447  <.0001
# 
# generous_status_second = lower:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     2.85 0.16 37.5     2.53     3.18  17.845  <.0001
# alternating   5.20 0.16 37.5     4.88     5.52  32.541  <.0001
# 
# generous_status_second = equal:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.45 0.16 37.5     3.13     3.77  21.586  <.0001
# alternating   5.61 0.16 37.5     5.28     5.93  35.093  <.0001
# 
# generous_status_second = just_met:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.21 0.16 37.6     2.89     3.54  20.102  <.0001
# alternating   5.55 0.16 37.6     5.23     5.87  34.711  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# generous_status_second = higher:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -1.35 0.115 2551    -1.58    -1.13 -11.782  <.0001
# 
# generous_status_second = lower:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -2.35 0.115 2551    -2.57    -2.12 -20.450  <.0001
# 
# generous_status_second = equal:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -2.16 0.115 2551    -2.38    -1.93 -18.819  <.0001
# 
# generous_status_second = just_met:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -2.34 0.115 2552    -2.56    -2.11 -20.306  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 


# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# strategy_pairwise       generous_status_second_pairwise estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating higher - lower                    0.9938 0.162 2551    0.675    1.312   6.122  <.0001
# repeating - alternating higher - equal                    0.8050 0.162 2551    0.487    1.123   4.960  <.0001
# repeating - alternating higher - just_met                 0.9828 0.163 2551    0.664    1.302   6.046  <.0001
# repeating - alternating lower - equal                    -0.1889 0.162 2551   -0.507    0.129  -1.164  0.2444
# repeating - alternating lower - just_met                 -0.0111 0.162 2551   -0.330    0.308  -0.068  0.9457
# repeating - alternating equal - just_met                  0.1778 0.162 2551   -0.141    0.496   1.095  0.2737
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ generous_status_second * strategy) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

emmeans(emm, pairwise ~ asymmetric * strategy) %>% 
  summary(infer = T)

# $emmeans
# asymmetric strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# NA         repeating     3.21 0.160 37.6     2.89     3.54  20.102  <.0001
# no         repeating     3.45 0.160 37.5     3.13     3.77  21.586  <.0001
# yes        repeating     3.34 0.149 28.5     3.04     3.65  22.410  <.0001
# NA         alternating   5.55 0.160 37.6     5.23     5.87  34.711  <.0001
# no         alternating   5.61 0.160 37.5     5.28     5.93  35.093  <.0001
# yes        alternating   5.19 0.149 28.5     4.89     5.50  34.816  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                         estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA repeating - no repeating       -0.2346 0.1148 2551  -0.5621   0.0929  -2.043  0.3182
# NA repeating - yes repeating      -0.1285 0.0996 2551  -0.4124   0.1555  -1.290  0.7907
# NA repeating - NA alternating     -2.3359 0.1150 2552  -2.6640  -2.0078 -20.306  <.0001
# NA repeating - no alternating     -2.3927 0.1149 2551  -2.7203  -2.0651 -20.830  <.0001
# NA repeating - yes alternating    -1.9785 0.0996 2552  -2.2625  -1.6945 -19.869  <.0001
# no repeating - yes repeating       0.1062 0.0994 2551  -0.1773   0.3896   1.068  0.8940
# no repeating - NA alternating     -2.1013 0.1149 2551  -2.4288  -1.7737 -18.296  <.0001
# no repeating - no alternating     -2.1581 0.1147 2551  -2.4851  -1.8310 -18.819  <.0001
# no repeating - yes alternating    -1.7439 0.0994 2551  -2.0273  -1.4604 -17.547  <.0001
# yes repeating - NA alternating    -2.2074 0.0996 2552  -2.4915  -1.9234 -22.165  <.0001
# yes repeating - no alternating    -2.2642 0.0994 2551  -2.5477  -1.9808 -22.779  <.0001
# yes repeating - yes alternating   -1.8500 0.0812 2551  -2.0816  -1.6185 -22.787  <.0001
# NA alternating - no alternating   -0.0568 0.1148 2551  -0.3843   0.2707  -0.495  0.9964
# NA alternating - yes alternating   0.3574 0.0996 2552   0.0734   0.6414   3.589  0.0046
# no alternating - yes alternating   0.4142 0.0994 2551   0.1308   0.6976   4.168  0.0005
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 6 estimates 
# P value adjustment: tukey method for comparing a family of 6 estimates 



contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# asymmetric_pairwise strategy_pairwise       estimate    SE   df lower.CL upper.CL t.ratio p.value
# NA - no             repeating - alternating   -0.178 0.162 2551   -0.496   0.1407  -1.095  0.2737
# NA - yes            repeating - alternating   -0.486 0.141 2551   -0.762  -0.2098  -3.451  0.0006
# no - yes            repeating - alternating   -0.308 0.140 2551   -0.584  -0.0326  -2.193  0.0284
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 



# Redo without the just met condition -------------------------------------


mod <-
  lmer(
    data = d %>% filter(response == "satisfied", generous_status_second != "just_met"),
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
               c("yes", "yes", "no"))

emmeans(emm, pairwise ~ asymmetric * strategy) %>% 
  summary(infer = T)


contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)



# ANNOYED response --------------------------------------------------------

# Model with all levels of each factor
mod <-
  lmer(data = d %>% filter(response == "annoyed"),
       likert_rating ~ strategy * generous_status_second + (1 |
                                                              subject_id) + (1 | story))

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                                  Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# strategy                        1656.23 1656.23     1 2554.3 1019.876 < 2.2e-16 ***
# generous_status_second            94.70   31.57     3 2554.4   19.439 1.847e-12 ***
# strategy:generous_status_second   74.29   24.76     3 2554.2   15.250 7.784e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>% 
  summary(infer = T)

# $emmeans
# generous_status_second = higher:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     2.59 0.13 35.8     2.33     2.86  19.866  <.0001
# alternating   1.53 0.13 35.8     1.26     1.79  11.715  <.0001
# 
# generous_status_second = lower:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.54 0.13 35.8     3.27     3.80  27.096  <.0001
# alternating   1.56 0.13 35.8     1.30     1.83  11.984  <.0001
# 
# generous_status_second = equal:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     2.90 0.13 35.8     2.63     3.16  22.195  <.0001
# alternating   1.39 0.13 35.8     1.12     1.65  10.640  <.0001
# 
# generous_status_second = just_met:
#   strategy    emmean   SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.11 0.13 35.8     2.84     3.37  23.816  <.0001
# alternating   1.44 0.13 35.8     1.17     1.70  11.011  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# generous_status_second = higher:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating     1.06 0.0973 2554    0.872     1.25  10.923  <.0001
# 
# generous_status_second = lower:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating     1.97 0.0973 2554    1.782     2.16  20.281  <.0001
# 
# generous_status_second = equal:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating     1.51 0.0972 2554    1.317     1.70  15.509  <.0001
# 
# generous_status_second = just_met:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating     1.67 0.0973 2554    1.480     1.86  17.166  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 


# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# strategy_pairwise       generous_status_second_pairwise estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating higher - lower                    -0.910 0.138 2554   -1.180   -0.641  -6.618  <.0001
# repeating - alternating higher - equal                    -0.445 0.137 2554   -0.714   -0.175  -3.236  0.0012
# repeating - alternating higher - just_met                 -0.608 0.138 2554   -0.878   -0.339  -4.422  <.0001
# repeating - alternating lower - equal                      0.465 0.137 2554    0.196    0.735   3.385  0.0007
# repeating - alternating lower - just_met                   0.302 0.138 2554    0.032    0.572   2.193  0.0284
# repeating - alternating equal - just_met                  -0.164 0.138 2554   -0.433    0.106  -1.189  0.2344
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95


# Check asymmetric/symmetric expectations
emm <-
  mod %>% emmeans(pairwise ~ generous_status_second * strategy) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

emmeans(emm, pairwise ~ asymmetric * strategy) %>% 
  summary(infer = T)

# $emmeans
# asymmetric strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# NA         repeating     3.11 0.130 35.8     2.84     3.37  23.816  <.0001
# no         repeating     2.90 0.130 35.8     2.63     3.16  22.195  <.0001
# yes        repeating     3.06 0.121 26.6     2.81     3.31  25.305  <.0001
# NA         alternating   1.44 0.130 35.8     1.17     1.70  11.011  <.0001
# no         alternating   1.39 0.130 35.8     1.12     1.65  10.640  <.0001
# yes        alternating   1.55 0.121 26.6     1.30     1.79  12.770  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                         estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA repeating - no repeating        0.2126 0.0973 2554  -0.0647   0.4900   2.186  0.2443
# NA repeating - yes repeating       0.0443 0.0843 2554  -0.1961   0.2847   0.525  0.9952
# NA repeating - NA alternating      1.6708 0.0973 2554   1.3933   1.9484  17.166  <.0001
# NA repeating - no alternating      1.7199 0.0973 2554   1.4425   1.9973  17.682  <.0001
# NA repeating - yes alternating     1.5618 0.0843 2554   1.3214   1.8022  18.527  <.0001
# no repeating - yes repeating      -0.1684 0.0842 2554  -0.4085   0.0718  -1.999  0.3428
# no repeating - NA alternating      1.4582 0.0973 2554   1.1808   1.7356  14.993  <.0001
# no repeating - no alternating      1.5073 0.0972 2554   1.2301   1.7844  15.509  <.0001
# no repeating - yes alternating     1.3491 0.0842 2554   1.1089   1.5893  16.018  <.0001
# yes repeating - NA alternating     1.6266 0.0843 2554   1.3862   1.8669  19.298  <.0001
# yes repeating - no alternating     1.6756 0.0842 2554   1.4354   1.9158  19.896  <.0001
# yes repeating - yes alternating    1.5175 0.0688 2554   1.3213   1.7136  22.064  <.0001
# NA alternating - no alternating    0.0491 0.0973 2554  -0.2283   0.3264   0.504  0.9960
# NA alternating - yes alternating  -0.1091 0.0843 2554  -0.3495   0.1313  -1.294  0.7885
# no alternating - yes alternating  -0.1581 0.0842 2554  -0.3983   0.0820  -1.878  0.4162
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 6 estimates 
# P value adjustment: tukey method for comparing a family of 6 estimates 


contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# asymmetric_pairwise strategy_pairwise       estimate    SE   df lower.CL upper.CL t.ratio p.value
# NA - no             repeating - alternating   0.1636 0.138 2554  -0.1061    0.433   1.189  0.2344
# NA - yes            repeating - alternating   0.1534 0.119 2554  -0.0804    0.387   1.287  0.1983
# no - yes            repeating - alternating  -0.0102 0.119 2554  -0.2437    0.223  -0.086  0.9315
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 


# Redo without the just met condition -------------------------------------


mod <-
  lmer(
    data = d %>% filter(response == "annoyed", generous_status_second != "just_met"),
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
               c("yes", "yes", "no"))

emmeans(emm, pairwise ~ asymmetric * strategy) %>% 
  summary(infer = T)


contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)



# Extra -------------------------------------------------------------------

# All conditions on one plot
f = ggplot(data = d %>% filter(response == "annoyed"),
           aes(x = strategy, y = likert_rating, fill = generous_status_second)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = strategy, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(response == "annoyed"),
    mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  scale_fill_manual(values = relationship.colors,
                    name = "social relationship") +
  labs(title = "study 3c", x = "status of second time generous person (A)", y = "how annoyed was A?") +
  theme(legend.position = "bottom")

f


f = ggplot(data = d %>% filter(response == "satisfied"),
           aes(x = strategy, y = likert_rating, fill = generous_status_second)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = strategy, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(response == "satisfied"),
    mapping = aes(x = strategy, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  scale_fill_manual(values = relationship.colors,
                    name = "social relationship") +
  labs(title = "study 3c", x = "status of second time generous person (A)", y = "how satisfied was A with outcome?") +
  theme(legend.position = "bottom")

f