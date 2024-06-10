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
  read.csv(here('data/3d_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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

write.csv(d, here('data/3d_tidy_data.csv'), row.names=FALSE)


# Demographics ------------------------------------------------------------


d.demographics <- read.csv(here('data/3d_demographics.csv'))
print(length(unique(d.demographics$subject_id)))

d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age),  min_age = min(age),
                             max_age = max(age))

print(length(unique(d$subject_id)))


# Plots -------------------------------------------------------------------


d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, generous_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) 


strategies = c("repeating", "alternating")

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
  
  ggsave(here(glue("figures/3d_satisfied_{strat}.pdf")),
         width = 4.3,
         height = 7.5)
  
}

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
  
  ggsave(here(glue("figures/outputs/3d_annoyed_{strat}.pdf")),
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
#                                 Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# strategy                        457.72  457.72     1 2608.0 226.4777 < 2.2e-16 ***
# generous_status_second           47.04   15.68     3 2608.1   7.7578 3.713e-05 ***
# strategy:generous_status_second  11.20    3.73     3 2608.1   1.8467    0.1365    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>% 
  summary(infer = T)

# $emmeans
# generous_status_second = higher:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     4.95 0.161 32.8     4.62     5.27  30.692  <.0001
# alternating   5.75 0.161 32.7     5.42     6.08  35.692  <.0001
# 
# generous_status_second = lower:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     4.97 0.161 32.7     4.64     5.29  30.823  <.0001
# alternating   5.58 0.161 32.8     5.25     5.91  34.602  <.0001
# 
# generous_status_second = equal:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     5.13 0.161 32.7     4.80     5.45  31.815  <.0001
# alternating   6.09 0.161 32.8     5.76     6.41  37.768  <.0001
# 
# generous_status_second = just_met:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     5.07 0.161 32.9     4.74     5.40  31.411  <.0001
# alternating   5.92 0.161 32.8     5.60     6.25  36.756  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# generous_status_second = higher:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   -0.804 0.107 2608   -1.014   -0.594  -7.495  <.0001
# 
# generous_status_second = lower:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   -0.613 0.107 2608   -0.823   -0.402  -5.706  <.0001
# 
# generous_status_second = equal:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   -0.961 0.107 2608   -1.172   -0.751  -8.961  <.0001
# 
# generous_status_second = just_met:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   -0.855 0.108 2609   -1.066   -0.643  -7.936  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 



# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# strategy_pairwise       generous_status_second_pairwise estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating higher - lower                   -0.1913 0.152 2608  -0.4890    0.106  -1.261  0.2075
# repeating - alternating higher - equal                    0.1572 0.152 2608  -0.1402    0.455   1.037  0.3000
# repeating - alternating higher - just_met                 0.0506 0.152 2609  -0.2475    0.349   0.333  0.7393
# repeating - alternating lower - equal                     0.3486 0.152 2608   0.0510    0.646   2.297  0.0217
# repeating - alternating lower - just_met                  0.2419 0.152 2609  -0.0562    0.540   1.591  0.1117
# repeating - alternating equal - just_met                 -0.1066 0.152 2608  -0.4047    0.191  -0.701  0.4831
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
# NA         repeating     5.07 0.161 32.9     4.74     5.40  31.411  <.0001
# no         repeating     5.13 0.161 32.7     4.80     5.45  31.815  <.0001
# yes        repeating     4.96 0.152 25.9     4.64     5.27  32.618  <.0001
# NA         alternating   5.92 0.161 32.8     5.60     6.25  36.756  <.0001
# no         alternating   6.09 0.161 32.8     5.76     6.41  37.768  <.0001
# yes        alternating   5.66 0.152 25.9     5.35     5.98  37.275  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                         estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA repeating - no repeating       -0.0564 0.1076 2609  -0.3633    0.250  -0.524  0.9952
# NA repeating - yes repeating       0.1131 0.0933 2609  -0.1531    0.379   1.212  0.8312
# NA repeating - NA alternating     -0.8546 0.1077 2609  -1.1617   -0.547  -7.936  <.0001
# NA repeating - no alternating     -1.0176 0.1077 2609  -1.3247   -0.710  -9.449  <.0001
# NA repeating - yes alternating    -0.5952 0.0934 2609  -0.8615   -0.329  -6.375  <.0001
# no repeating - yes repeating       0.1695 0.0928 2608  -0.0953    0.434   1.826  0.4493
# no repeating - NA alternating     -0.7982 0.1073 2608  -1.1041   -0.492  -7.441  <.0001
# no repeating - no alternating     -0.9612 0.1073 2608  -1.2671   -0.655  -8.961  <.0001
# no repeating - yes alternating    -0.5388 0.0929 2608  -0.8037   -0.274  -5.801  <.0001
# yes repeating - NA alternating    -0.9677 0.0929 2608  -1.2328   -0.703 -10.411  <.0001
# yes repeating - no alternating    -1.1307 0.0930 2608  -1.3958   -0.866 -12.164  <.0001
# yes repeating - yes alternating   -0.7083 0.0759 2608  -0.9247   -0.492  -9.334  <.0001
# NA alternating - no alternating   -0.1630 0.1074 2608  -0.4692    0.143  -1.518  0.6526
# NA alternating - yes alternating   0.2594 0.0930 2608  -0.0058    0.525   2.789  0.0594
# no alternating - yes alternating   0.4224 0.0930 2608   0.1572    0.688   4.543  0.0001
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
# strategy                        204.476 204.476     1 2610.6 200.5971 < 2.2e-16 ***
# generous_status_second           17.361   5.787     3 2610.5   5.6773 0.0007135 ***
# strategy:generous_status_second   0.674   0.225     3 2610.5   0.2205 0.8822193    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>% 
  summary(infer = T)

# $emmeans
# generous_status_second = higher:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     1.93 0.111 33.0     1.70     2.15  17.350  <.0001
# alternating   1.34 0.111 32.8     1.11     1.56  12.048  <.0001
# 
# generous_status_second = lower:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     1.97 0.111 33.0     1.75     2.20  17.777  <.0001
# alternating   1.44 0.111 32.9     1.21     1.66  12.955  <.0001
# 
# generous_status_second = equal:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     1.76 0.111 32.8     1.54     1.99  15.887  <.0001
# alternating   1.24 0.111 32.9     1.01     1.46  11.147  <.0001
# 
# generous_status_second = just_met:
#   strategy    emmean    SE   df lower.CL upper.CL t.ratio p.value
# repeating     1.80 0.111 32.9     1.58     2.03  16.244  <.0001
# alternating   1.30 0.111 32.9     1.07     1.52  11.689  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# generous_status_second = higher:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    0.590 0.0763 2610    0.441    0.740   7.736  <.0001
# 
# generous_status_second = lower:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    0.537 0.0763 2610    0.387    0.687   7.038  <.0001
# 
# generous_status_second = equal:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    0.526 0.0761 2610    0.377    0.675   6.908  <.0001
# 
# generous_status_second = just_met:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    0.507 0.0763 2610    0.357    0.656   6.644  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 


# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# strategy_pairwise       generous_status_second_pairwise estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating higher - lower                    0.0532 0.108 2610   -0.158    0.265   0.493  0.6218
# repeating - alternating higher - equal                    0.0644 0.108 2610   -0.147    0.276   0.598  0.5500
# repeating - alternating higher - just_met                 0.0836 0.108 2611   -0.128    0.295   0.775  0.4383
# repeating - alternating lower - equal                     0.0112 0.108 2610   -0.200    0.223   0.104  0.9174
# repeating - alternating lower - just_met                  0.0304 0.108 2610   -0.181    0.242   0.282  0.7782
# repeating - alternating equal - just_met                  0.0192 0.108 2610   -0.192    0.230   0.178  0.8586
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
# NA         repeating     1.80 0.111 32.9     1.58     2.03  16.244  <.0001
# no         repeating     1.76 0.111 32.8     1.54     1.99  15.887  <.0001
# yes        repeating     1.95 0.104 25.6     1.74     2.17  18.707  <.0001
# NA         alternating   1.30 0.111 32.9     1.07     1.52  11.689  <.0001
# no         alternating   1.24 0.111 32.9     1.01     1.46  11.147  <.0001
# yes        alternating   1.39 0.104 25.6     1.17     1.60  13.310  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                         estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA repeating - no repeating        0.0410 0.0762 2610   -0.176 0.258393   0.537  0.9946
# NA repeating - yes repeating      -0.1472 0.0662 2611   -0.336 0.041486  -2.225  0.2266
# NA repeating - NA alternating      0.5066 0.0763 2610    0.289 0.724105   6.644  <.0001
# NA repeating - no alternating      0.5668 0.0762 2610    0.349 0.784262   7.434  <.0001
# NA repeating - yes alternating     0.4164 0.0661 2610    0.228 0.604868   6.303  <.0001
# no repeating - yes repeating      -0.1882 0.0660 2610   -0.376 0.000105  -2.850  0.0502
# no repeating - NA alternating      0.4657 0.0761 2610    0.249 0.682791   6.117  <.0001
# no repeating - no alternating      0.5259 0.0761 2610    0.309 0.742934   6.908  <.0001
# no repeating - yes alternating     0.3755 0.0659 2610    0.187 0.563510   5.695  <.0001
# yes repeating - NA alternating     0.6539 0.0660 2610    0.466 0.842166   9.902  <.0001
# yes repeating - no alternating     0.7140 0.0660 2610    0.526 0.902341  10.813  <.0001
# yes repeating - yes alternating    0.5637 0.0540 2611    0.410 0.717533  10.446  <.0001
# NA alternating - no alternating    0.0602 0.0761 2610   -0.157 0.277323   0.790  0.9693
# NA alternating - yes alternating  -0.0902 0.0659 2610   -0.278 0.097854  -1.368  0.7464
# no alternating - yes alternating  -0.1504 0.0659 2610   -0.338 0.037669  -2.281  0.2023
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 6 estimates 
# P value adjustment: tukey method for comparing a family of 6 estimates 


contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# asymmetric_pairwise strategy_pairwise       estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA - no             repeating - alternating  -0.0192 0.1078 2610   -0.230    0.192  -0.178  0.8586
# NA - yes            repeating - alternating  -0.0570 0.0934 2610   -0.240    0.126  -0.610  0.5417
# no - yes            repeating - alternating  -0.0378 0.0933 2610   -0.221    0.145  -0.405  0.6854
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


# Extra plots -------------------------------------------------------------

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
  labs(title = "study 3d", x = "status of second time generous person (A)", y = "how satisfied was B with outcome?") +
  theme(legend.position = "bottom")

f


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
  labs(title = "study 3d", x = "status of second time generous person (A)", y = "how annoyed was B?") +
  theme(legend.position = "bottom")

f


