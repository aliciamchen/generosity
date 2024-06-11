library(here)
library(tidyverse)
library(tidyboot)
library(emmeans)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(glue)

# Options -----------------------------------------------------------------
options(warn = -1)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

relationship.colors <- c("#B87FFF", "#35ACFF", "#00A08A", "#D6D6D6")

# Load data ---------------------------------------------------------------

d <-
  read.csv(here('data/3b_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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

write.csv(d, here('data/3b_tidy_data.csv'), row.names = FALSE)



# Demographics ------------------------------------------------------------

d.demographics <- read.csv(here('data/3b_demographics.csv'))
print(length(unique(d.demographics$subject_id)))

d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
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





# 3b for presentation
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
    labs(x = "strategy", y = "how fair", fill = "social relationship") +
    theme(legend.position = "bottom")

  print(f)

  ggsave(here(glue("figures/3b_{strat}.pdf")),
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
#                                 Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)
# strategy                        5308.9  5308.9     1 2599.9 3665.395 < 2.2e-16 ***
# generous_status_second           126.7    42.2     3 2599.9   29.155 < 2.2e-16 ***
# strategy:generous_status_second  152.1    50.7     3 2599.7   35.010 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Alternating > repeating in every social relationship
emmeans(mod, pairwise ~ strategy | generous_status_second) %>%
  summary(infer = T)

# $emmeans
# generous_status_second = higher:
#   strategy    emmean     SE   df lower.CL upper.CL t.ratio p.value
# repeating     4.18 0.0883 84.3     4.00     4.35  47.272  <.0001
# alternating   6.16 0.0883 84.0     5.98     6.34  69.779  <.0001
#
# generous_status_second = lower:
#   strategy    emmean     SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.19 0.0884 84.5     3.01     3.36  36.043  <.0001
# alternating   5.98 0.0883 84.0     5.80     6.16  67.754  <.0001
#
# generous_status_second = equal:
#   strategy    emmean     SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.42 0.0883 84.3     3.25     3.60  38.730  <.0001
# alternating   6.54 0.0883 84.0     6.37     6.72  74.143  <.0001
#
# generous_status_second = just_met:
#   strategy    emmean     SE   df lower.CL upper.CL t.ratio p.value
# repeating     3.28 0.0883 84.0     3.11     3.46  37.199  <.0001
# alternating   6.41 0.0883 84.0     6.24     6.59  72.622  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# generous_status_second = higher:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -1.98 0.0911 2600    -2.16    -1.80 -21.782  <.0001
#
# generous_status_second = lower:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -2.79 0.0911 2600    -2.97    -2.62 -30.652  <.0001
#
# generous_status_second = equal:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -3.12 0.0911 2600    -3.30    -2.94 -34.297  <.0001
#
# generous_status_second = just_met:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    -3.13 0.0910 2600    -3.31    -2.95 -34.364  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95


# Interaction contrasts
contrast(emmeans(mod, ~ strategy * generous_status_second), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# strategy_pairwise       generous_status_second_pairwise estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating higher - lower                   0.81029 0.129 2600   0.5576    1.063   6.289  <.0001
# repeating - alternating higher - equal                   1.13964 0.129 2600   0.8871    1.392   8.850  <.0001
# repeating - alternating higher - just_met                1.14400 0.129 2600   0.8915    1.397   8.884  <.0001
# repeating - alternating lower - equal                    0.32935 0.129 2600   0.0768    0.582   2.557  0.0106
# repeating - alternating lower - just_met                 0.33371 0.129 2600   0.0812    0.586   2.591  0.0096
# repeating - alternating equal - just_met                 0.00436 0.129 2600  -0.2481    0.257   0.034  0.9730
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
# asymmetric strategy    emmean     SE   df lower.CL upper.CL t.ratio p.value
# NA         repeating     3.28 0.0883 84.0     3.11     3.46  37.199  <.0001
# no         repeating     3.42 0.0883 84.3     3.25     3.60  38.730  <.0001
# yes        repeating     3.68 0.0757 45.7     3.53     3.83  48.629  <.0001
# NA         alternating   6.41 0.0883 84.0     6.24     6.59  72.622  <.0001
# no         alternating   6.54 0.0883 84.0     6.37     6.72  74.143  <.0001
# yes        alternating   6.07 0.0756 45.5     5.92     6.22  80.249  <.0001
#
# Results are averaged over the levels of: generous_status_second
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# contrast                         estimate     SE   df lower.CL upper.CL t.ratio p.value
# NA repeating - no repeating        -0.138 0.0911 2600   -0.398   0.1218  -1.514  0.6551
# NA repeating - yes repeating       -0.398 0.0789 2600   -0.623  -0.1730  -5.045  <.0001
# NA repeating - NA alternating      -3.128 0.0910 2600   -3.387  -2.8680 -34.364  <.0001
# NA repeating - no alternating      -3.261 0.0910 2599   -3.521  -3.0016 -35.840  <.0001
# NA repeating - yes alternating     -2.787 0.0788 2600   -3.011  -2.5618 -35.357  <.0001
# no repeating - yes repeating       -0.260 0.0789 2600   -0.485  -0.0349  -3.294  0.0128
# no repeating - NA alternating      -2.990 0.0911 2600   -3.249  -2.7299 -32.827  <.0001
# no repeating - no alternating      -3.123 0.0911 2600   -3.383  -2.8635 -34.297  <.0001
# no repeating - yes alternating     -2.649 0.0789 2600   -2.874  -2.4237 -33.571  <.0001
# yes repeating - NA alternating     -2.730 0.0789 2600   -2.955  -2.5046 -34.600  <.0001
# yes repeating - no alternating     -2.863 0.0789 2600   -3.088  -2.6382 -36.296  <.0001
# yes repeating - yes alternating    -2.389 0.0644 2600   -2.572  -2.2050 -37.080  <.0001
# NA alternating - no alternating    -0.134 0.0910 2600   -0.393   0.1260  -1.467  0.6853
# NA alternating - yes alternating    0.341 0.0788 2600    0.116   0.5657   4.325  0.0002
# no alternating - yes alternating    0.474 0.0788 2600    0.250   0.6992   6.020  <.0001
#
# Results are averaged over the levels of: generous_status_second
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 6 estimates
# P value adjustment: tukey method for comparing a family of 6 estimates



contrast(emmeans(emm, ~ asymmetric * strategy), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# asymmetric_pairwise strategy_pairwise       estimate    SE   df lower.CL upper.CL t.ratio p.value
# NA - no             repeating - alternating -0.00436 0.129 2600   -0.257    0.248  -0.034  0.9730
# NA - yes            repeating - alternating -0.73886 0.112 2600   -0.958   -0.520  -6.626  <.0001
# no - yes            repeating - alternating -0.73450 0.112 2599   -0.953   -0.516  -6.586  <.0001
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


# Extra -------------------------------------------------------------------


# All conditions on one plot
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
  scale_fill_manual(values = relationship.colors,
                    name = "social relationship") +
  labs(x = "strategy", y = "how fair?") +
  theme(legend.position = "bottom")

f

# Compare with study 1c
# This needs d.first.response from the study 1c data

d.first.response <-
  read.csv(here('data/1c_tidy_data.csv')) %>%
  mutate(first_response_higher = recode(
    first_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(story, effort_diff, benefit_diff) %>%
  tidyboot_mean(first_response_higher, na.rm = TRUE)

d.fairness.story <-
  d %>% drop_na() %>%
  group_by(strategy, story, generous_status_second) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE)

d.fairness.with.1c <-
  left_join(
    d.first.response,
    d.fairness.story,
    suffix = c("_1c", "_3"),
    by = (c("story"))
  )


f <-
  ggplot(
    d.fairness.with.1c %>% filter(
      generous_status_second != "equal" &
        generous_status_second != "just_met"
    ),
    aes(x = empirical_stat_1c, y = empirical_stat_3, color = generous_status_second)
  ) +
  geom_point(size = 3.3,
             alpha = 0.7,
             stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1c, ymin = ci_lower_3, ymax = ci_upper_3),
    size = 1.5,
    width = 0.06,
    alpha = 0.6
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_3, xmin = ci_lower_1c, xmax = ci_upper_1c),
    size = 1.5,
    height = 0.1,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 4,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  # scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "study 1c first time higher", y = "study 3 how fair", color = "who is generous")

f

