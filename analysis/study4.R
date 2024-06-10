library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(glue)
library(emmeans)



# Options -----------------------------------------------------------------

theme_set(theme_classic(base_size = 20))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

# emm_options(pbkrtest.limit = 9071)
# emm_options(lmerTest.limit = 9071)


# Load data ---------------------------------------------------------------


d <-
  read.csv(here('data/4_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c(
      "own_benefit",
      "other_benefit",
      "inequity_aversion",
      "communicate_equal",
      "communicate_hierarchy"
    ),
    names_to = "response",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    strategy = fct_relevel(strategy,
                           "repeating", "alternating"),
    generous_status_second = fct_relevel(generous_status_second,
                                         "higher", "lower", "equal", "just_met")
  )



# Demographics ------------------------------------------------------------

d.demographics <- read.csv(here('data/4_demographics.csv'))
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
  group_by(strategy, generous_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


# Plot: averaged across all relationships
d.means.all.all <-
  d %>% drop_na() %>%
  group_by(strategy, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


f = ggplot(data = d,
           aes(x = response, y = likert_rating, fill = strategy)) +
  geom_violin(width = 1.8,
              bw = 0.43,
              position = position_dodge(width = 0.6)) +
  geom_point(
    d.means.all.all,
    mapping = aes(x = response, y = likert_rating),
    size = 1.3,
    alpha = 1,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    d.means.all.all,
    mapping = aes(x = response, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.6),
    size = 1.0,
    width = 0.10
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  scale_x_discrete(
    limits = c(
      "own_benefit",
      "other_benefit",
      "inequity_aversion",
      "communicate_equal",
      "communicate_hierarchy"
    ),
    labels = c(
      "self benefit",
      "other benefit",
      "inequity aversion",
      "communicate equality",
      "communicate hierarchy"
    )
  ) +
  scale_fill_manual(values = c(
    "repeating" = "#DD8D29",
    "alternating" = "#E2D200"
  )) +
  labs(title = "", x = "reason", y = "how motivated?") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

f

ggsave(
  here("figures/outputs/motives_averaged.pdf"),
  width = 6,
  height = 6.5
)


# Show communicate_equal faceted by relationship
relationships <- c("higher", "equal", "lower", "just_met")

for (relationship in relationships) {
  f = ggplot(
    data = d %>% filter(
      generous_status_second == relationship,
      response == "communicate_equal"
    ),
    aes(x = response, y = likert_rating, fill = strategy)
  ) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.6)) +
    geom_point(
      d.means.all %>% filter(
        generous_status_second == relationship,
        response == "communicate_equal"
      ),
      mapping = aes(x = response, y = likert_rating),
      size = 1.3,
      alpha = 1,
      position = position_dodge(width = 0.6)
    ) +
    geom_errorbar(
      d.means.all %>% filter(
        generous_status_second == relationship,
        response == "communicate_equal"
      ),
      mapping = aes(x = response, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.6),
      size = 1.0,
      width = 0.10
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    scale_fill_manual(values = c(
      "repeating" = "#DD8D29",
      "alternating" = "#E2D200"
    )) +
    labs(title = "", x = "reason", y = "how motivated?") +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
  
  print(f)
  
  ggsave(here(
    glue("figures/outputs/motives_{relationship}_comm_equal.pdf")
  ),
  width = 2,
  height = 6.5)
}





# Stats -------------------------------------------------------------------


mod <-
  lmer(
    data = d,
    likert_rating ~ strategy * generous_status_second * response + (1 |
                                                                      subject_id) + (1 | story)
  )

summary(mod)

anova(mod)

# Type III Analysis of Variance Table with Satterthwaite's method
#                                          Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# strategy                                  687.9  687.92     1 8910.4 249.9898 < 2.2e-16 ***
# generous_status_second                     28.3    9.42     3 8909.3   3.4246  0.016417 *  
# response                                 3240.1  810.03     4 8903.1 294.3656 < 2.2e-16 ***
# strategy:generous_status_second            40.5   13.49     3 8913.1   4.9036  0.002092 ** 
# strategy:response                        4550.1 1137.52     4 8903.0 413.3769 < 2.2e-16 ***
# generous_status_second:response           205.7   17.14    12 8903.0   6.2278 4.639e-11 ***
# strategy:generous_status_second:response   86.8    7.23    12 8903.1   2.6290  0.001646 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Violation of expectation ------------------------------------------------

emm_alt <-
  subset(
    emmeans(mod, ~ strategy * response * generous_status_second),
    strategy == "alternating" & response == "communicate_equal"
  ) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

# Alternating: Communicate equality equal > communicate equality hierarchical 
emmeans(emm_alt, pairwise ~ strategy * response * asymmetric) %>% 
  summary(infer = T)

# $emmeans
# strategy    response          asymmetric emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# alternating communicate_equal NA           5.93 0.127 Inf      5.68      6.17  46.782  <.0001
# alternating communicate_equal no           5.99 0.127 Inf      5.74      6.23  47.177  <.0001
# alternating communicate_equal yes          5.27 0.100 Inf      5.08      5.47  52.616  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                                                             estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# alternating communicate_equal NA - alternating communicate_equal no   -0.0598 0.156 Inf    -0.424     0.305  -0.384  0.9218
# alternating communicate_equal NA - alternating communicate_equal yes   0.6545 0.135 Inf     0.339     0.970   4.860  <.0001
# alternating communicate_equal no - alternating communicate_equal yes   0.7142 0.135 Inf     0.398     1.030   5.296  <.0001
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 3 estimates 
# P value adjustment: tukey method for comparing a family of 3 estimates 





# reciprocation, responses averaged across responses and considered separately --------


# communicate equal > communicate hierarchy, averaged across relationship
emmeans(mod, ~ response * strategy, at = list(strategy = "alternating")) %>% 
  pairs() %>% 
  summary(infer = T)

# contrast                                                          estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal alternating - communicate_hierarchy alternating    3.673 0.0778 Inf    3.4609     3.885  47.192  <.0001
# communicate_equal alternating - inequity_aversion alternating        0.518 0.0780 Inf    0.3056     0.731   6.646  <.0001
# communicate_equal alternating - other_benefit alternating            1.112 0.0778 Inf    0.9001     1.325  14.295  <.0001
# communicate_equal alternating - own_benefit alternating              1.411 0.0779 Inf    1.1982     1.623  18.113  <.0001
# communicate_hierarchy alternating - inequity_aversion alternating   -3.155 0.0780 Inf   -3.3676    -2.942 -40.442  <.0001
# communicate_hierarchy alternating - other_benefit alternating       -2.561 0.0778 Inf   -2.7732    -2.349 -32.901  <.0001
# communicate_hierarchy alternating - own_benefit alternating         -2.263 0.0779 Inf   -2.4751    -2.050 -29.044  <.0001
# inequity_aversion alternating - other_benefit alternating            0.594 0.0780 Inf    0.3813     0.807   7.617  <.0001
# inequity_aversion alternating - own_benefit alternating              0.892 0.0781 Inf    0.6794     1.105  11.432  <.0001
# other_benefit alternating - own_benefit alternating                  0.298 0.0779 Inf    0.0859     0.511   3.830  0.0012
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 5 estimates 
# P value adjustment: tukey method for comparing a family of 5 estimates 

# communicate equal > communicate hierarchy, for each relationship considered separately
emmeans(mod, ~ response * strategy | generous_status_second, at = list(strategy = "alternating")) %>% 
  pairs() %>% 
  summary(infer = T)

# generous_status_second = higher:
#   contrast                                                          estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal alternating - communicate_hierarchy alternating    3.042 0.156 Inf    2.6156     3.469  19.450  <.0001
# communicate_equal alternating - inequity_aversion alternating        0.200 0.156 Inf   -0.2254     0.626   1.284  0.7014
# communicate_equal alternating - other_benefit alternating            0.574 0.156 Inf    0.1487     0.999   3.682  0.0022
# communicate_equal alternating - own_benefit alternating              1.187 0.156 Inf    0.7612     1.613   7.606  <.0001
# communicate_hierarchy alternating - inequity_aversion alternating   -2.842 0.156 Inf   -3.2685    -2.415 -18.170  <.0001
# communicate_hierarchy alternating - other_benefit alternating       -2.468 0.156 Inf   -2.8944    -2.042 -15.798  <.0001
# communicate_hierarchy alternating - own_benefit alternating         -1.855 0.156 Inf   -2.2819    -1.429 -11.862  <.0001
# inequity_aversion alternating - other_benefit alternating            0.374 0.156 Inf   -0.0516     0.799   2.397  0.1161
# inequity_aversion alternating - own_benefit alternating              0.987 0.156 Inf    0.5609     1.412   6.322  <.0001
# other_benefit alternating - own_benefit alternating                  0.613 0.156 Inf    0.1878     1.038   3.932  0.0008
# 
# generous_status_second = lower:
#   contrast                                                          estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal alternating - communicate_hierarchy alternating    3.307 0.155 Inf    2.8832     3.731  21.285  <.0001
# communicate_equal alternating - inequity_aversion alternating        0.701 0.156 Inf    0.2757     1.125   4.499  0.0001
# communicate_equal alternating - other_benefit alternating            1.136 0.155 Inf    0.7122     1.560   7.312  <.0001
# communicate_equal alternating - own_benefit alternating              0.901 0.156 Inf    0.4768     1.325   5.793  <.0001
# communicate_hierarchy alternating - inequity_aversion alternating   -2.607 0.156 Inf   -3.0313    -2.182 -16.739  <.0001
# communicate_hierarchy alternating - other_benefit alternating       -2.171 0.155 Inf   -2.5949    -1.747 -13.974  <.0001
# communicate_hierarchy alternating - own_benefit alternating         -2.406 0.156 Inf   -2.8302    -1.982 -15.469  <.0001
# inequity_aversion alternating - other_benefit alternating            0.435 0.156 Inf    0.0107     0.860   2.797  0.0413
# inequity_aversion alternating - own_benefit alternating              0.201 0.156 Inf   -0.2247     0.626   1.287  0.6996
# other_benefit alternating - own_benefit alternating                 -0.235 0.156 Inf   -0.6592     0.189  -1.510  0.5557
# 
# generous_status_second = equal:
#   contrast                                                          estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal alternating - communicate_hierarchy alternating    4.313 0.156 Inf    3.8885     4.737  27.728  <.0001
# communicate_equal alternating - inequity_aversion alternating        0.463 0.156 Inf    0.0376     0.889   2.969  0.0249
# communicate_equal alternating - other_benefit alternating            1.437 0.156 Inf    1.0115     1.862   9.217  <.0001
# communicate_equal alternating - own_benefit alternating              1.768 0.156 Inf    1.3431     2.193  11.353  <.0001
# communicate_hierarchy alternating - inequity_aversion alternating   -3.849 0.156 Inf   -4.2747    -3.424 -24.694  <.0001
# communicate_hierarchy alternating - other_benefit alternating       -2.876 0.156 Inf   -3.3008    -2.451 -18.470  <.0001
# communicate_hierarchy alternating - own_benefit alternating         -2.545 0.156 Inf   -2.9692    -2.121 -16.362  <.0001
# inequity_aversion alternating - other_benefit alternating            0.973 0.156 Inf    0.5472     1.400   6.230  <.0001
# inequity_aversion alternating - own_benefit alternating              1.305 0.156 Inf    0.8788     1.730   8.359  <.0001
# other_benefit alternating - own_benefit alternating                  0.331 0.156 Inf   -0.0941     0.756   2.124  0.2098
# 
# generous_status_second = just_met:
#   contrast                                                          estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal alternating - communicate_hierarchy alternating    4.031 0.155 Inf    3.6069     4.455  25.943  <.0001
# communicate_equal alternating - inequity_aversion alternating        0.709 0.156 Inf    0.2834     1.135   4.544  0.0001
# communicate_equal alternating - other_benefit alternating            1.303 0.155 Inf    0.8788     1.726   8.384  <.0001
# communicate_equal alternating - own_benefit alternating              1.787 0.156 Inf    1.3619     2.211  11.474  <.0001
# communicate_hierarchy alternating - inequity_aversion alternating   -3.322 0.156 Inf   -3.7473    -2.896 -21.283  <.0001
# communicate_hierarchy alternating - other_benefit alternating       -2.728 0.155 Inf   -3.1519    -2.304 -17.559  <.0001
# communicate_hierarchy alternating - own_benefit alternating         -2.244 0.156 Inf   -2.6688    -1.819 -14.411  <.0001
# inequity_aversion alternating - other_benefit alternating            0.593 0.156 Inf    0.1678     1.019   3.803  0.0013
# inequity_aversion alternating - own_benefit alternating              1.077 0.156 Inf    0.6508     1.504   6.889  <.0001
# other_benefit alternating - own_benefit alternating                  0.484 0.156 Inf    0.0593     0.909   3.108  0.0161
# 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 5 estimates 
# P value adjustment: tukey method for comparing a family of 5 estimates 


# Is this diff from repeatedly generous? ----------------------------------


# Interaction contrast across all relationship conditions
contrast(emmeans(mod, ~ strategy * response), interaction = c("pairwise", "pairwise")) %>% 
  summary(infer = T)

# strategy_pairwise       response_pairwise                         estimate   SE  df asymp.LCL asymp.UCL z.ratio p.value
# repeating - alternating communicate_equal - communicate_hierarchy   -4.198 0.11 Inf    -4.413    -3.982 -38.140  <.0001
# repeating - alternating communicate_equal - inequity_aversion       -0.912 0.11 Inf    -1.128    -0.695  -8.268  <.0001
# repeating - alternating communicate_equal - other_benefit           -2.117 0.11 Inf    -2.333    -1.901 -19.227  <.0001
# repeating - alternating communicate_equal - own_benefit             -2.228 0.11 Inf    -2.444    -2.012 -20.233  <.0001
# repeating - alternating communicate_hierarchy - inequity_aversion    3.286 0.11 Inf     3.070     3.502  29.812  <.0001
# repeating - alternating communicate_hierarchy - other_benefit        2.081 0.11 Inf     1.865     2.296  18.900  <.0001
# repeating - alternating communicate_hierarchy - own_benefit          1.970 0.11 Inf     1.754     2.186  17.893  <.0001
# repeating - alternating inequity_aversion - other_benefit           -1.205 0.11 Inf    -1.422    -0.989 -10.931  <.0001
# repeating - alternating inequity_aversion - own_benefit             -1.316 0.11 Inf    -1.532    -1.100 -11.936  <.0001
# repeating - alternating other_benefit - own_benefit                 -0.111 0.11 Inf    -0.327     0.105  -1.006  0.3144
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 

# interaction contrasts for each relationship considered separately
contrast(emmeans(mod, ~ strategy * response | generous_status_second),
         interaction = c("pairwise", "pairwise"),
         by = "generous_status_second") %>% 
  summary(infer = T)

# generous_status_second = higher:
#   strategy_pairwise       response_pairwise                         estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# repeating - alternating communicate_equal - communicate_hierarchy  -3.6030 0.221 Inf    -4.035    -3.171 -16.334  <.0001
# repeating - alternating communicate_equal - inequity_aversion      -0.7410 0.220 Inf    -1.173    -0.309  -3.363  0.0008
# repeating - alternating communicate_equal - other_benefit          -1.5119 0.220 Inf    -1.944    -1.080  -6.862  <.0001
# repeating - alternating communicate_equal - own_benefit            -1.9018 0.220 Inf    -2.333    -1.470  -8.637  <.0001
# repeating - alternating communicate_hierarchy - inequity_aversion   2.8620 0.221 Inf     2.429     3.295  12.968  <.0001
# repeating - alternating communicate_hierarchy - other_benefit       2.0910 0.221 Inf     1.658     2.524   9.474  <.0001
# repeating - alternating communicate_hierarchy - own_benefit         1.7012 0.221 Inf     1.269     2.133   7.712  <.0001
# repeating - alternating inequity_aversion - other_benefit          -0.7709 0.220 Inf    -1.203    -0.339  -3.497  0.0005
# repeating - alternating inequity_aversion - own_benefit            -1.1608 0.220 Inf    -1.593    -0.729  -5.269  <.0001
# repeating - alternating other_benefit - own_benefit                -0.3899 0.220 Inf    -0.822     0.042  -1.770  0.0768
# 
# generous_status_second = lower:
#   strategy_pairwise       response_pairwise                         estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# repeating - alternating communicate_equal - communicate_hierarchy  -3.8296 0.220 Inf    -4.261    -3.398 -17.400  <.0001
# repeating - alternating communicate_equal - inequity_aversion      -0.8385 0.220 Inf    -1.271    -0.406  -3.803  0.0001
# repeating - alternating communicate_equal - other_benefit          -1.9727 0.220 Inf    -2.404    -1.541  -8.963  <.0001
# repeating - alternating communicate_equal - own_benefit            -1.8379 0.220 Inf    -2.270    -1.406  -8.342  <.0001
# repeating - alternating communicate_hierarchy - inequity_aversion   2.9911 0.220 Inf     2.559     3.423  13.576  <.0001
# repeating - alternating communicate_hierarchy - other_benefit       1.8569 0.220 Inf     1.426     2.288   8.442  <.0001
# repeating - alternating communicate_hierarchy - own_benefit         1.9917 0.220 Inf     1.560     2.423   9.045  <.0001
# repeating - alternating inequity_aversion - other_benefit          -1.1342 0.220 Inf    -1.566    -0.702  -5.148  <.0001
# repeating - alternating inequity_aversion - own_benefit            -0.9994 0.221 Inf    -1.432    -0.567  -4.531  <.0001
# repeating - alternating other_benefit - own_benefit                 0.1348 0.220 Inf    -0.297     0.566   0.612  0.5405
# 
# generous_status_second = equal:
#   strategy_pairwise       response_pairwise                         estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# repeating - alternating communicate_equal - communicate_hierarchy  -4.5566 0.220 Inf    -4.988    -4.125 -20.715  <.0001
# repeating - alternating communicate_equal - inequity_aversion      -0.8726 0.221 Inf    -1.305    -0.440  -3.956  0.0001
# repeating - alternating communicate_equal - other_benefit          -2.2946 0.220 Inf    -2.726    -1.863 -10.415  <.0001
# repeating - alternating communicate_equal - own_benefit            -2.5000 0.220 Inf    -2.932    -2.068 -11.353  <.0001
# repeating - alternating communicate_hierarchy - inequity_aversion   3.6840 0.220 Inf     3.252     4.116  16.720  <.0001
# repeating - alternating communicate_hierarchy - other_benefit       2.2619 0.220 Inf     1.831     2.693  10.277  <.0001
# repeating - alternating communicate_hierarchy - own_benefit         2.0565 0.220 Inf     1.625     2.488   9.349  <.0001
# repeating - alternating inequity_aversion - other_benefit          -1.4221 0.221 Inf    -1.855    -0.990  -6.443  <.0001
# repeating - alternating inequity_aversion - own_benefit            -1.6275 0.221 Inf    -2.060    -1.195  -7.378  <.0001
# repeating - alternating other_benefit - own_benefit                -0.2054 0.220 Inf    -0.637     0.226  -0.932  0.3512
# 
# generous_status_second = just_met:
#   strategy_pairwise       response_pairwise                         estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# repeating - alternating communicate_equal - communicate_hierarchy  -4.8014 0.220 Inf    -5.232    -4.371 -21.840  <.0001
# repeating - alternating communicate_equal - inequity_aversion      -1.1940 0.221 Inf    -1.626    -0.762  -5.413  <.0001
# repeating - alternating communicate_equal - other_benefit          -2.6886 0.220 Inf    -3.120    -2.257 -12.216  <.0001
# repeating - alternating communicate_equal - own_benefit            -2.6714 0.220 Inf    -3.103    -2.240 -12.138  <.0001
# repeating - alternating communicate_hierarchy - inequity_aversion   3.6074 0.220 Inf     3.175     4.039  16.363  <.0001
# repeating - alternating communicate_hierarchy - other_benefit       2.1128 0.220 Inf     1.682     2.544   9.605  <.0001
# repeating - alternating communicate_hierarchy - own_benefit         2.1300 0.220 Inf     1.699     2.561   9.683  <.0001
# repeating - alternating inequity_aversion - other_benefit          -1.4946 0.221 Inf    -1.927    -1.062  -6.772  <.0001
# repeating - alternating inequity_aversion - own_benefit            -1.4774 0.221 Inf    -1.910    -1.045  -6.694  <.0001
# repeating - alternating other_benefit - own_benefit                 0.0172 0.220 Inf    -0.414     0.449   0.078  0.9376
# 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 


# Compare motives for specific actions ------------------------------------

# Communicate equality relative to the other 4 motivations?
emmeans(mod, ~ response | strategy, at = list(strategy = "alternating")) %>% 
  pairs() %>% 
  summary(infer = T)

# strategy = alternating:
#   contrast                                  estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy    3.673 0.0778 Inf    3.4609     3.885  47.192  <.0001
# communicate_equal - inequity_aversion        0.518 0.0780 Inf    0.3056     0.731   6.646  <.0001
# communicate_equal - other_benefit            1.112 0.0778 Inf    0.9001     1.325  14.295  <.0001
# communicate_equal - own_benefit              1.411 0.0779 Inf    1.1982     1.623  18.113  <.0001
# communicate_hierarchy - inequity_aversion   -3.155 0.0780 Inf   -3.3676    -2.942 -40.442  <.0001
# communicate_hierarchy - other_benefit       -2.561 0.0778 Inf   -2.7732    -2.349 -32.901  <.0001
# communicate_hierarchy - own_benefit         -2.263 0.0779 Inf   -2.4751    -2.050 -29.044  <.0001
# inequity_aversion - other_benefit            0.594 0.0780 Inf    0.3813     0.807   7.617  <.0001
# inequity_aversion - own_benefit              0.892 0.0781 Inf    0.6794     1.105  11.432  <.0001
# other_benefit - own_benefit                  0.298 0.0779 Inf    0.0859     0.511   3.830  0.0012
# 
# Results are averaged over the levels of: generous_status_second 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 5 estimates 
# P value adjustment: tukey method for comparing a family of 5 estimates 

# And for each relationship considered separately
emmeans(mod,  ~ response | strategy * generous_status_second, at = list(strategy = "alternating")) %>% 
  pairs(simple = "response") %>% 
  summary(infer = T)

# strategy = alternating, generous_status_second = higher:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy    3.042 0.156 Inf    2.6156     3.469  19.450  <.0001
# communicate_equal - inequity_aversion        0.200 0.156 Inf   -0.2254     0.626   1.284  0.7014
# communicate_equal - other_benefit            0.574 0.156 Inf    0.1487     0.999   3.682  0.0022
# communicate_equal - own_benefit              1.187 0.156 Inf    0.7612     1.613   7.606  <.0001
# communicate_hierarchy - inequity_aversion   -2.842 0.156 Inf   -3.2685    -2.415 -18.170  <.0001
# communicate_hierarchy - other_benefit       -2.468 0.156 Inf   -2.8944    -2.042 -15.798  <.0001
# communicate_hierarchy - own_benefit         -1.855 0.156 Inf   -2.2819    -1.429 -11.862  <.0001
# inequity_aversion - other_benefit            0.374 0.156 Inf   -0.0516     0.799   2.397  0.1161
# inequity_aversion - own_benefit              0.987 0.156 Inf    0.5609     1.412   6.322  <.0001
# other_benefit - own_benefit                  0.613 0.156 Inf    0.1878     1.038   3.932  0.0008
# 
# strategy = alternating, generous_status_second = lower:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy    3.307 0.155 Inf    2.8832     3.731  21.285  <.0001
# communicate_equal - inequity_aversion        0.701 0.156 Inf    0.2757     1.125   4.499  0.0001
# communicate_equal - other_benefit            1.136 0.155 Inf    0.7122     1.560   7.312  <.0001
# communicate_equal - own_benefit              0.901 0.156 Inf    0.4768     1.325   5.793  <.0001
# communicate_hierarchy - inequity_aversion   -2.607 0.156 Inf   -3.0313    -2.182 -16.739  <.0001
# communicate_hierarchy - other_benefit       -2.171 0.155 Inf   -2.5949    -1.747 -13.974  <.0001
# communicate_hierarchy - own_benefit         -2.406 0.156 Inf   -2.8302    -1.982 -15.469  <.0001
# inequity_aversion - other_benefit            0.435 0.156 Inf    0.0107     0.860   2.797  0.0413
# inequity_aversion - own_benefit              0.201 0.156 Inf   -0.2247     0.626   1.287  0.6996
# other_benefit - own_benefit                 -0.235 0.156 Inf   -0.6592     0.189  -1.510  0.5557
# 
# strategy = alternating, generous_status_second = equal:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy    4.313 0.156 Inf    3.8885     4.737  27.728  <.0001
# communicate_equal - inequity_aversion        0.463 0.156 Inf    0.0376     0.889   2.969  0.0249
# communicate_equal - other_benefit            1.437 0.156 Inf    1.0115     1.862   9.217  <.0001
# communicate_equal - own_benefit              1.768 0.156 Inf    1.3431     2.193  11.353  <.0001
# communicate_hierarchy - inequity_aversion   -3.849 0.156 Inf   -4.2747    -3.424 -24.694  <.0001
# communicate_hierarchy - other_benefit       -2.876 0.156 Inf   -3.3008    -2.451 -18.470  <.0001
# communicate_hierarchy - own_benefit         -2.545 0.156 Inf   -2.9692    -2.121 -16.362  <.0001
# inequity_aversion - other_benefit            0.973 0.156 Inf    0.5472     1.400   6.230  <.0001
# inequity_aversion - own_benefit              1.305 0.156 Inf    0.8788     1.730   8.359  <.0001
# other_benefit - own_benefit                  0.331 0.156 Inf   -0.0941     0.756   2.124  0.2098
# 
# strategy = alternating, generous_status_second = just_met:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy    4.031 0.155 Inf    3.6069     4.455  25.943  <.0001
# communicate_equal - inequity_aversion        0.709 0.156 Inf    0.2834     1.135   4.544  0.0001
# communicate_equal - other_benefit            1.303 0.155 Inf    0.8788     1.726   8.384  <.0001
# communicate_equal - own_benefit              1.787 0.156 Inf    1.3619     2.211  11.474  <.0001
# communicate_hierarchy - inequity_aversion   -3.322 0.156 Inf   -3.7473    -2.896 -21.283  <.0001
# communicate_hierarchy - other_benefit       -2.728 0.155 Inf   -3.1519    -2.304 -17.559  <.0001
# communicate_hierarchy - own_benefit         -2.244 0.156 Inf   -2.6688    -1.819 -14.411  <.0001
# inequity_aversion - other_benefit            0.593 0.156 Inf    0.1678     1.019   3.803  0.0013
# inequity_aversion - own_benefit              1.077 0.156 Inf    0.6508     1.504   6.889  <.0001
# other_benefit - own_benefit                  0.484 0.156 Inf    0.0593     0.909   3.108  0.0161
# 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 5 estimates 
# P value adjustment: tukey method for comparing a family of 5 estimates 



# Are communicative motives were generally rated lower for following a precedent, than for alternation? 
emm_alt <-
  emmeans(mod, ~ strategy * response) %>%
  add_grouping("is_communicative",
               "response",
               c("yes", "yes", "no", "no", "no"))

emmeans(emm_alt, pairwise ~ strategy | is_communicative) %>% 
  summary(infer = T)

# And for a character who is repeatedly generous?
emmeans(mod,  ~ response | strategy * generous_status_second, at = list(strategy = "repeating")) %>% 
  pairs(simple = "response") %>% 
  summary(infer = T)

# strategy = repeating, generous_status_second = higher:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy  -0.5608 0.156 Inf   -0.9851   -0.1365  -3.605  0.0029
# communicate_equal - inequity_aversion      -0.5407 0.156 Inf   -0.9650   -0.1164  -3.476  0.0046
# communicate_equal - other_benefit          -0.9380 0.156 Inf   -1.3628   -0.5133  -6.024  <.0001
# communicate_equal - own_benefit            -0.7149 0.155 Inf   -1.1387   -0.2911  -4.601  <.0001
# communicate_hierarchy - inequity_aversion   0.0201 0.156 Inf   -0.4047    0.4448   0.129  0.9999
# communicate_hierarchy - other_benefit      -0.3772 0.156 Inf   -0.8024    0.0480  -2.420  0.1100
# communicate_hierarchy - own_benefit        -0.1541 0.156 Inf   -0.5784    0.2701  -0.991  0.8596
# inequity_aversion - other_benefit          -0.3973 0.156 Inf   -0.8225    0.0279  -2.549  0.0802
# inequity_aversion - own_benefit            -0.1742 0.156 Inf   -0.5985    0.2501  -1.120  0.7961
# other_benefit - own_benefit                 0.2231 0.156 Inf   -0.2016    0.6479   1.433  0.6064
# 
# strategy = repeating, generous_status_second = lower:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy  -0.5226 0.156 Inf   -0.9478   -0.0974  -3.353  0.0072
# communicate_equal - inequity_aversion      -0.1380 0.156 Inf   -0.5637    0.2877  -0.884  0.9028
# communicate_equal - other_benefit          -0.8368 0.156 Inf   -1.2620   -0.4115  -5.368  <.0001
# communicate_equal - own_benefit            -0.9369 0.156 Inf   -1.3626   -0.5112  -6.003  <.0001
# communicate_hierarchy - inequity_aversion   0.3846 0.156 Inf   -0.0406    0.8099   2.467  0.0981
# communicate_hierarchy - other_benefit      -0.3141 0.156 Inf   -0.7389    0.1106  -2.017  0.2575
# communicate_hierarchy - own_benefit        -0.4142 0.156 Inf   -0.8395    0.0110  -2.657  0.0605
# inequity_aversion - other_benefit          -0.6988 0.156 Inf   -1.1240   -0.2735  -4.483  0.0001
# inequity_aversion - own_benefit            -0.7989 0.156 Inf   -1.2246   -0.3732  -5.119  <.0001
# other_benefit - own_benefit                -0.1001 0.156 Inf   -0.5253    0.3251  -0.642  0.9681
# 
# strategy = repeating, generous_status_second = equal:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy  -0.2438 0.156 Inf   -0.6681    0.1805  -1.568  0.5184
# communicate_equal - inequity_aversion      -0.4093 0.156 Inf   -0.8345    0.0160  -2.625  0.0658
# communicate_equal - other_benefit          -0.8579 0.156 Inf   -1.2827   -0.4332  -5.510  <.0001
# communicate_equal - own_benefit            -0.7322 0.156 Inf   -1.1570   -0.3075  -4.702  <.0001
# communicate_hierarchy - inequity_aversion  -0.1654 0.156 Inf   -0.5902    0.2593  -1.062  0.8258
# communicate_hierarchy - other_benefit      -0.6141 0.156 Inf   -1.0384   -0.1898  -3.948  0.0008
# communicate_hierarchy - own_benefit        -0.4884 0.156 Inf   -0.9127   -0.0641  -3.140  0.0146
# inequity_aversion - other_benefit          -0.4487 0.156 Inf   -0.8739   -0.0234  -2.878  0.0326
# inequity_aversion - own_benefit            -0.3230 0.156 Inf   -0.7482    0.1022  -2.072  0.2323
# other_benefit - own_benefit                 0.1257 0.156 Inf   -0.2991    0.5504   0.807  0.9285
# 
# strategy = repeating, generous_status_second = just_met:
#   contrast                                  estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# communicate_equal - communicate_hierarchy  -0.7707 0.156 Inf   -1.1950   -0.3464  -4.955  <.0001
# communicate_equal - inequity_aversion      -0.4849 0.156 Inf   -0.9101   -0.0596  -3.110  0.0160
# communicate_equal - other_benefit          -1.3860 0.156 Inf   -1.8112   -0.9608  -8.891  <.0001
# communicate_equal - own_benefit            -0.8847 0.156 Inf   -1.3090   -0.4605  -5.688  <.0001
# communicate_hierarchy - inequity_aversion   0.2859 0.156 Inf   -0.1389    0.7106   1.836  0.3528
# communicate_hierarchy - other_benefit      -0.6153 0.156 Inf   -1.0400   -0.1905  -3.951  0.0007
# communicate_hierarchy - own_benefit        -0.1140 0.155 Inf   -0.5378    0.3098  -0.734  0.9486
# inequity_aversion - other_benefit          -0.9011 0.156 Inf   -1.3268   -0.4754  -5.774  <.0001
# inequity_aversion - own_benefit            -0.3999 0.156 Inf   -0.8246    0.0249  -2.568  0.0763
# other_benefit - own_benefit                 0.5012 0.156 Inf    0.0765    0.9260   3.219  0.0113
# 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 5 estimates 
# P value adjustment: tukey method for comparing a family of 5 estimates


# Total number of trials --------------------------------------------------

result <- d %>% filter(strategy == "alternating") %>%
  group_by(subject_id, story, strategy, generous_status_second) %>%
  mutate(max_likert_count = sum(likert_rating == max(likert_rating))) %>%
  filter(likert_rating == max(likert_rating)) %>%
  filter(max_likert_count == 1) %>%
  ungroup() %>%
  # Count the frequency of each response
  count(response)

print(result)

sum(result$n)
chisq.test(x = result$n)

# Display all proportions -------------------------------------------------

result_with_proportions <- result %>%
  mutate(total = sum(n)) %>%
  mutate(proportion = n / total) %>%
  select(response, proportion)

print(result_with_proportions)




# ########################################################

######## EXTRAS

# Extra plots -------------------------------------------------------------


f = ggplot(data = d,
           aes(x = response, y = likert_rating)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = response, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = response, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  scale_x_discrete(
    limits = c(
      "own_benefit",
      "other_benefit",
      "inequity_aversion",
      "communicate_equal",
      "communicate_hierarchy"
    )
  ) +
  labs(title = "", x = "reason", y = "how motivated?") +
  theme(legend.position = "bottom") +
  facet_grid(strategy ~ generous_status_second) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f


# Facet by relationship
relationships <- c("higher", "equal", "lower", "just_met")

for (relationship in relationships) {
  f = ggplot(
    data = d %>% filter(generous_status_second == relationship),
    aes(x = response, y = likert_rating, fill = strategy)
  ) +
    geom_violin(width = 1.7,
                bw = 0.43,
                position = position_dodge(width = 0.6)) +
    geom_point(
      d.means.all %>% filter(generous_status_second == relationship),
      mapping = aes(x = response, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.6)
    ) +
    geom_errorbar(
      d.means.all %>% filter(generous_status_second == relationship),
      mapping = aes(x = response, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.6),
      size = 1.5,
      width = 0.14
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    scale_x_discrete(
      limits = c(
        "own_benefit",
        "other_benefit",
        "inequity_aversion",
        "communicate_equal",
        "communicate_hierarchy"
      ),
      labels = c(
        "self benefit",
        "other benefit",
        "inequity aversion",
        "communicate equality",
        "communicate hierarchy"
      )
    ) +
    scale_fill_manual(values = c(
      "repeating" = "#DD8D29",
      "alternating" = "#E2D200"
    )) +
    labs(title = "", x = "reason", y = "how motivated?") +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
  
  print(f)
  
  ggsave(here(glue(
    "figures/outputs/motives_{relationship}.pdf"
  )),
  width = 4,
  height = 6)
}

