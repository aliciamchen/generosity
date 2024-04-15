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
library(sjPlot)

theme_set(theme_classic(base_size = 20))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))


d <-
  read.csv(here('data/3e_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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


d.demographics <- read.csv(here('data/3e_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))


d.means.all <-
  d %>% drop_na() %>%
  group_by(strategy, generous_status_second, response) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


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



#### STATS
# emm_options(pbkrtest.limit = 9071)
# emm_options(lmerTest.limit = 9071)

mod <-
  lmer(
    data = d,
    likert_rating ~ strategy * generous_status_second * response + (1 |
                                                                      subject_id) + (1 | story)
  )

plot_model(mod, type = "re") 
summary(mod)
emm <- emmeans(mod,  ~ response * strategy * generous_status_second)
pairs(emm, simple = "response")


# For comparing 'communicate_equality' and 'communicate_hierarchy' across social interactions
emm1 <- emmeans(mod, ~ strategy * response)
contrast(emm1, interaction = c("pairwise", "pairwise"))
contrast(emm1, interaction = "pairwise", by = "response")

# For each relationship
emm1 <- emmeans(mod, ~ strategy * response | generous_status_second)
contrast(emm1,
         interaction = c("pairwise", "pairwise"),
         by = "generous_status_second")
contrast(emm1,
         interaction = "pairwise",
         by = c("generous_status_second", "response"))

# Violation of expectation
emm_alt <-
  subset(
    emmeans(mod, ~ strategy * response * generous_status_second),
    strategy == "alternating" & response == "communicate_equal"
  ) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

emm_alt
emmeans(emm_alt, pairwise ~ strategy * response * asymmetric)

emm_alt <-
  subset(
    emmeans(mod, ~ strategy * response * generous_status_second),
    strategy == "repeating" & response == "communicate_hierarchy"
  ) %>%
  add_grouping("asymmetric",
               "generous_status_second",
               c("yes", "yes", "no", "NA"))

emm_alt
emmeans(emm_alt, pairwise ~ strategy * response * asymmetric)


# Compare communicate equality to other motivations within alternating
emm4 <- emmeans(mod, ~ response | strategy,
                at = list(strategy = "alternating"))
contrast(emm4, interaction = "pairwise")

# And for specific relationships
emm4 <-
  emmeans(mod,
          ~ response | strategy * generous_status_second,
          at = list(strategy = "alternating"))
contrast(emm4, interaction = "pairwise")


# Total number of trials
result <- d %>% filter(strategy == "alternating") %>%
  group_by(subject_id, story, strategy, generous_status_second) %>%
  mutate(max_likert_count = sum(likert_rating == max(likert_rating))) %>%
  filter(likert_rating == max(likert_rating)) %>%
  filter(max_likert_count == 1) %>%
  ungroup() %>%
  # Count the frequency of each response
  count(response)


print(result)

result_with_proportions <- result %>%
  mutate(total = sum(n)) %>%
  mutate(proportion = n / total) %>%
  select(response, proportion)

print(result_with_proportions)
