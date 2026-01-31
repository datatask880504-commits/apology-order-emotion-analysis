# ============================================
# 03_hypothesis_tests_self_blame_apology
# ============================================
library(dplyr)
library(broom)

############################################################
## Root
############################################################

root       <- "/Users/pei-chin/Research/Behavioral Science and Marketing_data_task"
data_clean <- file.path(root, "data_clean")
output     <- file.path(root, "output")
figures    <- file.path(output, "figures")

load(file.path(data_clean, "clean_data.RData"))


# Check variable type 
str(clean_data$high_blame)
str(clean_data$feelings_youalone)

############################################################
## H1: In the "you apologize alone" scenario, individuals high
## in self-blame experience higher emotional positivity than
## those low in self-blame.
############################################################

# Extract data for low and high self-blame groups
y0 <- clean_data$feelings_youalone[clean_data$high_blame == 0]
y1 <- clean_data$feelings_youalone[clean_data$high_blame == 1]

# Quick check 
length(y0); length(y1)
sum(is.na(y0)); sum(is.na(y1))

mean(y0)
mean(y1)
mean(y1) - mean(y0)  

# Run Welch's t-test (unequal variances assumed)
tt_H1 <- t.test(y1, y0, var.equal = FALSE)
tt_H1

# Calculate effect size - using Hedges' g for better small sample correction
n0 <- length(y0); n1 <- length(y1)
m0 <- mean(y0);   m1 <- mean(y1)
sd0 <- sd(y0);    sd1 <- sd(y1)

# Pooled standard deviation
sp <- sqrt(((n0 - 1)*sd0^2 + (n1 - 1)*sd1^2) / (n0 + n1 - 2))

# Cohen's d
d  <- (m1 - m0) / sp

# Small sample correction factor
J  <- 1 - (3 / (4*(n0 + n1) - 9))

# Hedges' g
g  <- J * d

effect_H1 <- c(
  n_low     = n0,
  n_high    = n1,
  mean_low  = m0,
  mean_high = m1,
  mean_diff = m1 - m0,
  cohen_d   = d,
  hedges_g  = g
)

effect_H1

############################################################
## Save result: H1 
############################################################
H1_summary <- data.frame(
  hypothesis = "H1",
  scenario   = "feelings_youalone",
  n_low      = n0,
  n_high     = n1,
  mean_low   = m0,
  mean_high  = m1,
  mean_diff  = m1 - m0,
  t_value    = as.numeric(tt_H1$statistic),
  df         = as.numeric(tt_H1$parameter),
  p_value    = tt_H1$p.value,
  ci_low     = tt_H1$conf.int[1],
  ci_high    = tt_H1$conf.int[2],
  hedges_g   = g
)

write.csv(
  H1_summary,
  file.path(output, "H1_ttest_hedges_g.csv"),
  row.names = FALSE
)

############################################################
## Robustness Check Across Scenarios
## Exploratory robustness check across scenarios
## See if the pattern holds in other apology scenarios
############################################################

feeling_vars <- c(
  "feelings_youalone",
  "feelings_bothyoufirst",
  "feelings_themalone",
  "feelings_boththemfirst",
  "feelings_neither",
  "feelings_youaloneforgiven"
)

# Helper function to run t-test and compute effect sizes
run_welch_effect <- function(data, outcome, group_var = "high_blame") {
  
  y0 <- data[[outcome]][data[[group_var]] == 0]
  y1 <- data[[outcome]][data[[group_var]] == 1]
  
  # Remove missing values
  y0 <- na.omit(y0)
  y1 <- na.omit(y1)
  
  n0 <- length(y0); n1 <- length(y1)
  m0 <- mean(y0);   m1 <- mean(y1)
  sd0 <- sd(y0);    sd1 <- sd(y1)
  
  # Run the test
  tt <- t.test(y1, y0, var.equal = FALSE)
  
  # Effect size calculation
  sp <- sqrt(((n0 - 1)*sd0^2 + (n1 - 1)*sd1^2) / (n0 + n1 - 2))
  d  <- (m1 - m0) / sp
  J  <- 1 - (3 / (4*(n0 + n1) - 9))
  g  <- J * d
  
  # Return a data frame with all the info
  data.frame(
    scenario   = outcome,
    n_low      = n0,
    n_high     = n1,
    mean_low   = m0,
    mean_high  = m1,
    mean_diff  = m1 - m0,
    t_value    = as.numeric(tt$statistic),
    df         = as.numeric(tt$parameter),
    p_value    = tt$p.value,
    ci_low     = tt$conf.int[1],
    ci_high    = tt$conf.int[2],
    hedges_g   = g
  )
}

results_table <- do.call(
  rbind,
  lapply(feeling_vars, function(v) {
    run_welch_effect(clean_data, v)
  })
)

results_table

############################################################
## Save result: exploratory_ttests_all_scenarios
############################################################
write.csv(
  results_table,
  file.path(output, "exploratory_ttests_all_scenarios.csv"),
  row.names = FALSE
)

############################################################
## H2: The emotional difference between "other apologizes first"
## and "self apologizes first" differs by self-blame level.
## (testing for an interaction effect)
############################################################

# Create difference score: how much better do people feel when the other person apologizes first?
clean_data <- clean_data %>%
  mutate(
    either_first_feeling =
      feelings_boththemfirst - feelings_bothyoufirst
  )

# Test if this difference varies by self-blame group
tt_H2 <- t.test(
  either_first_feeling ~ high_blame,
  data = clean_data,
  var.equal = FALSE
)

tt_H2

############################################################
## H2: Calculate effect size
############################################################

# Extract difference scores for both groups
diff0 <- clean_data$either_first_feeling[clean_data$high_blame == 0]
diff1 <- clean_data$either_first_feeling[clean_data$high_blame == 1]

# Remove NA values
diff0 <- na.omit(diff0)
diff1 <- na.omit(diff1)

# Calculate descriptive statistics
n0_h2 <- length(diff0)
n1_h2 <- length(diff1)
m0_h2 <- mean(diff0)
m1_h2 <- mean(diff1)
sd0_h2 <- sd(diff0)
sd1_h2 <- sd(diff1)

# Calculate pooled standard deviation
sp_h2 <- sqrt(((n0_h2 - 1)*sd0_h2^2 + (n1_h2 - 1)*sd1_h2^2) / (n0_h2 + n1_h2 - 2))

# Cohen's d
d_h2 <- (m1_h2 - m0_h2) / sp_h2

# Hedges' g (small sample correction)
J_h2 <- 1 - (3 / (4*(n0_h2 + n1_h2) - 9))
g_h2 <- J_h2 * d_h2

############################################################
## Save result: H2 (updated with effect size)
############################################################

H2_summary <- data.frame(
  hypothesis = "H2",
  outcome    = "other_first_minus_self_first",
  n_low      = n0_h2,
  n_high     = n1_h2,
  mean_low   = m0_h2,
  mean_high  = m1_h2,
  mean_diff  = m1_h2 - m0_h2,
  t_value    = as.numeric(tt_H2$statistic),
  df         = as.numeric(tt_H2$parameter),
  p_value    = tt_H2$p.value,
  ci_low     = tt_H2$conf.int[1],
  ci_high    = tt_H2$conf.int[2],
  hedges_g   = g_h2
)

write.csv(
  H2_summary,
  file.path(output, "H2_difference_ttest.csv"),
  row.names = FALSE
)

############################################################
## H3: People who are more sensitive to the order of apologies 
## feel less emotionally positive even when apologies are mutual, 
## and this pattern differs between high and low self-blame individuals.
############################################################

# Compute average feeling across both mutual apology scenarios
# Also compute order sensitivity (how much does order matter to this person?)

clean_data <- clean_data %>%
  mutate(
    mean_mutual = rowMeans(
      cbind(feelings_bothyoufirst, feelings_boththemfirst),
      na.rm = TRUE
    ),
    order_sensitivity =
      abs(feelings_bothyoufirst - feelings_boththemfirst)
  )

# Regression with interaction term
model_H3 <- lm(
  mean_mutual ~ order_sensitivity * high_blame,
  data = clean_data
)

summary(model_H3)

############################################################
## H3: Calculate effect size
############################################################

# Get R-squared for full model
r2_full <- summary(model_H3)$r.squared

# Fit model without interaction term
model_H3_no_interaction <- lm(
  mean_mutual ~ order_sensitivity + high_blame,
  data = clean_data
)
r2_no_interaction <- summary(model_H3_no_interaction)$r.squared

# Partial R-squared for interaction
partial_r2_interaction <- r2_full - r2_no_interaction

# Cohen's f for interaction
cohens_f_interaction <- sqrt(partial_r2_interaction / (1 - r2_full))

# Calculate standardized coefficients (beta)
clean_data_std <- clean_data %>%
  mutate(
    order_sensitivity_std = scale(order_sensitivity)[,1],
    mean_mutual_std = scale(mean_mutual)[,1]
  )

model_H3_std <- lm(
  mean_mutual_std ~ order_sensitivity_std * high_blame,
  data = clean_data_std
)

############################################################
## Save result: H3 (updated with effect size)
############################################################

H3_coefficients <- tidy(model_H3) %>%
  mutate(
    std_estimate = coef(model_H3_std)
  )

H3_model_fit <- glance(model_H3) %>%
  mutate(
    partial_r2_interaction = partial_r2_interaction,
    cohens_f_interaction = cohens_f_interaction
  )

write.csv(
  H3_coefficients,
  file.path(output, "H3_regression_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  H3_model_fit,
  file.path(output, "H3_regression_model_fit.csv"),
  row.names = FALSE
)

############################################################
## Summary table: Effect sizes for all hypotheses
############################################################

effect_size_summary <- data.frame(
  hypothesis = c("H1", "H2", "H3"),
  effect_type = c("Hedges' g", "Hedges' g", "Cohen's f (interaction)"),
  effect_size = c(g, g_h2, cohens_f_interaction),
  interpretation = c(
    ifelse(abs(g) < 0.2, "negligible",
           ifelse(abs(g) < 0.5, "small",
                  ifelse(abs(g) < 0.8, "medium", "large"))),
    ifelse(abs(g_h2) < 0.2, "negligible",
           ifelse(abs(g_h2) < 0.5, "small",
                  ifelse(abs(g_h2) < 0.8, "medium", "large"))),
    ifelse(abs(cohens_f_interaction) < 0.1, "small",
           ifelse(abs(cohens_f_interaction) < 0.25, "medium", "large"))
  )
)

write.csv(
  effect_size_summary,
  file.path(output, "all_hypotheses_effect_sizes.csv"),
  row.names = FALSE
)

print(effect_size_summary)
