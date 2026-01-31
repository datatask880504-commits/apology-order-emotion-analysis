# ============================================
# 02_Descriptive Analysis
# ============================================

library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)


############################################################
## Root 
############################################################

root <- "/Users/pei-chin/Research/Behavioral Science and Marketing_data_task"
data_clean  <- file.path(root, "data_clean")
output     <- file.path(root, "output")
figures    <- file.path(output, "figures")  


load(file.path(data_clean, "clean_data.RData"))
############################################################
## Variable check
############################################################

feeling_vars <- c(
  "feelings_youalone",
  "feelings_bothyoufirst",
  "feelings_themalone",
  "feelings_boththemfirst",
  "feelings_neither",
  "feelings_youaloneforgiven"
)


summary(clean_data[, feeling_vars])
#see how many people are in the high-self blame group (blame_1 > 50 = high-self group)

feeling_complete_n <- sapply(
  feeling_vars,
  function(var) sum(!is.na(clean_data[[var]]))
)

print(feeling_complete_n)


table(clean_data$high_blame)

# Breaking down feeling ratings by self-blame group
by(
  clean_data[, feeling_vars],
  clean_data$high_blame,
  summary
)

# Counting preferences — first set of binary choice questions
sum(clean_data$prefer_I_apologize_first)  # participants who prefer apologizing first
sum(clean_data$prefer_Neither_I_nor)      # participants who prefer neither option

# Counting preferences — second set of binary choice questions
sum(clean_data$prefer_feelings_youalone)   # participants who prefer apologizing alone
sum(clean_data$prefer__Neither_I_nor_2)    # participants who prefer neither option

# Break preferences down by self-blame group
# First preference set
clean_data %>%
  group_by(high_blame) %>%
  summarise(
    I_apologize_first_1 = sum(prefer_I_apologize_first, na.rm = TRUE),
    Neither_1 = sum(prefer_Neither_I_nor, na.rm = TRUE),
    n = n()
  )

# Second preference set — used to check whether patterns are consistent
clean_data %>%
  group_by(high_blame) %>%
  summarise(
    I_apologize_alone_2 = sum(prefer_feelings_youalone, na.rm = TRUE),
    Neither_2 = sum(prefer__Neither_I_nor_2, na.rm = TRUE),
    n = n()
  )




############################################################
## Visualize these results
############################################################

plot_data_all <- clean_data %>%
  select(all_of(feeling_vars)) %>%
  pivot_longer(
    cols = all_of(feeling_vars),
    names_to = "scenario",
    values_to = "feeling"
  )

# grouped by blame level
plot_data_group <- clean_data %>%
  select(high_blame, all_of(feeling_vars)) %>%
  pivot_longer(
    cols = all_of(feeling_vars),
    names_to = "scenario",
    values_to = "feeling"
  )

# First plot - overall distribution across all scenarios
p1 <- ggplot(plot_data_all, aes(x = scenario, y = feeling)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Overall Distribution",
    x = "Apology scenario",
    y = "Emotional positivity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # tilting labels so they don't overlap
  )


# Second plot - breaking it down by self-blame groups
p2 <- ggplot(plot_data_group, aes(
  x = scenario,
  y = feeling,
  fill = factor(high_blame)
)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Grouped by Self-Blame",
    x = "Apology scenario",
    y = "Emotional positivity",
    fill = "High self-blame"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


p_all <- p1 + p2 + plot_layout(guides = "collect")
p_all

ggsave(
  filename = file.path(figures, "feeling_distribution.png"),
  plot = p_all
  
)