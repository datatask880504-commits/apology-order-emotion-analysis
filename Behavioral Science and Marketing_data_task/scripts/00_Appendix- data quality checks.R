# ============================================
# 00_Appendix: Data Quality Checks
# ============================================

library(dplyr)
library(readxl)

############################################################
## Root 
############################################################
root <- "/Users/pei-chin/Research/Behavioral Science and Marketing_data_task"

data_raw   <- file.path(root, "data_raw")
data_processed <- file.path(root, "data_processed")
data_clean <- file.path(root, "data_clean")
scripts    <- file.path(root, "scripts")
output     <- file.path(root, "output")
figures    <- file.path(output, "figures")

# Load the Excel file - sheet 1 has variable definitions, sheet 2 has actual data
df_variable <- read_excel(
  file.path(data_raw, "Data - 2026.xlsx"),
  sheet = 1
)

raw_data <- read_excel(file.path(data_raw, "Data - 2026.xlsx"), sheet = 2)

############################################################
## A. Sanity-check ranges and missingness
############################################################

dim(raw_data)

# 1. Check feeling variables (should be -30 to +30 based on survey)
# No observations are removed in this step.

feeling_vars <- c(
  "feelings_youalone",
  "feelings_bothyoufirst",
  "feelings_themalone",
  "feelings_boththemfirst",
  "feelings_neither",
  "feelings_youaloneforgiven"
)

for (var in feeling_vars) {
  min_val <- min(raw_data[[var]], na.rm = TRUE)
  max_val <- max(raw_data[[var]], na.rm = TRUE)
  n_missing <- sum(is.na(raw_data[[var]]))
  in_range <- min_val >= -30 & max_val <= 30
  
  print(
    data.frame(
      variable = var,
      min = min_val,
      max = max_val,
      missing = n_missing,
      in_range = in_range
    )
  )
}


# 2. Recompute emotional positivity rankings

# The platform-generated feeling_DO variables do not reflect
# the true ordering of emotional positivity.
# here I recompute rankings based on the raw feeling scores based on feeling_vars
# and overwrite the original ranking variables.

# Raw feeling score variables (used to compute rankings)
feeling_vars <- c(
  "feelings_youalone",
  "feelings_bothyoufirst",
  "feelings_themalone",
  "feelings_boththemfirst",
  "feelings_neither",
  "feelings_youaloneforgiven"
)

# Original (incorrect) ranking variables 
do_vars <- c(
  "feelings_DO_1",
  "feelings_DO_2",
  "feelings_DO_3",
  "feelings_DO_4",
  "feelings_DO_5",
  "feelings_DO_6"
)


n <- nrow(raw_data)


for (i in 1:n) {
  
  # Extract raw feeling scores for participant i
  x <- as.numeric(raw_data[i, feeling_vars])
  
  # Rank feelings in descending order
  # Higher emotional positivity receives a lower rank value
  # Ties are assigned the minimum rank if the emotional positivity is the same.
  raw_data[i, do_vars] <- as.list(rank(-x, ties.method = "min"))
}


# 3.  Handle outcome ordering for binary choice questions (similar issues as previous ones)
# randomization order doesn't match so creating binary indicators for each choice option

# --------------------------------------------------
# Binary choice 1: I apologize first vs. Neither apologizes
# --------------------------------------------------
raw_data$outcome_binary1_DO_1 <- ifelse(
  grepl("^I apologize first", raw_data$outcome_binary1),
  1, 0
)

raw_data$outcome_binary1_DO_2 <- ifelse(
  grepl("Neither I nor ", raw_data$outcome_binary1),
  1, 0
)

# --------------------------------------------------
# Binary choice 2: Other doesn't apologize after vs. Neither apologizes
# --------------------------------------------------
raw_data$outcome_binary2_DO_1 <- ifelse(
  grepl("does not apologize after that", raw_data$outcome_binary2),
  1, 0
)

raw_data$outcome_binary2_DO_2 <- ifelse(
  grepl("Neither I nor", raw_data$outcome_binary2),
  1, 0
)


# Rename variables (after all binaries are created)

raw_data <- raw_data %>%
  rename(
    prefer_I_apologize_first = outcome_binary1_DO_1,
    prefer_Neither_I_nor = outcome_binary1_DO_2,
    prefer_feelings_youalone = outcome_binary2_DO_1,
    prefer__Neither_I_nor_2 = outcome_binary2_DO_2  # Note: keeping the extra underscore typo for consistency
  )

# 4. Check blame variable (should be 0 to 100 based on survey slider)

max_blame <- max(raw_data$blame_1, na.rm = TRUE)
max_blame
min_blame <- min(raw_data$blame_1, na.rm = TRUE)
min_blame
n_missing <- sum(is.na(raw_data$blame_1))
n_missing

# 5. Check demographic variables

print(table(raw_data$target_sex, useNA = "ifany"))
print(table(raw_data$sex, useNA = "ifany"))

print(min(raw_data$age, na.rm = TRUE))
print(max(raw_data$age, na.rm = TRUE))
summary(raw_data$age)
print(sum(is.na(raw_data$age)))


# Drop obervations which are invalid basd on checking above
processed_data <- raw_data[complete.cases(raw_data$feelings_youalone), ]
processed_data <- processed_data %>%
  filter(age < 100) # age over 100 is suspicious

# 6. Overall missingness summary
count_na <- function(x) {
  sum(is.na(x))
}

sapply(processed_data, count_na)


# Save cprocessed_data
save(
  processed_data,
  file = file.path(data_processed, "processed_data.RData")
)


############################################################
## B. WITHIN- VS BETWEEN-SUBJECT STRUCTURE
## (some code adapted from 02_descriptive_analysis)
############################################################
# Purpose:
# Here, I use response patterns (i.e., non-missing values) to confirm that
# the observed data structure aligns with the intended survey design.

# ----------------------------------------------------------
# 1. Feeling ratings: within-subject
# ----------------------------------------------------------
# To verify this, I count the number of non-missing responses for each
# feeling variable. The presence of valid responses across multiple
# conditions for the same participants confirms a within-subject design.
# I also inspect basic descriptive statistics for these variables.

feeling_vars <- c(
  "feelings_youalone",
  "feelings_bothyoufirst",
  "feelings_themalone",
  "feelings_boththemfirst",
  "feelings_neither",
  "feelings_youaloneforgiven"
)

summary(clean_data[, feeling_vars])

# Check how many participants fall into the high self-blame group
# (defined as blame_1 > 50)
feeling_complete_n <- sapply(
  feeling_vars,
  function(var) sum(!is.na(clean_data[[var]]))
)

print(feeling_complete_n)

# ----------------------------------------------------------
# 2. Self-blame grouping: between-subject
# ----------------------------------------------------------
# Self-blame is treated as a stable, individual-level characteristic and
# is used to classify participants into high vs. low self-blame groups.
# Each participant should belong to exactly one group, indicating a
# between-subject structure.
#
# To confirm this, I inspect the distribution of the self-blame grouping
# variable and verify that each observation is assigned to a single category.
table(clean_data$high_blame)

# ----------------------------------------------------------
# 3. Binary choice questions: within-subject
# ----------------------------------------------------------
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
## C. VERIFY LEVELS OF VARIABLES
############################################################
# Purpose:
# Verify that categorical and grouping variables have the expected
# levels and coding before analysis.

# ----------------------------------------------------------
# 1. Binary choice variables
# ----------------------------------------------------------

# Original text responses
table(clean_data$outcome_binary1, useNA = "ifany")
table(clean_data$outcome_binary2, useNA = "ifany")

# Recoded binary indicators
table(clean_data$prefer_I_apologize_first, useNA = "ifany")
table(clean_data$prefer_Neither_I_nor, useNA = "ifany")
table(clean_data$prefer_feelings_youalone, useNA = "ifany")
table(clean_data$prefer__Neither_I_nor_2, useNA = "ifany")


# ----------------------------------------------------------
# 2. Self-blame grouping variable
# ----------------------------------------------------------

# Distribution of original self-blame score
summary(clean_data$blame_1)

# ----------------------------------------------------------
# 3. Gender variables
# ----------------------------------------------------------

table(clean_data$sex, useNA = "ifany")
table(clean_data$target_sex, useNA = "ifany")


############################################################
## D. Missing Data Handling
############################################################

# Before running any analyses, the data were organized into three stages:
# raw, processed, and clean. This structure is used to keep data-cleaning
# steps transparent and to clearly separate data quality decisions from
# analysis-related decisions.

# ----------------------------------------------------------
# Raw → Processed Data
# ----------------------------------------------------------
# The transition from the raw dataset to the processed dataset is documented
# in 00_Appendix: Data Quality Checks, Section A (Sanity-check ranges and
# missingness). At this stage, the data were screened for obvious validity
# issues by confirming that scale-based variables fell within their expected
# ranges and that demographic values were plausible (e.g., excluding ages
# over 100).
#
# Several variables were found not to align with the intended survey logic
# due to randomization order (e.g., emotional positivity rankings and binary
# choice variables). These variables were reconstructed directly from the
# original response values.
#
# Importantly, no variables were removed at this stage; only observations
# with clearly invalid data were excluded. The resulting dataset is referred
# to as the processed dataset.

# ----------------------------------------------------------
# Processed → Clean Data
# ----------------------------------------------------------
# The clean dataset was constructed by applying the study’s inclusion
# criteria, including survey completion, consent, and attention checks.
# At this stage, variables that were not required for analysis were removed,
# and analysis-specific variables were created.
#
# The clean dataset represents the final analysis sample. It is used for
# all subsequent analyses, as well as for verifying the within- versus
# between-subject structure of the study and confirming that all key
# variables have the expected levels and coding prior to analysis.
############################################################

