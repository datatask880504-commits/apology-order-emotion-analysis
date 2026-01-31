# ============================================
# 01_Clean Data
# ============================================

library(readxl)
library(dplyr)

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


load(file.path(data_processed, "processed_data.RData"))

############################################################
## Clean data (Sanity-check is done and shown in 
## script called  00_Appendix: Data Quality Checks)
## Here is focus on dropping variables unnessary for analysis
############################################################

# Note: These column indices correspond to metadata, system-generated fields,
# and other variables that are not relevant for analysis, so they are removed here
df_data <- processed_data %>%
  select(-c(1:6), -c(8:17), -c(19:26), -c(62, 64, 65, 68, 69, 70))

clean_data <- df_data %>%
  filter(Finished == "TRUE", consent == "AGREE", passedattn == "yes") %>%
  select(-real_imaginary, -initials_box, -describe, -real_imaginary, -describe, 
         -feelings_exp, -attention_1, -attention_1_TEXT, -attention_2, -attention_2_TEXT,
         -attention_3, -attention_3_TEXT, -target_sex_3_TEXT, -sex_3_TEXT, -comments,
         -consent, -Finished, -passedattn, -initials, -initiator_type) %>%
  # Some variables were not shown to participants in the survey interface,
  # so they are removed here to avoid ambiguity in interpretation
  mutate(id = row_number()) %>%
  # Create a high vs. low self-blame grouping variable using a median split at 50
  # This step distinguishes participants by their tendency toward self-blame
  # While dichotomizing a continuous variable has methodological limitations,
  # a binary grouping (0/1) is used here for simplicity and time constraints
  mutate(high_blame = if_else(blame_1 > 50, 1, 0)) %>%
  relocate(id)  # Move ID to the first column for easier inspection and reference



# Save cleaned dataset
save(
  clean_data,
  file = file.path(data_clean, "clean_data.RData")
)

# Quick check
dim(clean_data)

