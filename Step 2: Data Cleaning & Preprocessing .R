# Check missing data summary
colSums(is.na(claims_raw))

# Data Cleaning Pipeline
claims_clean <- claims_raw %>%
  # 1. Handle missing continuous values with median imputation
  mutate(bmi = ifelse(is.na(bmi), median(bmi, na.rm = TRUE), bmi)) %>%
  
  # 2. Format categorical variables and set explicit baseline reference groups
  mutate(
    sex = factor(sex, levels = c("Female", "Male")),
    treatment = factor(treatment, levels = c("Standard_Care", "Novel_SGLT2i")),
    hospitalized_factor = factor(hospitalized, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  
  # 3. Filter out non-positive costs (data integrity check)
  filter(total_cost > 0)

# Verify clean dataset structure
glimpse(claims_clean)