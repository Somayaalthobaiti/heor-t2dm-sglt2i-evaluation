# Install required packages if not already installed
# install.packages(c("tidyverse", "gtsummary", "ggplot2", "MASS"))

library(tidyverse)
library(gtsummary)
library(ggplot2)

# Set seed for reproducible synthetic data generation
set.seed(42)
n <- 1000

# 1. Generate Raw Claims Dataset
claims_raw <- tibble(
  patient_id  = 1:n,
  age         = round(rnorm(n, mean = 63, sd = 8)),
  sex         = sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.52, 0.48)),
  charlson_ci = rpois(n, lambda = 1.4), # Charlson Comorbidity Index (0, 1, 2, ...)
  treatment   = sample(c("Standard_Care", "Novel_SGLT2i"), n, replace = TRUE, prob = c(0.55, 0.45)),
  bmi         = rnorm(n, mean = 29.8, sd = 4.2)
)

# Introduce 4% missing values in BMI to practice data cleaning
claims_raw$bmi[sample(1:n, size = 40)] <- NA

# Simulate realistic health economics outcomes:
# - Novel_SGLT2i increases drug costs but decreases cardiovascular hospitalization risk
claims_raw <- claims_raw %>%
  mutate(
    # Log-cost predictor formula
    log_cost_mu = 8.6 + 0.015 * (age - 60) + 0.20 * charlson_ci + 0.22 * (treatment == "Novel_SGLT2i"),
    # Gamma-distributed healthcare costs (skewed distribution)
    total_cost  = rgamma(n, shape = 2.0, scale = exp(log_cost_mu) / 2.0),
    # Hospitalization probability (binary outcome)
    p_hosp      = 1 / (1 + exp(-(-1.1 + 0.03 * (age - 60) + 0.32 * charlson_ci - 0.60 * (treatment == "Novel_SGLT2i")))),
    hospitalized = rbinom(n, size = 1, prob = p_hosp)
  ) %>%
  select(-log_cost_mu, -p_hosp)

# To load an external CSV dataset instead, use:
# claims_raw <- read_csv("path/to/your/claims_dataset.csv")