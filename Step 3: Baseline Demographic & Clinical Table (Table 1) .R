# Generate HEOR Table 1
table1 <- claims_clean %>%
  select(age, sex, bmi, charlson_ci, total_cost, hospitalized_factor, treatment) %>%
  tbl_summary(
    by = treatment,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      bmi ~ "Body Mass Index (kg/m²)",
      charlson_ci ~ "Charlson Comorbidity Index",
      total_cost ~ "Total Annual Claims Cost ($)",
      hospitalized_factor ~ "1-Year CV Hospitalization"
    )
  ) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels()

table1