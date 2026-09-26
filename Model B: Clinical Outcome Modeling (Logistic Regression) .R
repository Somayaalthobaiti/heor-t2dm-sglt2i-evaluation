# Fit Logistic Regression for hospitalization risk
hosp_model <- glm(
  hospitalized ~ treatment + age + sex + charlson_ci,
  family = binomial(link = "logit"),
  data = claims_clean
)

summary(hosp_model)

# Extract Odds Ratios (ORs)
odds_ratios <- exp(cbind(OR = coef(hosp_model), confint(hosp_model)))
print(odds_ratios)