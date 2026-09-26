# Fit GLM Gamma log-link model for costs
cost_glm <- glm(
  total_cost ~ treatment + age + sex + charlson_ci,
  family = Gamma(link = "log"),
  data = claims_clean
)

summary(cost_glm)

# Calculate Cost Multipliers (Exponentiated Coefficients)
cost_multipliers <- exp(coef(cost_glm))
conf_intervals   <- exp(confint(cost_glm))
cbind(Multiplier = cost_multipliers, conf_intervals)
