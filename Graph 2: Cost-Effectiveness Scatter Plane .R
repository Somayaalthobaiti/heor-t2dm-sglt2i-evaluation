# Generate bootstrap samples to visualize uncertainty around ICER
set.seed(123)
n_boot <- 500
boot_results <- tibble(
  boot_id = 1:n_boot,
  delta_cost = numeric(n_boot),
  delta_effect = numeric(n_boot)
)

for(i in 1:n_boot) {
  sample_data <- claims_clean[sample(1:nrow(claims_clean), replace = TRUE), ]
  res <- sample_data %>% 
    group_by(treatment) %>% 
    summarise(c = mean(total_cost), e = mean(hospitalized), .groups = 'drop')
  
  boot_results$delta_cost[i]   <- res$c[res$treatment == "Novel_SGLT2i"] - res$c[res$treatment == "Standard_Care"]
  boot_results$delta_effect[i] <- res$e[res$treatment == "Standard_Care"] - res$e[res$treatment == "Novel_SGLT2i"]
}

ggplot(boot_results, aes(x = delta_effect, y = delta_cost)) +
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "Figure 2: Cost-Effectiveness Plane (Bootstrap Uncertainty)",
    subtitle = "Quadrant I: Higher Cost & Higher Effectiveness (Trade-off)",
    x = "Incremental Effectiveness (Hospitalizations Avoided)",
    y = "Incremental Cost ($)"
  )