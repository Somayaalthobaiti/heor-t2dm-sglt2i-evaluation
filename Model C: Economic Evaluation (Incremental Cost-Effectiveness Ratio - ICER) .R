# Calculate mean costs and hospitalization rates by group
economic_summary <- claims_clean %>%
  group_by(treatment) %>%
  summarise(
    mean_cost = mean(total_cost),
    hosp_rate = mean(hospitalized)
  )

cost_standard <- economic_summary$mean_cost[economic_summary$treatment == "Standard_Care"]
cost_novel    <- economic_summary$mean_cost[economic_summary$treatment == "Novel_SGLT2i"]

hosp_standard <- economic_summary$hosp_rate[economic_summary$treatment == "Standard_Care"]
hosp_novel    <- economic_summary$hosp_rate[economic_summary$treatment == "Novel_SGLT2i"]

delta_cost   <- cost_novel - cost_standard
delta_effect <- hosp_standard - hosp_novel # Hospitalizations avoided (positive outcome)

icer <- delta_cost / delta_effect

cat("Incremental Cost ($):", round(delta_cost, 2), "\n")
cat("Incremental Effectiveness (Hospitalizations Avoided per patient):", round(delta_effect, 4), "\n")
cat("ICER ($ per hospitalization avoided):", round(icer, 2), "\n")