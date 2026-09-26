


# Function to run Markov Simulation
run_markov <- function(P, annual_drug_cost) {
  # Create Trace Matrix
  trace <- matrix(0, nrow = n_cycles + 1, ncol = 3)
  colnames(trace) <- c("Stable", "CV_Event", "Dead")
  
  # Starting state: 100% of cohort starts in 'Stable'
  trace[1, ] <- c(1, 0, 0)
  
  # Simulate cohort across cycles
  for (t in 1:n_cycles) {
    trace[t + 1, ] <- trace[t, ] %*% P
  }
  
  # Vectors for Costs and Utilities per state
  state_costs <- c(c_stable + annual_drug_cost, c_cv_event, c_dead)
  state_utilities <- c(u_stable, u_cv_event, u_dead)
  
  # Discount vectors over time
  cycle_vec <- 0:n_cycles
  discount_factors <- 1 / ((1 + r)^cycle_vec)
  
  # Calculate expected undiscounted & discounted outcomes
  cycle_costs <- (trace %*% state_costs) * discount_factors
  cycle_qalys <- (trace %*% state_utilities) * discount_factors
  
  total_cost <- sum(cycle_costs)
  total_qaly <- sum(cycle_qalys)
  
  return(list(trace = as.data.frame(trace), total_cost = total_cost, total_qaly = total_qaly))
}

# Execute simulations
sim_std   <- run_markov(P_std, annual_drug_cost = 0)
sim_novel <- run_markov(P_novel, annual_drug_cost = c_drug_novel)



# Incremental Analysis
delta_cost_markov <- sim_novel$total_cost - sim_std$total_cost
delta_qaly_markov <- sim_novel$total_qaly - sim_std$total_qaly

icer_qaly <- delta_cost_markov / delta_qaly_markov

# Display Output
cat("=== MARKOV MODEL RESULTS (20-Year Horizon) ===\n")
cat("Standard Care Total Cost: $", round(sim_std$total_cost, 2), " | QALYs:", round(sim_std$total_qaly, 3), "\n")
cat("Novel SGLT2i Total Cost:  $", round(sim_novel$total_cost, 2), " | QALYs:", round(sim_novel$total_qaly, 3), "\n")
cat("Incremental Cost ($):    $", round(delta_cost_markov, 2), "\n")
cat("Incremental QALYs:        ", round(delta_qaly_markov, 4), "\n")
cat("ICER ($/QALY gained):     $", round(icer_qaly, 2), "/QALY\n")



# Prepare trace data for plotting
trace_df <- sim_novel$trace %>%
  mutate(Cycle = 0:n_cycles) %>%
  pivot_longer(cols = c("Stable", "CV_Event", "Dead"), names_to = "State", values_to = "Proportion")

ggplot(trace_df, aes(x = Cycle, y = Proportion, color = State, linetype = State)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "20-Year Markov Cohort Trace (Novel SGLT2i Arm)",
    subtitle = "Proportion of cohort in each health state over time",
    x = "Cycle (Years)",
    y = "Percentage of Cohort"
  )


