library(tidyverse)

# 1. Define Master Function to Compute ICER given inputs
calc_icer <- function(c_drug_novel = 3200, 
                      rr_sglt2i = 0.68, 
                      c_cv_event = 18000, 
                      u_stable = 0.85, 
                      r = 0.03) {
  n_cycles <- 20
  c_stable <- 2500
  c_dead <- 0
  u_cv_event <- 0.65
  u_dead <- 0.00
  p_stable_to_cv <- 0.08
  p_stable_to_dead <- 0.015
  p_cv_to_dead <- 0.12
  
  # Transition Matrices
  P_std <- matrix(
    c(1 - p_stable_to_cv - p_stable_to_dead, p_stable_to_cv, p_stable_to_dead,
      0, 1 - p_cv_to_dead, p_cv_to_dead,
      0, 0, 1), nrow = 3, byrow = TRUE
  )
  
  P_novel <- matrix(
    c(1 - (p_stable_to_cv * rr_sglt2i) - p_stable_to_dead, p_stable_to_cv * rr_sglt2i, p_stable_to_dead,
      0, 1 - p_cv_to_dead, p_cv_to_dead,
      0, 0, 1), nrow = 3, byrow = TRUE
  )
  
  # Internal Markov Runner
  run_mkv <- function(P, drug_c) {
    tr <- matrix(0, nrow = n_cycles + 1, ncol = 3)
    tr[1, ] <- c(1, 0, 0)
    for(t in 1:n_cycles) tr[t + 1, ] <- tr[t, ] %*% P
    
    c_vec <- c(c_stable + drug_c, c_cv_event, c_dead)
    u_vec <- c(u_stable, u_cv_event, u_dead)
    disc <- 1 / ((1 + r)^(0:n_cycles))
    
    tc <- sum((tr %*% c_vec) * disc)
    tq <- sum((tr %*% u_vec) * disc)
    return(c(tc, tq))
  }
  
  res_std <- run_mkv(P_std, 0)
  res_novel <- run_mkv(P_novel, c_drug_novel)
  
  d_cost <- res_novel[1] - res_std[1]
  d_qaly <- res_novel[2] - res_std[2]
  
  return(d_cost / d_qaly)
}

# 2. Base Case ICER
base_icer <- calc_icer()

# 3. Define OWSA Parameter Ranges
owsa_data <- tibble::tribble(
  ~parameter, ~low_icer, ~high_icer,
  "Novel Drug Annual Cost ($2,000 - $4,500)", calc_icer(c_drug_novel = 2000), calc_icer(c_drug_novel = 4500),
  "SGLT2i Relative Risk (0.50 - 0.85)",       calc_icer(rr_sglt2i = 0.50), calc_icer(rr_sglt2i = 0.85),
  "CV Event Cost ($12,000 - $25,000)",        calc_icer(c_cv_event = 25000), calc_icer(c_cv_event = 12000), # inverted order for cost offset
  "Stable Utility Weight (0.75 - 0.95)",      calc_icer(u_stable = 0.95), calc_icer(u_stable = 0.75),
  "Annual Discount Rate (0% - 5%)",           calc_icer(r = 0.00), calc_icer(r = 0.05)
)

# 4. Prepare Data for Tornado Plot
owsa_data <- owsa_data %>%
  mutate(
    width = abs(high_icer - low_icer),
    parameter = fct_reorder(parameter, width) # Order parameters by impact width
  )

# 5. Render Tornado Diagram
ggplot(owsa_data) +
  geom_segment(aes(x = parameter, xend = parameter, y = low_icer, yend = high_icer), 
               color = "steelblue", size = 8, alpha = 0.85) +
  geom_hline(yintercept = base_icer, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "One-Way Sensitivity Analysis: Tornado Diagram",
    subtitle = paste0("Red dashed line represents Base-Case ICER ($", round(base_icer, 2), "/QALY)"),
    x = "Model Parameter (Tested Range)",
    y = "Incremental Cost-Effectiveness Ratio (ICER in $/QALY)"
  )