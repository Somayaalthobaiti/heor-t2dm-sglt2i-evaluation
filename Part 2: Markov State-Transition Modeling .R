# ---------------------------------------------------------
# 1. Model Parameters & Inputs
# ---------------------------------------------------------
n_cycles <- 20
r <- 0.03 # 3% annual discount rate

# Health State Costs (Annual)
c_stable <- 2500
c_cv_event <- 18000
c_dead <- 0
c_drug_novel <- 3200 # Additional drug cost per year for SGLT2i

# Health State Utilities (QALY weights)
u_stable <- 0.85
u_cv_event <- 0.65
u_dead <- 0.00

# Annual Transition Probabilities (Standard Care)
p_stable_to_cv <- 0.08
p_stable_to_dead <- 0.015
p_cv_to_dead <- 0.12

# Relative Risk (RR) of CV Event with Novel SGLT2i
rr_sglt2i <- 0.68

# ---------------------------------------------------------
# 2. Build Transition Probability Matrices
# ---------------------------------------------------------

# Matrix A: Standard Care
P_std <- matrix(
  c(
    1 - p_stable_to_cv - p_stable_to_dead, p_stable_to_cv, p_stable_to_dead,
    0, 1 - p_cv_to_dead, p_cv_to_dead,
    0, 0, 1
  ),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Stable", "CV_Event", "Dead"), c("Stable", "CV_Event", "Dead"))
)

# Matrix B: Novel SGLT2i (Reduced probability of CV Event)
p_stable_to_cv_novel <- p_stable_to_cv * rr_sglt2i

P_novel <- matrix(
  c(
    1 - p_stable_to_cv_novel - p_stable_to_dead, p_stable_to_cv_novel, p_stable_to_dead,
    0, 1 - p_cv_to_dead, p_cv_to_dead,
    0, 0, 1
  ),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Stable", "CV_Event", "Dead"), c("Stable", "CV_Event", "Dead"))
)