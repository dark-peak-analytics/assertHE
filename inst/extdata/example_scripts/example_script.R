# basic information:
Strategies     <- c("No Treatment", "Treatment")     # strategy names
n_age_init     <- 25                                 # age at baseline
n_age_max      <- 55                                 # maximum age of follow up
n_t            <- n_age_max - n_age_init             # time horizon, number of cycles
d_r            <- 0.035                              # equal discount of costs and QALYs by 3%

# Transition probabilities (per cycle)
p_HD    <- 0.005           # probability to die when healthy
p_HS1   <- 0.15          	 # probability to become sick when healthy
p_S1H   <- 0.5           	 # probability to become healthy when sick
p_S1S2  <- 0.105         	 # probability to become sicker when sick
hr_S1   <- 3             	 # hazard ratio of death in sick vs healthy
hr_S2   <- 10            	 # hazard ratio of death in sicker vs healthy

# Cost and utility inputs
c_H     <- 2000            # cost of remaining one cycle in the healthy state
c_S1    <- 4000            # cost of remaining one cycle in the sick state
c_S2    <- 15000           # cost of remaining one cycle in the sicker state
c_Trt   <- 12000           # cost of treatment(per cycle)
c_D     <- 0               # cost of being in the death state
u_H     <- 1               # utility when healthy
u_S1    <- 0.75            # utility when sick
u_S2    <- 0.5             # utility when sicker
u_D     <- 0               # utility when dead
u_Trt   <- 0.95            # utility when being treated

# stm_1_probs_calcs

# rate of death in healthy
r_HD    <- - log(1 - p_HD)

# rate of death in sick
r_S1D   <- hr_S1 * r_HD
# rate of death in sicker
r_S2D   <- hr_S2 * r_HD

# probability of death in sick
p_S1D   <- 1 - exp(-r_S1D)
# probability of death in sicker
p_S2D   <- 1 - exp(-r_S2D)

# stm_1_discweight

# calculate discount weight for each cycle
v_dwe <- v_dwc <- 1 / (1 + d_r) ^ (0:(n_t-1))  # discount weight (equal discounting is assumed for costs and effects)


# stm_1_names

v_n  <- c("H", "S1", "S2", "D")               # the 4 states of the model: Healthy (H), Sick (S1), Sicker                                                  (S2), Dead (D)
n_states <- length(v_n)                            # number of health states


# stm_1_transmat

#transition probability matrix for NO treatment
m_P <- matrix(data = NA,
              nrow = n_states,
              ncol = n_states,
              dimnames = list(v_n, v_n))

m_P

### From Healthy
m_P["H", ]  <- c(1 - (p_HS1 + p_HD), p_HS1, 0, p_HD)

### From Sick
m_P["S1", ]  <- c(p_S1H, 1 - (p_S1H + p_S1S2 + p_S1D), p_S1S2, p_S1D)

### From Sicker
m_P["S2", ]   <- c(0, 0, 1 - p_S2D, p_S2D)

### From Dead
m_P["D", ] <- c(0, 0, 0, 1)

# check rows add up to 1
rowSums(m_P)
m_P

# ============================== #
#  Check the probability matrix  #
# ============================== #

check_trans_prob_mat(m_P)

# ============================== #


## @knitr stm_1_trace

# create empty Markov trace
m_TR <- matrix(data = NA,
               nrow = n_t,
               ncol = n_states,
               dimnames = list(1:n_t, v_n))

head(m_TR)      # The head() function enables you to view the top of a table rather than the full matrix

# initialize Markov trace
m_TR[1, ] <- c("H" = 1, "S1" = 0, "S2" = 0, "D" = 0)

head(m_TR)  # head shows us the first six rows by default.

# =================================== #
#  Check the initialisation conforms  #
# =================================== #

check_init(x = m_TR[1, ])

# =================================== #

## @knitr stm_1_runfirst

# Run the model for a single cycle
m_TR[2, ] <- m_TR[1, ] %*% m_P

# Display the first two rows of the markov trace matrix
m_TR[c(1:2), ]



# stm_1_runmod
for (cycle in 2:n_t){ # throughout the number of cycles
  # estimate cycle of Markov trace
  m_TR[cycle, ] <- m_TR[cycle - 1, ] %*% m_P
}

head(m_TR)  # head shows us the first six rows by default.

# ============================== #
#  we can check the markov trace #
# ============================== #

check_markov_trace(m_TR = m_TR, dead_state = "D", confirm_ok = TRUE)

# ============================== #

# check to make sure looks correct
plot(m_TR[, "H"],
     col = "green",
     type = "l",
     ylim = c(0,1) )
lines(m_TR[, "S1"], col = "purple")
lines(m_TR[, "S2"], col = "orange")
lines(m_TR[, "D"], col = "red")


# stm_1_costutilvecs

#  1. Create vectors for the costs and utility of each treatment group
v_u_trt    <- c("H" = u_H, "S1" = u_Trt, "S2" = u_S2, "D" = u_D)
v_u_no_trt <- c("H" = u_H, "S1" = u_S1, "S2" = u_S2, "D" = u_D)
v_c_trt    <- c("H" = c_H, "S1" = c_S1 + c_Trt, "S2" = c_S2 + c_Trt, "D" = c_D)
v_c_no_trt <- c("H" = c_H, "S1" = c_S1, "S2" = c_S2, "D" = c_D)

# stm_1_outputs

#  2. Estimate mean costs and QALYs for each year (hint: need to use matrix multiplication)
v_E_no_trt <- m_TR %*% v_u_no_trt
v_E_trt    <- m_TR %*% v_u_trt
v_C_no_trt <- m_TR %*% v_c_no_trt
v_C_trt    <- m_TR %*% v_c_trt

# stm_1_discount

te_no_trt <- sum(v_E_no_trt * v_dwe)
te_trt    <- sum(v_E_trt    * v_dwe)
tc_no_trt <- sum(v_C_no_trt * v_dwc)
tc_trt    <- sum(v_C_trt    * v_dwc)

# stm_1_results

results <- c(
  "Cost_NoTrt" = tc_no_trt,
  "Cost_Trt"   = tc_trt,
  "QALY_NoTrt" = te_no_trt,
  "QALY_Trt" = te_trt
)

ICER <- (results["Cost_Trt"] - results["Cost_NoTrt"]) / (results["QALY_Trt"] - results["QALY_NoTrt"])

ICER
