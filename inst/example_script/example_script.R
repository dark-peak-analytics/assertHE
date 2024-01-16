# Example R script which calls the foo.
v_hs_names <- c("H", "S", "D")
n_hs <- length(v_hs_names)
n_t <- 10
m_TR <- matrix(data = NA,
              nrow = n_t,
              ncol = n_hs,
              dimnames = list(NULL, v_hs_names))

m_TR[, "H"] <- seq(1, 0, length.out = n_t)
m_TR[, "S"] <- seq(0, 0.5, length.out = n_t)
m_TR[, "D"] <- 1 - m_TR[, "H"] - m_TR[, "S"]

check_markov_trace(m_TR = m_TR, dead_state = "D", confirm_ok = T)

m_TR[10, "D"] <- 0
m_TR[9, "S"] <- 1

check_markov_trace(m_TR = m_TR, stop_if_not = T, dead_state = "D", confirm_ok = T)


# test function 1
foo1 <- function(x){
  x
}

foo1 (10)

#
any_of(c("H", "S", "D"))
