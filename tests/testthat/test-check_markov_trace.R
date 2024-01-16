
# Create trace that conforms:
v_hs_names <- c("H", "S", "D") # Three health states
n_hs <- length(v_hs_names)
n_t <- 10 # 10 time points
# matrix structure
m_TR <-
  matrix(
    data = NA,
    nrow = n_t,
    ncol = n_hs,
    dimnames = list(NULL, v_hs_names)
  )
# dummy values that conform (dead proportion increase monotonically)
m_TR[, "H"] <- seq(1, 0, length.out = n_t)
m_TR[, "S"] <- seq(0, 0.5, length.out = n_t)
m_TR[, "D"] <- 1 - m_TR[, "H"] - m_TR[, "S"]

# create markov traces in which:
# - the matrix contains non numeric values
m_TR_non_numeric <- m_TR
m_TR_non_numeric[1, 1] <- "a"

# - the matrix contains values outside the range 0-1
m_TR_outside_range <- m_TR
m_TR_outside_range[1, 1] <- -1
m_TR_outside_range[1, 2] <- 2 # done so the row still sums to 1

# - the matrix contains rows that do not sum to 1
m_TR_rows_not_sum_to_1 <- m_TR
m_TR_rows_not_sum_to_1[1, 1] <- 0.5

# - the matrix contains dead state values that do not increase monotonically
m_TR_dead_not_monotonic <- m_TR
m_TR_dead_not_monotonic[5, "D"] <- 0.5

# remove unnecessary objects
rm(n_hs, n_t, v_hs_names)


#===========#
# Run tests #
#===========#

test_that(desc = "check_markov_trace shouldn't issue warnings for valid input",
          code = {
            expect_silent(check_markov_trace(m_TR))
          })


test_that(desc = "check_markov_trace should issue error for non-numeric values",
          code = {

            expect_error(
              check_markov_trace(m_TR = m_TR_non_numeric, stop_if_not = T)
            )

          })

test_that(desc = "check_markov_trace should issue warning & error for values outside the range 0-1",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_outside_range, stop_if_not = F)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_outside_range, stop_if_not = T)
            )

          })


test_that(desc = "check_markov_trace should issue warning & error for rows that do not sum to 1",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_rows_not_sum_to_1, stop_if_not = F)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_rows_not_sum_to_1, stop_if_not = T)
            )

          })



test_that(desc = "check_markov_trace should issue warning & error for dead state values that do not increase monotonically",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_dead_not_monotonic, stop_if_not = F)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_dead_not_monotonic, stop_if_not = T)
            )

          })
