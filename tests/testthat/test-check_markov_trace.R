
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
m_TR_dead_not_monotonic[5, "H"] <- 1 - m_TR_dead_not_monotonic[5, "D"] - m_TR_dead_not_monotonic[5, "S"]

# - the matrix has 2 dimensions
m_TR_3d <- array(data = 1, dim = c(n_t, n_hs, 10))
m_TR_1d <- rep(1, n_t)

# adjust column names
m_TR_missing_column_name <- m_TR
colnames(m_TR_missing_column_name) <- c("H", "S", NA)
m_TR_duplicate_column_name <- m_TR
colnames(m_TR_duplicate_column_name) <- c("H", "S", "H")

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
              check_markov_trace(m_TR = m_TR_non_numeric, stop_if_not = TRUE)
            )

          })

test_that(desc = "check_markov_trace should issue warning & error for values outside the range 0-1",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_outside_range, stop_if_not = FALSE)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_outside_range, stop_if_not = TRUE)
            )

          })


test_that(desc = "check_markov_trace should issue warning & error for rows that do not sum to 1",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_rows_not_sum_to_1, stop_if_not = FALSE)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_rows_not_sum_to_1, stop_if_not = TRUE)
            )

          })



test_that(desc = "check_markov_trace should issue warning & error for dead state values that do not increase monotonically",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_dead_not_monotonic, stop_if_not = FALSE, dead_state = "D")
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_dead_not_monotonic, stop_if_not = TRUE, dead_state = "D")
            )

          })



test_that(desc = "check_markov_trace should issue error for 1d or 3d arrays",
          code = {
            expect_error(
              check_markov_trace(m_TR = m_TR_3d, stop_if_not = TRUE)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_1d, stop_if_not = TRUE)
            )

          })



test_that(desc = "check_markov_trace issues warning for missing column name and error for duplicate column name",
          code = {
            expect_warning(
              check_markov_trace(m_TR = m_TR_missing_column_name, stop_if_not = TRUE)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR_duplicate_column_name, stop_if_not = TRUE)
            )

          })


test_that(desc = "Check confirm OK works as expected",
          code = {
            expect_match(
              check_markov_trace(m_TR = m_TR, stop_if_not = TRUE, confirm_ok = TRUE),
              "Markov Trace passed all checks."
            )

          })
