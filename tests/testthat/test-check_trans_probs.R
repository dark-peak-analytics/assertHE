length_greater_than_zero <- function(x) length(x) > 0
# tests on check_trans_prob_mat
test_that(desc = "check_trans_prob_mat shouldn't issue warnings for valid input",
          code =  {

            # named transition matrix
            v_hs_names <- c("H", "S", "D")
            n_hs <- length(v_hs_names)
            m_P <-
              matrix(
                data = 0,
                nrow = n_hs,
                ncol = n_hs,
                dimnames = list(v_hs_names, v_hs_names)
              )

            diag(m_P) <- 1

            check_trans_prob_mat(m_P = m_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_length(n = 0)

            # unnamed matrix

            m_P <-
              matrix(
                data = 1/3,
                nrow = n_hs,
                ncol = n_hs
              )

            check_trans_prob_mat(m_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_length(n = 0)

            # EXPECTING NO ERROR
            m_P <-
              matrix(
                data = 1/3,
                nrow = n_hs,
                ncol = n_hs
              )

            check_trans_prob_mat(m_P, stop_if_not = F) |>
              capture_error() |>
              expect_length(n = 0)

          })




test_that(desc = "check_trans_prob_mat should issue warnings (or errors) for invalid input depending on stop_if_not argument",
          code =  {
            v_hs_names <- c("H", "S", "D")
            n_hs <- length(v_hs_names)
            m_P <-
              matrix(
                data = 0.5,
                nrow = n_hs,
                ncol = n_hs-1,
                dimnames = list(v_hs_names, v_hs_names[1:2])
              )

            check_trans_prob_mat(m_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_equal(expected = "Transition matrix is not square.")

            check_trans_prob_mat(m_P, stop_if_not = T) |>
              expect_error()

            m_P <-
              matrix(
                data = 0.2,
                nrow = n_hs,
                ncol = n_hs,
                dimnames = list(v_hs_names, v_hs_names)
              )

            check_trans_prob_mat(m_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_equal(expected = "Rows of transition matrix don't sum to 1.")

            check_trans_prob_mat(m_P, stop_if_not = T) |>
              expect_error()

            m_P <-
              matrix(
                data = c(-0.1, 0.5, 0.6,
                         0, 1, 0,
                         0, 0, 1),
                byrow = T,
                nrow = n_hs,
                ncol = n_hs,
                dimnames = list(v_hs_names, v_hs_names)
              )

            check_trans_prob_mat(m_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_equal(expected = "Transition matrix has values below 0 or above 1.")

            check_trans_prob_mat(m_P, stop_if_not = T) |>
              expect_error()


            m_P <-
              matrix(
                data = 1/3,
                nrow = n_hs,
                ncol = n_hs,
                dimnames = list(v_hs_names, v_hs_names[c(1,3,2)])
              )

            check_trans_prob_mat(m_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_equal(expected = "Row and column names do not match.")

            check_trans_prob_mat(m_P, stop_if_not = T) |>
              expect_error()

            m_P <-
              matrix(
                data = c(1, 0, 0,
                         0, 1, 0,
                         0, 0.2, 0.8),
                byrow = T,
                nrow = n_hs,
                ncol = n_hs,
                dimnames = list(v_hs_names, v_hs_names)
              )

            check_trans_prob_mat(m_P, dead_state = "D", stop_if_not = F) |>
              capture_warnings() |>
              expect_equal(expected = "Death state row does not equal 1 in the death state column.")

            check_trans_prob_mat(m_P, dead_state = "D", stop_if_not = T) |>
              expect_error()
          })


# tests on check_trans_prob_array
test_that(desc = "check_trans_prob_array is silent where no error and flags error",
          code = {
            v_hs_names <- c("H", "S", "D")
            n_hs <- length(v_hs_names)
            # create array
            a_P <- array(
              data = 0,
              dim = c(n_hs, n_hs, 1000),
              dimnames = list(v_hs_names, v_hs_names, 1:1000)
            )
            # add in values.
            a_P["H", "S", ] <- 0.3
            a_P["H", "D", ] <- 0.01
            a_P["S", "D", ] <- 0.1
            a_P["S", "H", ] <- 0.5

            for (x in 1:1000) {
              diag(a_P[, , x]) <- 1 - rowSums(a_P[, , x])
            }

            expect_silent(
              check_trans_prob_array(a_P = a_P, stop_if_not = F)
            )

            # introduce error
            a_P["H", "S", 1:10] <- -10

            check_trans_prob_array(a_P = a_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_length(n = 2)

            expect_error(
              check_trans_prob_array(a_P = a_P, stop_if_not = T)
            )


            a_P["H", "S", 1:10] <- 0.5

            check_trans_prob_array(a_P = a_P, stop_if_not = F) |>
              capture_warnings() |>
              expect_length(n = 1)

            a_P <- a_P[1:2, 2:3, 1:10]

            check_trans_prob_array(a_P = a_P, stop_if_not = F) |>
              capture_warnings() |>
              length_greater_than_zero() |>
              expect_true()

            check_trans_prob_array(a_P = a_P, stop_if_not = T) |>
              expect_error()

            # create array
            a_P <- array(
              data = 0,
              dim = c(n_hs, n_hs, 1000),
              dimnames = list(v_hs_names, v_hs_names, 1:1000)
            )
            # add in values.
            a_P["H", "S", ] <- 0.3
            a_P["H", "D", ] <- 0.01
            a_P["S", "D", ] <- 0.1
            a_P["S", "H", ] <- 0.5

            for (x in 1:1000) {
              diag(a_P[, , x]) <- 1 - rowSums(a_P[, , x])
            }

            # introduce error in one cycle of the array, in the death state transition probability row
            a_P["D", "D", c(200, 201)] <- 0.8
            a_P["D", "H", c(200, 201)] <- 0.2

            # expect only 1 warning because all previous checks should pass
            check_trans_prob_array(a_P = a_P, dead_state = "D", stop_if_not = F) |>
              capture_warnings() |>
              expect_length(n = 1)
          })
