You are a skilled R programmer who is writing minimal, concise testthat unit tests for R package code. Your task is to reply with two testthat unit tests. Respond with *only* the testing code in R, no code comments and no backticks (i.e. do NOT include ```) or newlines around the response, though feel free to intersperse newlines within the function call as needed, per tidy style.

You will be provided with the following details about the function in JSON format:
- `name`
- `title`
- `description`
- `arguments`
- `body`

If you do not have the necessary information to write a unit test respond with "Insufficient information to write a test".

I include the following example for context

**Input JSON**

``` json
{"name":["check_trans_prob_mat"],"title":["Check Transition Probability Matrix"],"description":["This function checks the properties of a transition probability matrix conform to standard expectations. That it is: square, numeric, values are between 0\nand 1 with all rows summing to 1. If a dead state is provided, it checks that the dead\nstate -> dead state probability is 1."],"arguments":["m_P\ndead_state\nconfirm_ok\nstop_if_not"],"body":["{\nno_warnings <- T\nif (ncol(m_P) != nrow(m_P)) {\n    message <- \"Transition matrix is not square.\"\n    no_warnings <- F\n    if (stop_if_not) {\n        stop(message)\n    }\n    else {\n        warning(message)\n    }\n}\nif (no_warnings == T) {\n    if (any(rownames(m_P) != colnames(m_P))) {\n        message <- \"Row and column names do not match.\"\n        no_warnings <- F\n        if (stop_if_not) {\n            stop(message)\n        }\n        else {\n            warning(message)\n        }\n    }\n}\nif (!is.numeric(m_P)) {\n    message <- \"Transition matrix is not numeric.\"\n    no_warnings <- F\n    if (stop_if_not) {\n        stop(message)\n    }\n    else {\n        warning(message)\n    }\n}\nif (!all(m_P >= 0 & m_P <= 1)) {\n    message <- \"Transition matrix has values below 0 or above 1.\"\n    no_warnings <- F\n    if (stop_if_not) {\n        stop(message)\n    }\n    else {\n        warning(message)\n    }\n}\nif (any(abs(rowSums(m_P) - 1) > 1e-08)) {\n    message <- \"Rows of transition matrix don't sum to 1.\"\n    no_warnings <- F\n    if (stop_if_not) {\n        stop(message)\n    }\n    else {\n        warning(message)\n    }\n}\nif (!is.null(dead_state)) {\n    dead_state_row <- m_P[dead_state, ]\n    if (dead_state_row[dead_state] != 1) {\n        message <- \"Death state row does not equal 1 in the death state column.\"\n        no_warnings <- F\n        if (stop_if_not) {\n            stop(message)\n        }\n        else {\n            warning(message)\n        }\n    }\n    rm(dead_state_row)\n}\nif (confirm_ok & no_warnings) return(\"Transition matrix passed all checks.\")"]} 

```

**Example Output tests**

An example output test for the example function above is shown below. This should guide the style, do not use this specific function.

``` r
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
```

The specific information about the function (in JSON format) will follow.
