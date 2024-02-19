test_that(desc = "Check functions & associated tests identified from sicksickerPack",
          code = {

            expect_silent({
            df_output <- function_calls_in_folder(
              foo_strings = c(
                "calculate_costs",
                "calculate_QALYs",
                "create_Markov_trace",
                "FOO_WITH_NO_TESTS"
              ),
              test_folder = testthat::test_path("example_project/tests/testthat")
            )
            })

            # generates some output
            expect_true(nrow(df_output) > 0)

            expect_error({
              df_output <- function_calls_in_folder(
                foo_strings = c(
                  "calculate_costs",
                  "calculate_QALYs",
                  "create_Markov_trace",
                  "FOO_WITH_NO_TESTS"
                ),
                test_folder = testthat::test_path("no path")
              )
            })

            expect_silent({
              df_output_2 <- function_calls_in_folder(
                foo_strings = "THIS_IS_NOT_A_FUNCTION",
                test_folder = testthat::test_path("example_project/tests/testthat")
              )
            })

            expect_equal(object = nrow(df_output_2), expected = 1)

          })


