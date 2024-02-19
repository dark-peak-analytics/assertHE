test_that(desc = "Check function_calls_in_file works for example scripts",
          code = {

            # strings and file loctions
            relative_path <- testthat::test_path("example_project/tests/testthat/test-calculate_costs.R")
            foo_strings <- "calculate_costs"

            # create expectation
            expected_df <- data.frame(foo_string = rep("calculate_costs", 2),
                                      location = paste0(relative_path, ":L", c(26, 33)))

            # run function and store output
            object_df <- function_calls_in_file(relative_path = relative_path,
                                             foo_strings = foo_strings)

            # check equality, should be identical
            expect_equal(object = object_df,
                         expected = expected_df)

          })




test_that(desc = "Check function_calls_in_folder works for sicksickerPack example",
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

            expect_equal(object = df_output_2$foo_string, expected = "THIS_IS_NOT_A_FUNCTION")
            expect_equal(object = df_output_2$location, expected = NA)

          })



test_that(desc = "Check summarise_model works for sicksickerPack example",
          code = {
            foo_folder  <- testthat::test_path("example_project/R")
            test_folder <- testthat::test_path("example_project/tests/testthat")

            expect_silent({
              tmp <- summarise_model(foo_folder = foo_folder,
                              test_folder =  test_folder)
            })

            expect_true(nrow(tmp) > 0)

          })


