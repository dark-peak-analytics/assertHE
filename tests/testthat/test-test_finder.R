test_that(desc = "Check find_function_calls_in_file works for example scripts",
          code = {

            # strings and file locations - currently just 1 but ready for more
            example_project_scripts <- c(
              "calculate_costs"
            )

            for (i in example_project_scripts) {

              relative_path <-
                testthat::test_path(paste0("example_project/tests/testthat/test-", i, ".R"))
              foo_strings <- i

              # create expectation
              expected_df <-
                data.frame(
                  foo_string = rep("calculate_costs", 2),
                  location = paste0(relative_path, ":L", c(26, 33))
                )

              # run function and store output
              object_df <-
                find_function_calls_in_file(relative_path = relative_path,
                                            foo_strings = foo_strings)

              # check equality, should be identical
              expect_equal(object = object_df,
                           expected = expected_df)

            }

          })




test_that(desc = "Check find_function_calls_in_folder works for sicksickerPack example",
          code = {

            expect_silent({
            df_output <- find_function_calls_in_folder(
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
              df_output <- find_function_calls_in_folder(
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
              df_output_2 <- find_function_calls_in_folder(
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

            expect_no_error({
              summarise_model(foo_folder = foo_folder,
                                     test_folder =  test_folder,
                                     output_format = "latex")
              })

            expect_no_error({
              summarise_model(foo_folder = foo_folder,
                              test_folder =  test_folder,
                              output_format = "word")
            })




          })


