test_that("Extracting function names works as intended",
          {

  example1 <- " .function.name. <-

  function
  (a, b, c){


  }"

  example2 <- "function_name <- function(a, b, c){}"

  example3 <-"__function.name__ =



                      function(a, b, c){}"

  example4 <- "extract_function_name = function(string) {
  # does the string 'function' exist in the string
"
  # run through all four examples.
  expect_equal(extract_function_name(example1), ".function.name.")
  expect_equal(extract_function_name(example2), "function_name")
  expect_equal(extract_function_name(example3), "__function.name__")
  expect_equal(extract_function_name(example4), "extract_function_name")

  # run through all four examples.
  expect_equal(extract_function_name2(example1), ".function.name.")
  expect_equal(extract_function_name2(example2), "function_name")
  expect_equal(extract_function_name2(example3), "__function.name__")
  expect_equal(extract_function_name2(example4), "extract_function_name")


})





test_that("Next element after integer in vector works as intended",
          {
            expect_equal(find_next_vector_element(10, 1:12), 11)
            expect_equal(find_next_vector_element(4, seq(1, 120, 5)), 6)
})






test_that("Previous element before integer in vector works as intended",
          {
            expect_equal(find_previous_vector_element(10, 1:12), 9)
            expect_equal(find_previous_vector_element(4, seq(1, 120, 5)), 1)
          })









# test_that("get_file_cheers_classifications works for a few example scripts",
#           {
#           if (testthat::testing_package() != ""){
#               path <- dirname(dirname(getwd()))
#             }else{
#               path <- getwd()
#             }
#
#             expect_silent({
#
#               expect_equal(
#                get_file_cheers_classifications(filename = paste0(path, "/inst/example_script/create_markov_trace.R"),
#                                                cheers_pattern = "@family"),
#                 stats::setNames(object = "create_Markov_trace", nm =  "simulation")
#               )
#
#               expect_equal(
#                 get_file_cheers_classifications(filename = paste0(path, "/inst/example_script/define_transition_matrix.R"),
#                                                 cheers_pattern = "@family"),
#                 stats::setNames(object = "define_transition_matrix", nm =  "transitions")
#               )
#
#               #expect_equal(
#               #  get_file_cheers_classifications(filename = paste0(path,"/inst/example_script/example_script.R"),
#               #                                  cheers_pattern = "@family"),
#               #  NA
#               #)
#
#             })
#           })



test_that("get_folder_cheers_classifications works for a few example folders",
          {

            if (testthat::testing_package() != ""){
              path <- dirname(dirname(getwd()))
            }else{
              path <- getwd()
            }

            expect_silent({

              get_folder_cheers_classifications(path = "./inst",
                                                cheers_pattern =  "@family")

            })
          })





