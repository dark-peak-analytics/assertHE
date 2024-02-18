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

  example5 <- "function_name <-function(a){}"
  example6 <- "function_name =function(a){}"
  example7 <- "function_name<-function(a){}"
  example8 <- "function_name=function(a){}"

  exampleHORRID <- " .function.name. =  #comment
    function       # more comment
       (a)   # yet more comment
          {}"

  exampleComplicated <- "# Here are some comments for foo
    #' @family test

      foo <-

    #a comment here - function indented by whitespace too !

      function  ( a )

  {

            a <- some_function(x = a)
            a = another_function(x = 'hello')

          }"

  simple <- "   test_4  <-
  #' adding text here

        function(   x)   {y <- x/2
  return(y)

  }"


    function_commented <- "#' test_my_foo <-  function(){}
      myfoo <- function(){

    }"


  double_function <- "#' double function
  # two functions here, damn.

  myfoo1 <-


  function(){

  hello_function(a)
  }


  myfoo2 <- functon(){}"

  expect_equal(extract_function_name(example1), ".function.name.")
  expect_equal(extract_function_name(example2), "function_name")
  expect_equal(extract_function_name(example3), "__function.name__")
  expect_equal(extract_function_name(example4), "extract_function_name")
  expect_equal(extract_function_name(example5), "function_name")
  expect_equal(extract_function_name(example6), "function_name")
  expect_equal(extract_function_name(example7), "function_name")
  expect_equal(extract_function_name(example8), "function_name")
  expect_equal(extract_function_name(exampleHORRID), ".function.name.")
  expect_equal(extract_function_name(simple), "test_4")
  expect_equal(extract_function_name(exampleComplicated), "foo")
  expect_equal(extract_function_name(function_commented), "myfoo")   # NOT test_my_foo
  expect_equal(extract_function_name(double_function), "myfoo1")

  })


test_that("Extracting function names works as intended ON GITHUB",
          {

source_lines <- function(file, lines){
  # read all lines of the file
  all_lines <- readLines(file)

  if(!is.null(lines)){
  # filter selected lines only
  selected_lines <- all_lines[lines]
  }else{
    selected_lines <- all_lines
  }
  # stitch them all together
  string <- selected_lines |> stringr::str_flatten(collapse = "\n")

  return(string)
}

# intialise empty list
path_lines <- list()

# fill in blanks FOR A GIVEN EXAMPLE
path_lines$create_Markov_trace <- list("url" = "https://raw.githubusercontent.com/dark-peak-analytics/sicksickerPack/v1.0/R/create_Markov_trace.R",
                                       "lines" = NULL,
                                        "expected" = "create_Markov_trace")


path_lines$calculate_QALYs <- list("url" = "https://raw.githubusercontent.com/dark-peak-analytics/sicksickerPack/v1.0/R/calculate_QALYs.R",
                                   "lines" = NULL,
                                   "expected" = "calculate_QALYs")

# for each test case, source from GitHub, run the function and test against expectation
for(i in 1:length(path_lines)){

  string <- source_lines(file = path_lines[[i]][["url"]],
                         lines = path_lines[[i]][["lines"]])

  function_output <- assertHE::extract_function_name(string)
  expected_output <- path_lines[[i]][["expected"]]

  expect_equal(object = function_output,
               expected = expected_output)

}

})



test_that("Next element after integer in vector works as intended",
{
  expect_equal(find_next_vector_element(10, 1:12),
               11)
  expect_equal(find_next_vector_element(value = 4, vector = 1:4),
               as.logical(NA))
  expect_equal(find_next_vector_element(value = 4, vector = 1:4, LTE = T),
               4)
  expect_equal(find_next_vector_element(value = 4, vector = rep(NA, 10)),
               as.integer(NA))
})


test_that("Previous element before integer in vector works as intended", {
  expect_equal(find_previous_vector_element(10, 1:12),
               9)
  expect_equal(find_previous_vector_element(4, 4:12),
               as.logical(NA))
  expect_equal(find_previous_vector_element(4, 1:12, LTE = T),
               4)
  expect_equal(find_next_vector_element(value = 4, vector = rep(NA, 10)),
               as.integer(NA))
})









 test_that("get_file_cheers_classifications works for a few example scripts",
           {
             expect_silent({

               expect_equal(
                get_file_cheers_classifications(filename = testthat::test_path("example_scripts/create_markov_trace.R"),
                                                cheers_pattern = "@family"),
                 stats::setNames(object = "create_Markov_trace", nm =  "simulation")
               )

               expect_equal(
                 get_file_cheers_classifications(filename = testthat::test_path("example_scripts/define_transition_matrix.R"),
                                                 cheers_pattern = "@family"),
                 stats::setNames(object = "define_transition_matrix", nm =  "transitions")
               )

               expect_equal(
                get_file_cheers_classifications(filename = testthat::test_path("example_scripts/example_script.R"),
                                                cheers_pattern = "@family"),
                NA
               )

             })
           })


 test_that("get_file_cheers_classifications works for each script in a project folder",
           {
             v_files <-
               list.files(testthat::test_path("example_project/R"), full.names = T)

             v_target_tags <-
               c(
                 "calculate_costs",
                 "calculate_discounting_weights",
                 "calculate_QALYs",
                 "create_Markov_trace",
                 "define_transition_matrix",
                 "run_sickSicker_model"
               )

             v_outcome_tags <- sapply(X = v_files,
                                      FUN = get_file_cheers_classifications,
                                      cheers_pattern = "@family") |> as.character()

             expect_true(object = length(v_outcome_tags) > 1 && length(setdiff(v_outcome_tags, v_target_tags)) == 0)

           })




test_that("get_folder_cheers_classifications works for a simple set of example scripts",
          {
          tmp <- get_folder_cheers_classifications(path = testthat::test_path("example_scripts"),
                                                   cheers_pattern =  "@family") |> nrow()
          expect_true(object = tmp > 0)
          })



test_that("get_folder_cheers_classifications works for an example project",
          {
            tmp <- get_folder_cheers_classifications(path = testthat::test_path("example_project"),
                                                     cheers_pattern =  "@family") |> nrow()
            expect_true(object = tmp > 0)
          })







test_that("find_function_definitions works as intended",
    {
      expected = c(
        "do_something_random",
        "calculate_something",
        "find_matches",
        "perform_task",
        "combine_strings",
        "process_data",
        "transform_data",
        "sort_values",
        "generate_output",
        "do_everything",
        "lots_of_comments_foo"
      )


      object = find_function_definitions(
                    filename = testthat::test_path("example_scripts", "example_tricky_functions.R"))
      object <- object$text
      expect_equal(object, expected)
    })




# test_that("find_function_definitions works as intended FROM GITHUB",
#           {
#
#             source_lines <- function(file, lines){
#               # read all lines of the file
#               all_lines <- readLines(file)
#               # filter selected lines only
#               selected_lines <- all_lines[lines]
#               # stitch them all together
#               string <- selected_lines |> stringr::str_flatten(collapse = "\n")
#
#               return(string)
#             }
#
#             # intialise empty list
#             l_all_github_tests <- list()
#
#             # fill in blanks FOR A GIVEN EXAMPLE
#             v_sicksickerPack_function_names <- c("create_Markov_trace", "calculate_QALYs", "calculate_discounting_weights", "calculate_costs", "run_sickSicker_model")
#
#             path_lines_sicksickerPack <-
#               lapply(
#                 X = v_sicksickerPack_function_names,
#                 FUN =  function(x) {
#                   list(
#                     url = paste0(
#                       "https://raw.githubusercontent.com/dark-peak-analytics/sicksickerPack/v1.0/R/",
#                       x,
#                       ".R"
#                     ),
#                     expected = x
#                   )
#                 }
#               )
#
#             l_all_github_tests <- c(l_all_github_tests, path_lines_sicksickerPack)
#
#             # for each test case, source from GitHub, run the function and test against expectation
#             for(i in 1:length(l_all_github_tests)){
#
#               Sys.sleep(2)
#
#               function_output <- assertHE::find_function_definitions(filename = l_all_github_tests[[i]][["url"]])
#               function_output <- function_output$text
#               expected_output <- l_all_github_tests[[i]][["expected"]]
#
#               expect_equal(object = function_output,
#                            expected = expected_output)
#
#             }
#
#
#           })

