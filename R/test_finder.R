rm(list = ls())

library(testthat)

# function to get a list of tested functions... i.e. functions that are tested
# in the tests folder ...

# locations of R scripts for the example project
v_source_file_paths_example_project <-
  list.files(path = "./tests/testthat/example_project/R",
             full.names = TRUE)


# locations of test files for the example project
v_test_file_paths_example_project <-
  list.files(path = "./tests/testthat/example_project/tests/testthat",
             recursive = TRUE,
             full.names = TRUE)

# file coverage checker
l_coverage <-
  covr::file_coverage(source_files = v_source_file_paths_example_project,
                      test_files = v_test_file_paths_example_project)

# get a vector of all of the tested functions...
v_tested_foo <- sapply(
  X = l_coverage,
  FUN = function(x)
    x$functions
) |>
  unlist() |>
  unique()

# see the vector
v_tested_foo

# However, this does not tell us where the functions are tested, only that they are.


#====================================================#
# ALTERNATIVE, SAME METHOD AS FOR FUNCTION SEARCHING #
#====================================================#

#' Find all function calls in file
#'
#' Searches through a file for function calls using SYMBOL_FUNCTION_CALL
#'
#' @param file_path path of file to search in
#' @param foo_strings function names to search for
#'
#' @return a dataframe with the columns 'foo' for function name and 'location' which gives
#' the file in which the function is called with the line in which the function is called
#' appended.
#'
#' @export
#'
#' @examples
function_calls_in_file <- function(file_path,
                                   foo_strings){

  parsed_file <- parse(file_path,
                       keep.source = TRUE)

  df <- utils::getParseData(parsed_file,
                            includeText = TRUE)

  #return(df[df$token == "SYMBOL_FUNCTION_CALL",])

  # get line and text from rows with function calls.
  if(length(foo_strings) > 1) {
    operand <- "df$text %in% foo_strings"
  } else{
    operand <- "df$text == foo_strings"
  }

  df <- df[df$token == "SYMBOL_FUNCTION_CALL" & eval(parse(text = operand)), c("line1", "text")]

  if(nrow(df) == 0) return(NULL)

  df$test_file <- file_path

  # combine file path & line number in single string
  df$location <- paste0(df$test_file, ":L", df$line)
  df$foo      <- df$text
  rownames(df) <- NULL

  return(df[, c("foo", "location")])

}

function_calls_in_file(file_path = "tests/testthat/example_project/tests/testthat/test-calculate_costs.R",
                       foo_strings = "calculate_costs")


#' Find specific function calls in a folder
#'
#' Runs function_calls_in_file on all files in a folder, and combined results into a
#' single dataframe
#'
#'
#' @param test_folder folder containing all tests
#' @param foo_strings function names to search for
#'
#' @return
#' @export
#'
#' @examples
function_calls_in_folder <- function(test_folder,
                                     foo_strings) {
  # locations of test files for the example project
  v_test_file_paths <-
    list.files(path = test_folder,
               recursive = TRUE,
               full.names = TRUE)

  # find function names in all files in test folder
  l_foo_test_paths <- lapply(X = v_test_file_paths,
                             FUN = function_calls_in_file,
                             foo_strings = foo_strings)

  # remove nulls
  l_foo_test_paths <- l_foo_test_paths |>
    Filter(f = Negate(is.null))

  return(dplyr:::bind_rows(l_foo_test_paths))

}

function_calls_in_folder(foo_strings = c("calculate_costs", "calculate_QALYs", "create_Markov_trace"),
                         test_folder = "./tests/testthat/example_project/tests/testthat")


