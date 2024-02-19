#' Find all function calls in file
#'
#' Searches through a file for function calls using SYMBOL_FUNCTION_CALL
#'
#' @param file_path path of file to search in
#' @param foo_strings string vector of function names to search for
#' @param filter_for_test_that whether to filter for only functions used after the call to test_that. Default FALSE.
#'
#' @return a dataframe with the columns 'foo' for function name and 'location' which gives
#' the file in which the function is called with the line in which the function is called
#' appended.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' function_calls_in_file(
#' file_path = "tests/testthat/example_project/tests/testthat/test-calculate_costs.R",
#' foo_strings = "calculate_costs"
#' )
#' }
function_calls_in_file <- function(file_path,
                                   foo_strings,
                                   filter_for_test_that = FALSE){

  # quick checks
  assertthat::assert_that(msg = paste("Can't find file at:", file_path),
                          file.exists(file_path))

  assertthat::assert_that(msg = "No function names provided to 'foo_strings'",
                          is.character(foo_strings) & length(foo_strings) > 0)

  assertthat::assert_that(msg = "filter_for_test_that must be logical",
                          is.logical(filter_for_test_that))

  # parse file and check
  parsed_file <- parse(file_path,
                       keep.source = TRUE)

  df <- utils::getParseData(parsed_file,
                            includeText = TRUE)

  # get line and text from rows with function calls.
  if(length(foo_strings) > 1) {
    operand <- "df$text %in% foo_strings"
  } else{
    operand <- "df$text == foo_strings"
  }

  # identify line for test_that and then filter for after test_that only.
  if (filter_for_test_that) {
    first_test_that_call <-
      df[df$token == "SYMBOL_FUNCTION_CALL" &
           df$text == "test_that", "line1"]
    if(length(first_test_that_call)  > 0){
      df <- df[df$line1 >= first_test_that_call,]
    }else{
      return(NULL)
    }
  }

  # subset only function calls in the list of provided functions after the first test_that call.
  df <- df[df$token == "SYMBOL_FUNCTION_CALL" & eval(parse(text = operand)), c("line1", "text")]

  # remove rownames, unnecessary
  rownames(df) <- NULL

  # return null if none available
  if(nrow(df) == 0) return(NULL)

  # combine file path & line number in single string
  df$location <- paste0(file_path, ":L", df$line)
  df$foo      <- df$text

  return(df[, c("foo", "location")])

}




#' Find specific function calls in a folder
#'
#' Runs function_calls_in_file on all files in a folder, and combined results into a
#' single dataframe
#'
#'
#' @param test_folder folder containing all tests
#' @param foo_strings function names to search for
#'
#' @return dataframe with two columns. 'foo' contains function names, location
#' contains the location of the tests for each function (file and line number).
#' @export
#'
#' @examples
#' \dontrun{
#' function_calls_in_folder(foo_strings = c("calculate_costs",
#' "calculate_QALYs",
#' "create_Markov_trace",
#' "FOO_WITH_NO_TESTS"),
#' test_folder = "./tests/testthat/example_project/tests/testthat")
#' }
function_calls_in_folder <- function(test_folder,
                                     foo_strings) {

  # quick checks
  assertthat::assert_that(msg = paste("Can't find file at:", file_path),
                          file.exists(test_folder))

  assertthat::assert_that(msg = "No function names provided to 'foo_strings'",
                          is.character(foo_strings) & length(foo_strings) > 0)


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

  if(length(l_foo_test_paths) == 0) return(data.frame(foo = foo_strings, location = NA))

  # get summary dataframe
  df_summary <- dplyr:::bind_rows(l_foo_test_paths) |>
                  as.data.frame()

  # ensure all function inputs are included in dataframe of outputs
  df_out <- merge(
    x = df_summary,
    y = data.frame(foo = foo_strings),
    by = "foo",
    all = T
  )

  return(df_out)

}
