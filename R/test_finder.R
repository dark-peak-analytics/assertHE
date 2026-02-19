#' Find all function calls in file
#'
#' Searches through a file for function calls using SYMBOL_FUNCTION_CALL
#'
#' @param relative_path path of file to search in
#' @param foo_strings string vector of function names to search for
#' @param filter_for_test_that whether to filter for only functions used after the call to test_that. Default FALSE.
#'
#' @return a dataframe with the columns 'foo' for function name and 'test_location' which gives
#' the file in which the function is called with the line in which the function is called
#' appended.
#'
#' @export
#'
#' @examples
#' file_path <- assertHE_example("example_project/tests/testthat/test-calculate_costs.R")
#' find_function_calls_in_file(
#'   relative_path = file_path,
#'   foo_strings = "calculate_costs"
#' )
#'
find_function_calls_in_file <- function(relative_path = NULL,
                                        foo_strings,
                                        filter_for_test_that = FALSE){

  # quick checks
  assertthat::assert_that(msg = paste("Can't find file at:", relative_path),
                          file.exists(relative_path))

  assertthat::assert_that(msg = "No function names provided to 'foo_strings'",
                          is.character(foo_strings) & length(foo_strings) > 0)

  assertthat::assert_that(msg = "filter_for_test_that must be logical",
                          is.logical(filter_for_test_that))

  # parse file and check
  parsed_file <- parse(relative_path,
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
  df$test_location <- paste0(relative_path, "#L", df$line)
  df$foo_string  <- df$text

  return(df[, c("foo_string", "test_location")])

}

#' Find specific function calls in a folder
#'
#' Runs find_function_calls_in_file on all files in a folder, and combined results into a
#' single dataframe
#'
#'
#' @param test_folder folder containing all tests
#' @inheritParams find_function_calls_in_file
#'
#' @return dataframe with two columns. 'foo' contains function names, test_location
#' contains the location of the tests for each function (file and line number).
#' @export
#'
#' @examples
#' folder_path <- assertHE_example("example_project/tests/testthat")
#' find_function_calls_in_folder(
#'   foo_strings = c("calculate_costs", "calculate_QALYs",
#'     "create_Markov_trace", "FOO_WITH_NO_TESTS"),
#'   test_folder = folder_path
#' )
#'
find_function_calls_in_folder <- function(test_folder,
                                          foo_strings,
                                          filter_for_test_that = FALSE) {

  # quick checks
  assertthat::assert_that(msg = paste("Can't find folder at:", test_folder),
                          file.exists(test_folder))

  assertthat::assert_that(msg = "No function names provided to 'foo_strings'",
                          is.character(foo_strings) & length(foo_strings) > 0)


  # locations of test files for the example project
  v_test_file_paths <-
    list.files(path = test_folder,
               recursive = TRUE,
               full.names = TRUE,
               pattern = "\\.R$")

  # find function names in all files in test folder
  l_foo_test_paths <- lapply(X = v_test_file_paths,
                             FUN = find_function_calls_in_file,
                             foo_strings = foo_strings,
                             filter_for_test_that = filter_for_test_that)

  # remove nulls
  l_foo_test_paths <- l_foo_test_paths |>
    Filter(f = Negate(is.null))

  if(length(l_foo_test_paths) == 0) return(data.frame(foo_string = foo_strings,
                                                      test_location = NA))

  # get summary dataframe
  df_summary <- dplyr::bind_rows(l_foo_test_paths) |>
                  as.data.frame()

  # ensure all function inputs are included in dataframe of outputs
  df_out <- merge(
    x = df_summary,
    y = data.frame(foo_string = foo_strings),
    by = "foo_string",
    all = TRUE
  )

  return(df_out)

}

#' Summarise the model functions in a single folder.
#'
#' @param foo_folder path to folder containing all functions for the model
#' @param exclude_files A regular expression for files to NOT process (basename)
#' @param exclude_dirs A regular expression for directories to NOT process (dirname)
#' @param output_format output format to use, defaults to dataframe, options include latex and word.
#' @param project_path path to the project folder, if not provided, will use current working directory.
#' @inheritParams find_function_calls_in_folder
#'
#' @return dataframe with three columns. 'foo_string' contains function names, 'foo_location'
#' contains the location of the function definitions, 'test_location' contains the locations
#' of tests for each function (both file and line number).
#' @export
#'
#' @importFrom knitr kable
#' @importFrom flextable flextable body_add_flextable width
#' @importFrom officer read_docx body_add_par
#'
#' @examples
#' project_path <- assertHE_example("example_project")
#' foo_folder  <- "R"
#' test_folder <- "tests/testthat"
#'
#' summarise_model(
#'   project_path = project_path,
#'   foo_folder = foo_folder,
#'   test_folder =  test_folder
#' )
#'
#' summarise_model(
#'   project_path = project_path,
#'   foo_folder = foo_folder,
#'   test_folder =  NULL
#' )
#'
summarise_model <- function(project_path = ".",
                            foo_folder = "R",
                            exclude_files = NULL,
                            exclude_dirs = NULL,
                            test_folder = NULL,
                            output_format = "dataframe") {

  # Check folder existence
  stopifnot(dir.exists(project_path),
            dir.exists(paste0(project_path,"/", foo_folder)),
            dir.exists(paste0(project_path,"/", test_folder)))

  # if test path is null then don't include them in summary...
  test_folder <- if (is.null(test_folder)) {
    NULL
  } else{
    paste0(project_path, "/", test_folder)
  }

  foo_folder <- paste0(project_path,"/", foo_folder)

  # function summary
  df <- find_folder_function_definitions(foo_folder = foo_folder,
                                         f_excl = exclude_files,
                                         d_excl = exclude_dirs)

  # if there is no test folder then there are no test locations...
  if (is.null(test_folder)) {
    df$test_location <- NA
  } else {

  # test summary
  df_test_summary <-
    find_function_calls_in_folder(foo_strings = df$foo_string,
                                  test_folder = test_folder)

  # merge the two files
  df <- merge(x = df,
              y = df_test_summary,
              by = "foo_string",
              all.x = TRUE)

  }

  # remove the project path from the start of the locations:
  df$foo_location <- sub(
    x = df$foo_location,
    pattern = paste0(project_path, "/"),
    replacement = "",
    fixed = TRUE
  )
  df$test_location <- sub(
    x = df$test_location,
    pattern = paste0(project_path, "/"),
    replacement = "",
    fixed = TRUE
  )

  # Return the correct format.
  if (output_format == "latex") {
    return(knitr::kable(x = df, format = "latex"))
  }

  if (output_format == "word") {
    # create a word document with title and summary.
    doc <- officer::read_docx() |>
      officer::body_add_par(value =  "Model function summary",
                            style = "heading 1") |>
      officer::body_add_par(value = "",
                            style = "centered",
                            pos = "after") |>
      officer::body_add_par(value = "Created using the assertHE R package.",
                            style = "centered",
                            pos = "after") |>
      officer::body_add_par(value = "",
                            style = "centered",
                            pos = "after")

    # add the table in
    doc <- doc |>
      flextable::body_add_flextable(
        flextable::flextable(df) |>
          flextable::width(width = 2)
      )

    print(x = doc, target = "model_summary.docx")

    return(paste0("Word document created at: ", project_path, "/", "model_summary.docx"))
  }

  return(df)
}
