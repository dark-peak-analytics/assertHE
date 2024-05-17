
#' Get coverage by function
#'
#' @param foo_folder folder containing functions
#' @param test_folder folder containing tests
#'
#' @return a dataframe with a column for functions and a column for coverage
#'
#' @export
#'
#' @importFrom covr file_coverage
#' @importFrom dplyr group_by summarise rename
#'
#' @examples
#' \dontrun{
#' get_foo_coverage(
#' foo_folder = "tests/testthat/example_project/R",
#' test_folder = "tests/testthat/example_project/tests/testthat"
#' )
#'
#' get_foo_coverage(
#' foo_folder = "R",
#' test_folder = "tests/testthat"
#' )
#' }
#'
get_foo_coverage <- function(foo_folder,
                             test_folder) {
  source_files <-
    list.files(foo_folder,
               pattern = ".R",
               full.names = T)
  test_files   <-
    list.files(test_folder,
               pattern = ".R",
               full.names = T)

  # Use file_coverage() to calculate test coverage
  tmp <- NULL

  tryCatch(
    expr = {
      invisible({
        tmp <- suppressMessages({
          covr::file_coverage(source_files = source_files,
                                                  test_files = test_files)
          })
      })
    },
    error = function(cond) {
      if (grepl(pattern = "fail",
                ignore.case = T,
                conditionMessage(cond))) {
        message(
          "Code tests fail - please correct before proceeding \nRunning function without code coverage."
        )
      } else{
        message(conditionMessage(cond))
      }
    }
  )

  # convert to a dataframe
  tmp <- as.data.frame(x = tmp)

  # group by function and then get proportion not 0
  functions <- value <- NULL
  tmp <- tmp |>
    dplyr::group_by(functions) |>
    dplyr::summarise(coverage = mean(value > 0))|>
    dplyr::rename(foo_string = functions) |>
    as.data.frame()

  return(tmp)

}


