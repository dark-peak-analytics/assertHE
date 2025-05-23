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
#' \donttest{
#' # Example takes more than 5 seconds to run
#' if(require(testthat)) {
#'   folder_path1 <- assertHE_example("example_project/R")
#'   folder_path2 <- assertHE_example("example_project/tests/testthat")
#'   get_foo_coverage(
#'     foo_folder = folder_path1,
#'     test_folder = folder_path2
#'   )
#' }
#' }
#'
get_foo_coverage <- function(foo_folder,
                             test_folder) {
  source_files <-
    list.files(foo_folder,
               pattern = ".R",
               full.names = TRUE)
  test_files   <-
    list.files(test_folder,
               pattern = ".R",
               full.names = TRUE)

  # Use file_coverage() to calculate test coverage
  tmp <- NULL

  tryCatch(
    expr = {
      invisible({
        tmp <- covr::file_coverage(source_files = source_files,
                                   test_files = test_files)
      })
    },
    error = function(cond) {
      if (grepl(pattern = "fail",
                ignore.case = TRUE,
                conditionMessage(cond))) {
        warning(
          "Code tests fail - please correct before proceeding \nRunning function without code coverage."
        )
      } else{
        warning(conditionMessage(cond))
      }
    }
  )

  if(!is.null(tmp)) {
    # convert to a dataframe
    tmp <- as.data.frame(x = tmp)

    # group by function and then get proportion not 0
    functions <- value <- NULL
    tmp <- tmp |>
      dplyr::group_by(functions) |>
      dplyr::summarise(coverage = mean(value > 0))|>
      dplyr::rename(foo_string = functions) |>
      as.data.frame()
  }

  return(tmp)
}
