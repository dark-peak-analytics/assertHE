#' Function to check for undefined objects in the function body
#'
#' @param func A function object
#' @param env The environment in which to evaluate the function. Default is '.GlobalEnv'
#'
#' @return A character vector of undefined objects contained in the function body
#' @export
#'
#' @importFrom codetools findGlobals
#'
#' @examples
#' \dontrun{
#'
#' # Example function
#' noArgFunction <- function(y) {
#'  x <- undefined_object1 * y
#'  return(x)
#'  }
#'
#' noArgFunction2 <- function(y) {
#'  undefined_object2 <- undefined_object2
#'  x <- undefined_object2 * y
#'  return(x)
#'  }
#'
#' noArgFunction3 <- function(y = T) {
#'  x <- undefined_object2 * T
#'  return(x)
#'  }
#'
#'  noArgFunction4 <- function(examp_list = list("A" = 1, "B" = 2)) {
#'  x <- with(examp_list, A + B)
#'  return(x)
#'  }
#'
#'  # Check for undefined objects
#'  identify_undefined_function_objects("noArgFunction")
#'
#'  # currently fails for the second example!!!
#'  identify_undefined_function_objects(noArgFunction2)
#'
#'  identify_undefined_function_objects(noArgFunction3)
#'  identify_undefined_function_objects(noArgFunction4)
#'  }
identify_undefined_function_objects <- function(func, env = .GlobalEnv) {

  # Extract global variables from the function
  globals <- codetools::findGlobals(fun = func, merge = FALSE)

  # Globals include both "functions" and "variables"; we want "variables"
  used_globals <- globals$variables

  # Get the function arguments
  func_args <- names(formals(fun = func, envir = env))

  # does the function contain a 'with' statement?
  has_with_statement <- grepl(pattern = "with", paste0(collapse = "", body(func)))

  # Identify undefined objects
  undefined_objects <- setdiff(used_globals, func_args)

  # For each of the undefined objects, identify if they are a function
  # or a variable:
  # If a function, remove from the vector
  ret <- sapply(USE.NAMES = F,
                X = undefined_objects,
                FUN = function(x) {
                  if (methods::existsFunction(x)) {
                    x <- paste0(x, " (function?)")
                  }
                  if(has_with_statement) {
                    x <- paste0(x, " (with?)")
                  }
                  if(x == "T") {
                      x <- NA
                  }
                  return(x)
                  })

  if(length(ret) == 0) {
    return(NA)
  }

  ret <- ret[!is.na(ret)]

  return(ret)
}










#' Function to source all files in folder & check for undefined objects
#'
#' It loads the functions to the global environment, which is not ideal.
#' But it works for now.
#'
#' @param foo_folder A character vector of the folder path
#'
#' @return A list of character vectors of undefined objects in each function in a folder
#'
#' @importFrom utils lsf.str
#'
#' @examples
#' \dontrun{
#'
#' foo_folder  <- testthat::test_path("example_project/R")
#' test_folder <- testthat::test_path("example_project/tests/testthat")
#'
#' identify_undefined_function_objects_in_folder(foo_folder)
#' }
identify_undefined_function_objects_in_folder <- function(foo_folder) {
  # Create a temporary environment
  temp_env <- rlang::env()

  # Load all functions into the temporary environment
  load_functions_into_env(foo_folder, env = temp_env)

  # Identify function objects in the temporary environment
  list_of_functions_in_env <- utils::lsf.str(envir = temp_env) |> as.vector()

  # Evaluate each function and identify undefined objects in the temporary environment
  results <- sapply(
    X = list_of_functions_in_env,
    USE.NAMES = TRUE,
    FUN = function(fn_name) {
      # Use rlang::eval_bare() to ensure the function body is evaluated in the temp_env
      fn <- get(fn_name, envir = temp_env)
      identify_undefined_function_objects(func = fn, env = temp_env)
    }
  )

  # Return results
  return(results)
}


