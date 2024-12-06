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
#'  # Check for undefined objects
#'  identify_undefined_function_objects("noArgFunction")
#'
#'  # currently fails for the second example!!!
#'  identify_undefined_function_objects(noArgFunction2)
#'
identify_undefined_function_objects <- function(func, env = .GlobalEnv) {

  # Extract global variables from the function
  globals <- codetools::findGlobals(fun = func, merge = FALSE)

  # Globals include both "functions" and "variables"; we want "variables"
  used_globals <- globals$variables

  # Get the function arguments
  func_args <- names(formals(fun = func, envir = env))

  # Identify undefined objects
  undefined_objects <- setdiff(used_globals, func_args)

  # sensecheck

  # For each of the undefined objects, identify if they are a function
  # or a variable:
  # If a function, remove from the vector
  ret <- sapply(USE.NAMES = F,
                X = undefined_objects,
                FUN = function(x) {
                  if (methods::existsFunction(x)) {
                    paste0(x, " (function?)")
                  } else {
                    x
                  }})

  if(length(ret) == 0) {
    return(NA)
  }

  return(ret)
}










#' #' Function to source all files in folder & check for undefined objects
#' #'
#' #' @param foo_folder A character vector of the folder path
#' #'
#' foo_folder  <- testthat::test_path("example_project/R")
#' test_folder <- testthat::test_path("example_project/tests/testthat")
#'
#' test_foo <- function(foo_folder){
#'
#'   # Create a new environment to avoid sourcing scripts into the namespace
#'   pkg_env <- new.env(parent = baseenv())
#'
#'   # Load all functions into this environment
#'   assertHE:::load_functions_into_env(foo_folder, pkg_env)
#'
#'   #print(ls(envir = environment()))
#'   # identify function objects in environment
#'   list_of_functions_in_env <- lsf.str(envir = pkg_env) |> as.vector()
#'
#'   sapply(X = list_of_functions_in_env,
#'          USE.NAMES = TRUE,
#'          env = pkg_env,
#'          FUN = identify_undefined_function_objects)
#'
#' }
#'
#' test_foo(foo_folder)
