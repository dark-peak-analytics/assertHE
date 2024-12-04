# rm(list = ls())
#
# file_path <- "tests/testthat/example_scripts/no_argument_function.R"
# # file_path <- "tests/testthat/example_scripts/create_markov_trace.R"
#
# # source functions to the global environment
# source(file_path)
#
# # Load codetools
# library(codetools)


#' Function to check for undefined objects in the function body
#'
#' @param func A function object
#'
#' @return A character vector of undefined objects contained in the function body
#' @export
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
#'  check_undefined_objects(noArgFunction)
#'
#'  # currently fails for the second example!!!
#'  check_undefined_objects(noArgFunction2)
#'
identify_undefined_function_objects <- function(func) {

  # Extract global variables from the function
  globals <- codetools::findGlobals(fun = func, merge = FALSE)

  # Globals include both "functions" and "variables"; we want "variables"
  used_globals <- globals$variables

  # Get the function arguments
  func_args <- names(formals(func))

  # Identify undefined objects
  undefined_objects <- setdiff(used_globals, func_args)

  return(undefined_objects)
}



# # Example function
# noArgFunction <- function(y) {
#   x <- another_object * y
#   return(x)
# }
#
#
# # Check for undefined objects
# identify_undefined_function_objects(noArgFunction)





