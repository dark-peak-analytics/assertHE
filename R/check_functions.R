#' #' @title List all of the functions in a script
#' #'
#' #' @description This function lists all of the functions in a script. It expects the script to be
#' #' an R file, and requires that package libraries are loaded when identifying packages.
#' #'
#' #' @param filename The name of the file to be checked.
#' #' @param alphabetic If TRUE, return the functions in alphabetic order.
#' #' @param by_package If TRUE, return a list of functions by package. Else return a vector of functions.
#' #'
#' #' @return A named list of functions in the script, by package.
#' #'
#' #' @family checking_functions
#' #'
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' list_functions_in_script(filename = "./R/check_trans_probs.R")
#' #'
#' #' list_functions_in_script(filename = "./R/check_functions.R", by_package = FALSE)
#' #' list_functions_in_script(filename =
#' #' paste0("https://raw.githubusercontent.com/dark-peak-analytics/",
#' #' "sicksickerPack/main/R/create_Markov_trace.R"), by_package = FALSE)
#' #'
#' #' }
#' list_functions_in_script <-
#'   function(filename,
#'            by_package = TRUE,
#'            alphabetic=TRUE) {
#'
#'   # from hrbrmstr, StackExchange 3/1/2015
#'   #if(!file.exists(filename)) { stop("couldn't find file ",filename) }
#'   if(!tools::file_ext(filename) == "R") { warning("expecting *.R file, will try to proceed") }
#'   # read in parsed data from script
#'   tmp <- utils::getParseData(parse(filename, keep.source=TRUE))
#'   # only keep those lines that are identified as function calls
#'   nms <- tmp$text[which(tmp$token=="SYMBOL_FUNCTION_CALL")]
#'   # only keep unique functions for script
#'   funs <- unique(if(alphabetic) { sort(nms) } else { nms })
#'
#'   # if don't want by package just return vector of function names
#'   if(!by_package) {
#'     return(funs)
#'   }
#'
#'   # return a list of functions and the file they were found in
#'   # take the first only
#'   v_packages <- as.vector(sapply(funs, utils::find))
#'   src <- paste(sapply(X = v_packages, FUN = function(x) x[1]))
#'   outlist <- tapply(funs, factor(src), c)
#'
#'   return(outlist)
#'
#' }
#'
#'
#' #' Create table of all of the functions in a script
#' #'
#' #' This function tabulates all of the functions in a script with the name of the
#' #' package next to it. It expects the script to be an R file, and requires that
#' #' package libraries are loaded when identifying packages.
#' #'
#' #' @param filename The name of the file to be checked.
#' #' @param packages_to_exclude A vector of packages to exclude from the output (e.g. "base")
#' #'
#' #' @return A data-frame of functions and the package the function is from.
#' #'
#' #' @family checking_functions
#' #'
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' tabulate_functions_in_script(filename = "./R/check_functions.R")
#' #' tabulate_functions_in_script(filename = "./R/check_trans_probs.R",
#' #'                              packages_to_exclude = NULL)
#' #' tabulate_functions_in_script(filename =
#' #' paste0("https://raw.githubusercontent.com/dark-peak-analytics/",
#' #' "sicksickerPack/main/R/create_Markov_trace.R"), packages_to_exclude = NULL)
#' #' }
#' tabulate_functions_in_script <- function(filename,
#'                                          packages_to_exclude = c("base", "stats", "utils")) {
#'   # list the functions in the file.
#'   my_packages <- list_functions_in_script(filename)
#'
#'   # convert nested list to a dataframe where each row gives the function name
#'   # and the package it belongs to:
#'   df <- utils::stack(my_packages)
#'   colnames(df) <- c("foo", "package")
#'
#'   # remove 'package:' from the strings
#'   df$package <- gsub("package:", "", df$package)
#'
#'   # remove 'character(0)' and replace with 'local'
#'   df$package[df$package == "character(0)"] <- "unknown"
#'
#'   # exclude unwanted packages (to reduce size)
#'   exclude_index <- df$package %in% packages_to_exclude
#'   df <- df[!exclude_index, ]
#'
#'   return(df)
#' }
#'
#'
#' #' Create table of all of the functions identified in a project folder
#' #'
#' #' This function tabulates all of the functions identified in R scripts within a
#' #' project folder. It requires that package libraries are loaded when identifying
#' #' a function's package.
#' #'
#' #' @param path The path to the folder to be checked.
#' #' @param collapse If TRUE, return a single data-frame of all functions. Else return a list by file.
#' #' @param packages_to_exclude A vector of packages to exclude from the output (e.g. "base")
#' #' @param path_exclude A string which if found in file path removes file from analysis.
#' #' Defaults to 'testthat/' to exclude functions only found in tests.
#' #'
#' #' @family checking_functions
#' #'
#' #' @return Either a data-frame of functions and the package the function is from, or a list of functions by file.
#' #' @export
#' #' @examples
#' #'
#' #' \dontrun{
#' #' tabulate_functions_in_folder(
#' #'     path = ".",
#' #'     path_exclude = "testthat/",
#' #'     collapse = T,
#' #'     packages_to_exclude = c("base", "stats", "utils")
#' #'     )
#' #' }
#' tabulate_functions_in_folder <- function(path = ".",
#'                                          path_exclude = "testthat/",
#'                                          collapse = T,
#'                                          packages_to_exclude = c("base", "stats", "utils")) {
#'   # get all files from the path folder, i.e. everything in repo.
#'   my_R_scripts <- list.files(
#'     path = path,
#'     pattern = "\\.R$",
#'     recursive = TRUE,
#'     full.names = TRUE
#'   )
#'
#'   # exclude those in the testthat (or other specified) folder if not null
#'   if(!is.null(path_exclude)){
#'     my_R_scripts <- my_R_scripts[!grepl(pattern = path_exclude, x = my_R_scripts)]
#'   }
#'
#'   l_foo <- lapply(X = my_R_scripts,
#'                   FUN = tabulate_functions_in_script,
#'                   packages_to_exclude = packages_to_exclude)
#'
#'   # collapse the list into a single dataframe
#'   if (collapse) {
#'     df_foo <- do.call(rbind, l_foo)
#'     # remove duplicates
#'     df_foo <- unique(df_foo)
#'     return(df_foo)
#'   } else{
#'     names(l_foo) <- my_R_scripts
#'     return(l_foo)
#'   }
#'
#' }
#'
#' #' Find test for a function in a codebase
#' #'
#' #' This function finds the test for each in a vector of functions in a specified
#' #' testing folder, default = tests/testthat as the relative path from the project folder (path).
#' #'
#' #' @param v_functions A vector of functions to search for.
#' #' @param path The path to the folder to be checked.
#' #' @param test_path The relative path to the test folder from the project folder (path).
#' #'
#' #' @family checking_functions
#' #'
#' #' @return A vector of file paths to the test scripts for each function.
#' #' Returns NA where no script can be found.
#' #'
#' #' @examples
#' #' \dontrun{
#' #'  v_funcs_to_find_tests_for <- c("check_init", "mean", "check_markov_trace", "find_test")
#' #'  find_test(v_functions = v_funcs_to_find_tests_for,
#' #'            path = ".")
#' #' }
#' #'
#' find_test <- function(v_functions,
#'                       path = ".",
#'                       test_path = "tests/testthat") {
#'   # get all files from the path folder, i.e. everything in repo.
#'   my_test_scripts <- list.files(
#'     path = paste0(path, "/", test_path),
#'     pattern = "\\.R$",
#'     recursive = TRUE,
#'     full.names = TRUE
#'   )
#'   # create a list of functions in scripts.
#'   l_foo <- lapply(X = my_test_scripts,
#'                   FUN = list_functions_in_script,
#'                   by_package = FALSE)
#'   names(l_foo) <- my_test_scripts
#'
#'   # loop through the vector of function names and check if they exist in the list of scripts.
#'   l_function_tests <- sapply(v_functions,
#'                              function(function_name) {
#'                                # find which elements of the list contain the function
#'                                v_test_file <- names(l_foo)[sapply(l_foo,
#'                                                                   function(x)
#'                                                                     (function_name %in% x) > 0)]
#'
#'                                # check that a file exists
#'                                if (length(v_test_file) == 0) {
#'                                  return(NA)
#'                                } else if (length(v_test_file) > 1) {
#'                                  file_path_match <-
#'                                    grep(pattern = function_name,
#'                                         x = v_test_file,
#'                                         value = TRUE)
#'                                  if (length(file_path_match) == 1) {
#'                                    return(file_path_match)
#'                                  } else{
#'                                    return(v_test_file[1])
#'                                  }
#'                                } else {
#'                                  return(v_test_file)
#'                                }
#'
#'                              })
#'
#'
#'   return(l_function_tests)
#'
#' }
#'
#'
#' #' Summarise project functions with details on packages and existence of unit-tests.
#' #'
#' #' Creates a summary table containing the name of each function in the project,
#' #' the package it is from, whether it has a unit-test and the file in which it is tested.
#' #'
#' #' @param path The path to the folder to be checked.
#' #' @param path_exclude A set of strings that will exclude any files if it is present in the file path.
#' #' @param packages_to_exclude A vector of packages to exclude from the search.
#' #' @param test_path The relative path to the test folder from the project folder (path).
#' #' @return A dataframe with the following columns:
#' #' * function_name: The name of the function.
#' #' * package: The package the function is from.
#' #' * file_name: The file in which the function is defined.
#' #' * test_location: The file in which the function is tested.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' tabulate_functions_in_folder_with_tests(
#' #' path = ".",
#' #' path_exclude = "testthat/",
#' #' packages_to_exclude = c("base", "stats", "ggplot2"),
#' #' test_path = "tests/testthat"
#' #' )
#' #' }
#' #'
#' tabulate_functions_in_folder_with_tests <-
#'   function(path = ".",
#'            path_exclude = "testthat/",
#'            packages_to_exclude = .packages(TRUE),
#'            test_path = "tests/testthat"
#'            ) {
#'
#'     # get list of functions (excluding those defined)
#'     df_foo <- tabulate_functions_in_folder(
#'       path = path,
#'       path_exclude = path_exclude,
#'       collapse = T,
#'       packages_to_exclude = packages_to_exclude
#'     )
#'
#'     # attempt to identify the testing file for the function
#'     df_foo$test_location <- find_test(
#'       v_functions = df_foo$foo,
#'       path = path,
#'       test_path = test_path
#'     )
#'
#'     return(df_foo)
#'
#'   }
#'
#'
#' #' @title Summarise project functions with details on packages and existence of unit-tests.
#' #' @description Creates a summary table containing the name of each function in the project,
#' #' the package it is from, whether it has a unit-test and the file in which it is tested.
#' #' @param path The path to the folder to be checked.
#' #' @param path_exclude A set of strings that will exclude any files if it is present in the file path.
#' #' @param packages_to_exclude A vector of packages to exclude from the search.
#' #' @param test_path The relative path to the test folder from the project folder (path).
#' #' @param cheers_pattern A string that will be used to identify the cheers tag.
#' #' @return A dataframe with the following columns:
#' #' * function_name: The name of the function.
#' #' * package: The package the function is from.
#' #' * file_name: The file in which the function is defined.
#' #' * test_location: The file in which the function is tested.
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' get_summary_table()
#' #' }
#' #'
#' get_summary_table <- function(path = ".",
#'                               path_exclude = "testthat/",
#'                               test_path = "tests/",
#'                               cheers_pattern = "@family",
#'                               packages_to_exclude = c("base",  "stats", "utils")) {
#'
#'   # table with list of functions, packages and tests.
#'   df_tests <- tabulate_functions_in_folder_with_tests(path = path,
#'                                                       path_exclude = path_exclude,
#'                                                       test_path = test_path,
#'                                                       packages_to_exclude = packages_to_exclude)
#'
#'   # tags relating to classifications and file paths
#'   df_cheers <- get_folder_cheers_classifications(path = path,
#'                                                  cheers_pattern = cheers_pattern,
#'                                                  path_ignore = path_exclude)
#'
#'   # merge dataframes together to get one summary dataframe
#'   df_merged <- merge(df_tests,
#'                      df_cheers,
#'                      by.x = "foo",
#'                      by.y = "function_name",
#'                      all = T)
#'
#'   # sort by tag in base R
#'   column_names <-
#'     c("foo", "tag", "package", "script", "test_location")
#'   df_summary <- df_merged[order(df_merged$tag), column_names]
#'
#'   return(df_summary)
#'
#' }
#'
#'
#'
