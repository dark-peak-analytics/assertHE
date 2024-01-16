#' List all of the functions in a script
#'
#' This function lists all of the functions in a script. It expects the script to be
#' an R file, and requires that package libraries are loaded when identifying packages.
#'
#' @param filename The name of the file to be checked.
#' @param alphabetic If TRUE, return the functions in alphabetic order.
#'
#' @return A named list of functions in the script, by package.
#' @importFrom utils find getParseData stack
list_functions_in_script <- function(filename,
                                     alphabetic=TRUE) {

  # from hrbrmstr, StackExchange 3/1/2015
  if(!file.exists(filename)) { stop("couldn't find file ",filename) }
  if(!tools::file_ext(filename) == "R") { warning("expecting *.R file, will try to proceed") }
  # read in parsed data from script
  tmp <- utils::getParseData(parse(filename, keep.source=TRUE))
  # only keep those lines that are identified as function calls
  nms <- tmp$text[which(tmp$token=="SYMBOL_FUNCTION_CALL")]
  # only keep unique functions for script
  funs <- unique(if(alphabetic) { sort(nms) } else { nms })
  # return a list of functions and the file they were found in
  src <- paste(as.vector(sapply(funs, utils::find)))
  outlist <- tapply(funs, factor(src), c)

  return(outlist)

}


#' Create table of all of the functions in a script
#'
#' This function tabulates all of the functions in a script. It expects the script to be
#' an R file, and requires that package libraries are loaded when identifying packages.
#'
#' @param filename The name of the file to be checked.
#' @param packages_to_exclude A vector of packages to exclude from the output (e.g. "base")
#'
#' @return A data-frame of functions and the package the function is from.
tabulate_functions_in_script <- function(filename,
                                         packages_to_exclude = c("base", "stats", "utils")) {
  # list the functions in the file.
  my_packages <- list_functions_in_script(filename)

  # convert nested list to a dataframe where each row gives the function name
  # and the package it belongs to:
  df <- stack(my_packages)
  colnames(df) <- c("function", "package")

  # remove 'package:' from the strings
  df$package <- gsub("package:", "", df$package)

  # remove 'character(0)' and replace with 'local'
  df$package[df$package == "character(0)"] <- "unknown"

  # exclude unwanted packages (to reduce size)
  exclude_index <- df$package %in% packages_to_exclude
  df <- df[!exclude_index, ]

  return(df)
}


#' Create table of all of the functions identified in a project folder
#'
#' This function tabulates all of the functions identified in R scripts within a
#' project folder. It requires that package libraries are loaded when identifying
#' a function's package.
#'
#' @param path The path to the folder to be checked.
#' @param collapse If TRUE, return a single data-frame of all functions. Else return a list by file.
#' @param packages_to_exclude A vector of packages to exclude from the output (e.g. "base")
#'
#' @return Either a data-frame of functions and the package the function is from, or a list of functions by file.
#' @export
#' @examples
#'
#' \dontrun{
#' tabulate_functions_in_folder(
#'     path = ".",
#'     collapse = T,
#'     packages_to_exclude = c("base", "stats", "utils")
#'     )
#' }
tabulate_functions_in_folder <- function(path = ".",
                                         collapse = T,
                                         packages_to_exclude = c("base", "stats", "utils")) {
  # get all files from the path folder, i.e. everything in repo.
  my_R_scripts <- list.files(
    path = path,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )

  l_foo <- lapply(X = my_R_scripts,
                  FUN = tabulate_functions_in_script,
                  packages_to_exclude = packages_to_exclude)

  # collapse the list into a single dataframe
  if (collapse) {
    df_foo <- do.call(rbind, l_foo)
    # remove duplicates
    df_foo <- unique(df_foo)
    return(df_foo)
  } else{
    names(l_foo) <- my_R_scripts
    return(l_foo)
  }

}
