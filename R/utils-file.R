#' @title locate_funcs
#' @description locates the lines which define a function within a single file
#' @param file = a connection object or a character string path to a file.
#'
#' @return Returns a data frame with the following columns:
#' func_num: The ID of the function - monotonic increasing from 1.
#' func_start: The line number (within the file) of the function start.
#' func_end: The line number of the function end.
#'
#' @export
#'
locate_funcs <-  function(file) {
  df <- utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)

  # Find number of tokens which identify the use of the keyword 'function'
  func_cnt <- nrow(df[df$token == "FUNCTION", ])

  list_out <- lapply(
    X = 1:func_cnt,
    FUN = function(func_id) {
      # get the 'id' of the expression defining a 'function'
      parent_id <-
        df[df$token == "FUNCTION", ][func_id, "parent"]

      # get 'id' of the of expression assigning a name to the defined function
      parent_of_assign <- df[df$id == parent_id, "parent"]

      # We only need these elements, so remove everything else
      filtered_df <-
        df[df$parent %in% c(parent_id, parent_of_assign), ]

      # Make sure we're not processing a 'lambda' ????
      if (all(!c("EQ_ASSIGN", "LEFT_ASSIGN") %in% filtered_df$token)) {
        # This causes a NULL to be returned !!!
        return()
      }

      # get the start and end line numbers of the function definition
      func_start <- filtered_df[1, "line1"]
      # largest line2 val
      func_end <- filtered_df[nrow(filtered_df), "line2"]

      return (c(
        "func_num" = func_id,
        "func_start" = func_start,
        "func_end" = func_end
      ))

    }
  )

  if (!is.null(list_out)) {
    ret <- do.call(rbind, list_out)
  } else {
    ret <- NULL
  }

  return(ret)
}

#' @title find_files
#' @description Find files based upon regular expression searching
#' IMPORTANT - a Directory is NOT a file. (for most instances of file systems)
#' @param file_regx = ".*" - a regular expression for files to source
#' @param path = "." - a path to search
#' @param recursive = TRUE - recurse into subdirectories
#' @param exclude_files = NULL - regx for files to exclude
#' @param exclude_dirs = NULL - regx for directories to exclude
#'
#' @return list of files
#'
#' @export
#'
#' @examples
#' find_files(file_regx = ".*",  ## any file name
#'  path = ".*",   # the current directory and all subdirectories
#'  recursive = FALSE,  # don't recurse
#'  exclude_files = ".*utility.*", # exclude "utility" anywhere in basename
#'  exclude_dirs = "\\<tmp\\>|/tmp/|/tmp\\>|\\<tmp/"  # exclude any directory named "tmp", or subdirs
#'  )
#'
find_files <- function( file_regx = ".R",
                          path = ".",
                          recursive = TRUE,
                          exclude_files = NULL,
                          exclude_dirs = NULL) {

  # Get the list of files matching file_regx in directories matching dir_regx
  files <- list.files(path = path, pattern = file_regx, recursive=recursive, full.names = TRUE)

  # Filter out directories - can't source a directory !
  files <- files[file.info(files)$isdir == FALSE]

  # Filter out any files which match the File exclude regex
  if (!is.null(exclude_files) && !is.na(exclude_files)) {
    files <- files[!grepl(exclude_files, basename(files))]
  }

  # Filter out any directories which match the Directory exclude regex
  if (!is.null(exclude_dirs) && !is.na(exclude_dirs)) {
    files <- files[!grepl(exclude_dirs, dirname(files))]
  }

  files <- files[!is.na(files)]
  return (files)
}

#' @title source_lines
#' @description Sources specified lines within a single file.
#' # IMPORTANT !!!
#' Sourcing *this* file is a mistake - may result in infinite recursion.
#' @param file a connection object or a character string path to a file.
#' @param lines A vector of integers specifying the lines to be sourced.
#' @param env the environment in which to source the lines.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
source_lines <- function(file, lines, env){

  # Check if 'file' is a character string
  if (is.character(file) && !file.exists(file)) {
    stop("'file' must be a valid file name or a valid connection.")
  }

  # read all lines of the file
  all_lines <- suppressWarnings(readLines(file))


  # filter selected lines only
  selected_lines <- all_lines[lines]

  # get the lines of code ready to source
  connection <- textConnection(object = selected_lines)

  # source the lines of code
  source(connection, local = env)

}

#' @title source_funcs
#' @description Sources *only* the functions discovered in an R file.
#' # IMPORTANT !!!
#' Sourcing *this* file is a mistake - may result in infinite recursion.
#' @param file a connection object or a character string path to a file.
#' @param env the environment in which to source the functions.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
source_funcs <- function(file, env){

  # identify which lines of the file are defining functions
  func_locs <- locate_funcs(file)

  # If there are no functions, don't continue - (may cause problems from usage)
  if(is.null(func_locs))  return()

  # for each function, generate the line numbers of interest (start...end)
  # into a vector of integers to be sourced
  func_lines <- unlist(
    lapply(1:nrow(func_locs), function(i) {
      seq(
        func_locs[i, "func_start"],
        func_locs[i, "func_end"]
      )
    })
  )

  # source the functions
  source_lines(file = file,
               lines = func_lines,
               env = env)

}

#' Get path to assertHE example
#'
#' assertHE comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#'
#' @return
#' If `file` is `NULL`, returns a character vector containing the names of all
#' files and directories available in the package's directory (`extdata`).
#' If `file` specifies the name of an existing example file, returns a character
#' vector of length one containing the full path to that file. Stops with an
#' error if the specified `file` does not exist within the example directory.
#'
#' @examples
#' assertHE_example()
#' assertHE_example("example_scripts/example_tricky_functions.R")
#'
assertHE_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "assertHE"))
  } else {
    system.file("extdata", file, package = "assertHE", mustWork = TRUE)
  }
}
