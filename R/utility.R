

#' @title source_files
#' @description Source files based upon regular expression searching
#' # IMPORTANT !!!
#' sourcing *this* file is a mistake - may result in infinite recursion
#' @param file_regx = ".*" - a regular expression for files to source
#' @param path = "." - a path to search
#' @param recursive = TRUE - recurse into subdirectories
#' @param exclude_files = NULL - regx for files to exclude
#' @param exclude_dirs = NULL - regx for directories to exclude
#' @param verbose = FALSE - whether to emit the sourced files.
#' @param keep_source = FALSE - whether to keep the source data when using source.
#'
#' @return list of files sourced
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' source_files(file_regx = ".*",  ## any file name
#'  path = ".*",   # the current directory and all subdirectories
#'  recursive = FALSE,  # don't recurse
#'  exclude_files = ".*utility.*", # exclude "utility" anywhere in basename
#'  exclude_dirs = "\/tmp\/"  # exclude any directory named "tmp", or subdirs
#'  )
#' }
#'
source_files <- function( file_regx = ".R",
                          path = ".",
                          recursive = TRUE,
                          exclude_files = NULL,
                          exclude_dirs = NULL,
                          verbose=FALSE,
                          keep_source=FALSE) {

  # Get the list of files matching file_regx in directories matching dir_regx
  files <- list.files(path = path, pattern = file_regx, recursive=recursive, full.names = TRUE)

  # Filter out directories - can't source a directory !
  files <- files[file.info(files)$isdir == FALSE]

  # Filter out any files which match the File exclude regex
  if (!is.null(exclude_files)) {
    files <- files[!grepl(exclude_files, basename(files))]
  }

  # Filter out any directories which match the Directory exclude regex
  if (!is.null(exclude_dirs)) {
    files <- files[!grepl(exclude_dirs, dirname(files))]
  }

  # Source each file
  for (file in files) {
    source(file = file, echo=verbose, keep.source = keep_source)
  }
  return (files)
}





#' Wrap a string to lines of a specified width
#'
#' This function takes an input string and wraps it to lines of a specified
#' width, breaking the string at word boundaries.
#'
#' @param input_string The input string to be wrapped.
#' @param width The maximum width of each line. Default is 80 characters.
#' @return A character vector where each element represents a line of the
#'   wrapped string.
#' @examples
#' input_string <- "This is a long string that needs to be wrapped to fit within
#'                 a specified width."
#' wrapped_lines <- wrap_string(input_string, width = 30)
#' cat(wrapped_lines, sep = "\n")
#'
#' @export
wrap_string <- function(input_string,
                        width = 80) {

  words <- unlist(strsplit(input_string, " "))
  lines <- ""
  current_line <- ""

  for (word in words) {
    if (nchar(current_line) + nchar(word) > width) {
      lines <- c(lines, current_line)
      current_line <- word
    } else {
      current_line <- paste(current_line, word, sep = " ")
    }
  }

  lines <- c(lines, current_line)

  return(lines)

}
