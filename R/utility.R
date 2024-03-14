

#' source files based upon regular expression searching
#' # IMPORTANT !!!
#' sourcing *this* file is a mistake - may result in infinite recursion
#' @param file_regx = ".*" - a regular expression for files to source
#' @param path = "." - a path to search
#' @param recursive = TRUE - recurse into subdirectories 
#' @param exclude_files = NULL - a regular expression for files to filter (from the results of file_regx)
#' @param exclude_dirs = NULL - a regular expression for directories to filter (from the results of dir_regx)
#' @param local = !testthat::is_testing() - see base::source() for details
#' @param verbose = FALSE - whether to emit the sourced files.
#' 
#' @return list of files sourced
#'
#' @export
#'
#' @importFrom base
#'
#' @examples
#' \dontrun{
#' source_files(file_regx = ".*",  ## any file name
#'  path = ".*",   # the current directory and all subdirectories
#'  recursive = FALSE  # don't recurse
#'  exclude_files = ".*utility.*",   # don't source any file with the string "utility" anywhere in its basename
#'  exclude_dirs = "\/tmp\/"  # don't source any files which reside in any directory named "tmp", or its subdirectories
#' )
#' }
#'
source_files <- function( file_regx = ".*",  
                          path = ".", 
                          recursive = TRUE, 
                          exclude_files = NULL, 
                          exclude_dirs = NULL, 
                          local=!testthat::is_testing(), 
                          verbose=FALSE) {
  
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
    source(file, local=local, echo=verbose)
  }
  return (files)
}
