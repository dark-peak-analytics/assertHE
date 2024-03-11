#' Source all files in target folder
#'
#' Recursively source all files in the target directory.
#' @param path a character vector of full path names; the default corresponds to the working directory. Defaults to current working directory.
#' @param fileIncRegx Character string containing regular expression for files to include. Defaults to '\\.R$' for R files.
#' @param dirExclRegx Character string containing regular expression for directories to exclude.
#' @param fileExclRegx Character string containing regular expression for files to exclude.
#' @param recursive logical. Should the listing recurse into directories?
#' @param verbose logical. Whether to print files as they are being sourced.
#'
#' @return nothing, sources functions to global environment.
source_files <- function(path = ".",
                         dirExclRegx = NULL,
                         fileIncRegx = "\\.R$",
                         fileExclRegx = NULL,
                         recursive = TRUE,
                         verbose = FALSE) {

  # Get the list of files matching fileIncRegx in directories matching dirIncRegx
  files <- list.files(path = path,
                      pattern = fileIncRegx,
                      recursive = recursive,
                      full.names = TRUE)

  # Filter out any files which might match the File exclude regex
  if (!is.null(fileExclRegx)) {
    files <- files[!grepl(fileExclRegx, files)]
  }

  # Filter out any directories which might match the Directory exclude regex
  if (!is.null(dirExclRegx)) {
    files <- files[!grepl(dirExclRegx, dirname(files))]
  }

  # Source each file
  for (f in files) {
    if(verbose) print(f)
    source(f)
  }
}
