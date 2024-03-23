% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{source_files}
\alias{source_files}
\title{source_files}
\usage{
source_files(
  file_regx = ".*",
  path = ".",
  recursive = TRUE,
  exclude_files = NULL,
  exclude_dirs = NULL,
  local = !testthat::is_testing(),
  verbose = FALSE
)
}
\arguments{
\item{file_regx}{= ".*" - a regular expression for files to source}

\item{path}{= "." - a path to search}

\item{recursive}{= TRUE - recurse into subdirectories}

\item{exclude_files}{= NULL - a regular expression for files to filter (from the results of file_regx)}

\item{exclude_dirs}{= NULL - a regular expression for directories to filter (from the results of dir_regx)}

\item{local}{= !testthat::is_testing() - see base::source() for details}

\item{verbose}{= FALSE - whether to emit the sourced files.}
}
\value{
list of files sourced
}
\description{
Source files based upon regular expression searching
}
\section{IMPORTANT !!!}{
sourcing \emph{this} file is a mistake - may result in infinite recursion
}

\examples{
\dontrun{
source_files(file_regx = ".*",  ## any file name
 path = ".*",   # the current directory and all subdirectories
 recursive = FALSE  # don't recurse
 exclude_files = ".*utility.*",   # don't source any file with the string "utility" anywhere in its basename
 exclude_dirs = "\/tmp\/"  # don't source any files which reside in any directory named "tmp", or its subdirectories
)
}

}