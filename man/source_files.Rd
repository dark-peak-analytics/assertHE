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

\item{exclude_files}{= NULL - regx for files to exclude}

\item{exclude_dirs}{= NULL - regx for directories to exclude}

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
 recursive = FALSE,  # don't recurse
 exclude_files = ".*utility.*", # exclude "utility" anywhere in basename
 exclude_dirs = "\/tmp\/"  # exclude any directory named "tmp", or subdirs
 )
}

}
