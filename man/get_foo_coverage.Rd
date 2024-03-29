% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_foo_coverage.R
\name{get_foo_coverage}
\alias{get_foo_coverage}
\title{Get coverage by function}
\usage{
get_foo_coverage(foo_folder, test_folder)
}
\arguments{
\item{foo_folder}{folder containing functions}

\item{test_folder}{folder containing tests}
}
\value{
a dataframe with a column for functions and a column for coverage
}
\description{
Get coverage by function
}
\examples{
\dontrun{
get_foo_coverage(
foo_folder = "tests/testthat/example_project/R",
test_folder = "tests/testthat/example_project/tests/testthat"
)

get_foo_coverage(
foo_folder = "R",
test_folder = "tests/testthat"
)
}

}
