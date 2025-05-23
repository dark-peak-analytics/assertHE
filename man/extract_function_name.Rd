% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cheers_checker.R
\name{extract_function_name}
\alias{extract_function_name}
\title{Extract function name from a string}
\usage{
extract_function_name(string)
}
\arguments{
\item{string}{A string containing a function definition, this must contain the word 'function'}
}
\value{
A string containing the function name
}
\description{
Extract function name from a long string. This works by
identifying "function(" in the string and then finding the operand before and splitting
on that before keeping the character there.
}
\examples{
extract_function_name("better_name <- function(x){\n more code} asfdas <- function(x){}")
extract_function_name("better_name <- function(x){\n more code}")

}
