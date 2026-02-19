# Get path to assertHE example

assertHE comes bundled with a number of sample files in its
`inst/extdata` directory. This function make them easy to access

## Usage

``` r
assertHE_example(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Value

If `file` is `NULL`, returns a character vector containing the names of
all files and directories available in the package's directory
(`extdata`). If `file` specifies the name of an existing example file,
returns a character vector of length one containing the full path to
that file. Stops with an error if the specified `file` does not exist
within the example directory.

## Examples

``` r
assertHE_example()
#> [1] "example_external_function" "example_notests"          
#> [3] "example_project"           "example_scripts"          
assertHE_example("example_scripts/example_tricky_functions.R")
#> [1] "/home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R"
```
