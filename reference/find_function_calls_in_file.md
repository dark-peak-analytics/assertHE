# Find all function calls in file

Searches through a file for function calls using SYMBOL_FUNCTION_CALL

## Usage

``` r
find_function_calls_in_file(
  relative_path = NULL,
  foo_strings,
  filter_for_test_that = FALSE
)
```

## Arguments

- relative_path:

  path of file to search in

- foo_strings:

  string vector of function names to search for

- filter_for_test_that:

  whether to filter for only functions used after the call to test_that.
  Default FALSE.

## Value

a dataframe with the columns 'foo' for function name and 'test_location'
which gives the file in which the function is called with the line in
which the function is called appended.

## Examples

``` r
file_path <- assertHE_example("example_project/tests/testthat/test-calculate_costs.R")
find_function_calls_in_file(
  relative_path = file_path,
  foo_strings = "calculate_costs"
)
#>        foo_string
#> 1 calculate_costs
#> 2 calculate_costs
#>                                                                                                test_location
#> 1 /home/runner/work/_temp/Library/assertHE/extdata/example_project/tests/testthat/test-calculate_costs.R#L26
#> 2 /home/runner/work/_temp/Library/assertHE/extdata/example_project/tests/testthat/test-calculate_costs.R#L33
```
