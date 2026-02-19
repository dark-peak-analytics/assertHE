# Find specific function calls in a folder

Runs find_function_calls_in_file on all files in a folder, and combined
results into a single dataframe

## Usage

``` r
find_function_calls_in_folder(
  test_folder,
  foo_strings,
  filter_for_test_that = FALSE
)
```

## Arguments

- test_folder:

  folder containing all tests

- foo_strings:

  string vector of function names to search for

- filter_for_test_that:

  whether to filter for only functions used after the call to test_that.
  Default FALSE.

## Value

dataframe with two columns. 'foo' contains function names, test_location
contains the location of the tests for each function (file and line
number).

## Examples

``` r
folder_path <- assertHE_example("example_project/tests/testthat")
find_function_calls_in_folder(
  foo_strings = c("calculate_costs", "calculate_QALYs",
    "create_Markov_trace", "FOO_WITH_NO_TESTS"),
  test_folder = folder_path
)
#>            foo_string
#> 1   FOO_WITH_NO_TESTS
#> 2     calculate_QALYs
#> 3     calculate_costs
#> 4     calculate_costs
#> 5 create_Markov_trace
#>                                                                                                    test_location
#> 1                                                                                                           <NA>
#> 2     /home/runner/work/_temp/Library/assertHE/extdata/example_project/tests/testthat/test-calculate_QALYs.R#L26
#> 3     /home/runner/work/_temp/Library/assertHE/extdata/example_project/tests/testthat/test-calculate_costs.R#L26
#> 4     /home/runner/work/_temp/Library/assertHE/extdata/example_project/tests/testthat/test-calculate_costs.R#L33
#> 5 /home/runner/work/_temp/Library/assertHE/extdata/example_project/tests/testthat/test-create_Markov_trace.R#L30
```
