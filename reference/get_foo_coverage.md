# Get coverage by function

Get coverage by function

## Usage

``` r
get_foo_coverage(foo_folder, test_folder)
```

## Arguments

- foo_folder:

  folder containing functions

- test_folder:

  folder containing tests

## Value

a dataframe with a column for functions and a column for coverage

## Examples

``` r
# \donttest{
# Example takes more than 5 seconds to run
if(require(testthat)) {
  folder_path1 <- assertHE_example("example_project/R")
  folder_path2 <- assertHE_example("example_project/tests/testthat")
  get_foo_coverage(
    foo_folder = folder_path1,
    test_folder = folder_path2
  )
}
#> Loading required package: testthat
#> Test passed with 2 successes 🎊.
#> Test passed with 2 successes 🎊.
#> Test passed with 6 successes 🌈.
#> Test passed with 2 successes 🥳.
#> Test passed with 3 successes 😸.
#> Test passed with 2 successes 😀.
#> Test passed with 1 success 🎉.
#>                      foo_string coverage
#> 1               calculate_QALYs      1.0
#> 2               calculate_costs      1.0
#> 3 calculate_discounting_weights      1.0
#> 4           create_Markov_trace      1.0
#> 5      define_transition_matrix      1.0
#> 6          run_sickSicker_model      1.0
#> 7               utility_example      0.6
#> 8              utility_example2      0.0
# }
```
