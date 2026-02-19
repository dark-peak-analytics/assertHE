# Summarise the model functions in a single folder.

Summarise the model functions in a single folder.

## Usage

``` r
summarise_model(
  project_path = ".",
  foo_folder = "R",
  exclude_files = NULL,
  exclude_dirs = NULL,
  test_folder = NULL,
  output_format = "dataframe"
)
```

## Arguments

- project_path:

  path to the project folder, if not provided, will use current working
  directory.

- foo_folder:

  path to folder containing all functions for the model

- exclude_files:

  A regular expression for files to NOT process (basename)

- exclude_dirs:

  A regular expression for directories to NOT process (dirname)

- test_folder:

  folder containing all tests

- output_format:

  output format to use, defaults to dataframe, options include latex and
  word.

## Value

dataframe with three columns. 'foo_string' contains function names,
'foo_location' contains the location of the function definitions,
'test_location' contains the locations of tests for each function (both
file and line number).

## Examples

``` r
project_path <- assertHE_example("example_project")
foo_folder  <- "R"
test_folder <- "tests/testthat"

summarise_model(
  project_path = project_path,
  foo_folder = foo_folder,
  test_folder =  test_folder
)
#>                       foo_string                          foo_location
#> 1                calculate_QALYs               R/calculate_QALYs.R#L40
#> 2                calculate_costs               R/calculate_costs.R#L39
#> 3                calculate_costs               R/calculate_costs.R#L39
#> 4  calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 5  calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 6  calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 7  calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 8  calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 9  calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 10           create_Markov_trace           R/create_markov_trace.R#L44
#> 11      define_transition_matrix      R/define_transition_matrix.R#L29
#> 12      define_transition_matrix      R/define_transition_matrix.R#L29
#> 13      define_transition_matrix      R/define_transition_matrix.R#L29
#> 14                      mat_mult              R/calculate_QALYs.R#L107
#> 15          run_sickSicker_model          R/run_sickSicker_model.R#L36
#> 16               utility_example                          R/utils.R#L3
#> 17              utility_example2                         R/utils.R#L14
#>                                                     test_location
#> 1                       tests/testthat/test-calculate_QALYs.R#L26
#> 2                       tests/testthat/test-calculate_costs.R#L33
#> 3                       tests/testthat/test-calculate_costs.R#L26
#> 4          tests/testthat/test-calculate_discounting_weights.R#L8
#> 5          tests/testthat/test-calculate_discounting_weights.R#L9
#> 6         tests/testthat/test-calculate_discounting_weights.R#L19
#> 7         tests/testthat/test-calculate_discounting_weights.R#L24
#> 8         tests/testthat/test-calculate_discounting_weights.R#L29
#> 9         tests/testthat/test-calculate_discounting_weights.R#L10
#> 10                  tests/testthat/test-create_Markov_trace.R#L30
#> 11             tests/testthat/test-define_transition_matrix.R#L17
#> 12             tests/testthat/test-define_transition_matrix.R#L32
#> 13             tests/testthat/test-define_transition_matrix.R#L41
#> 14                                                           <NA>
#> 15                 tests/testthat/test-run_sickSicker_model.R#L32
#> 16 tests/testthat/test-utility_functions_with_different_name.R#L5
#> 17                                                           <NA>

summarise_model(
  project_path = project_path,
  foo_folder = foo_folder,
  test_folder =  NULL
)
#>                      foo_string                          foo_location
#> 1               calculate_QALYs               R/calculate_QALYs.R#L40
#> 2                      mat_mult              R/calculate_QALYs.R#L107
#> 3               calculate_costs               R/calculate_costs.R#L39
#> 4 calculate_discounting_weights R/calculate_discounting_weights.R#L29
#> 5           create_Markov_trace           R/create_markov_trace.R#L44
#> 6      define_transition_matrix      R/define_transition_matrix.R#L29
#> 7          run_sickSicker_model          R/run_sickSicker_model.R#L36
#> 8               utility_example                          R/utils.R#L3
#> 9              utility_example2                         R/utils.R#L14
#>   test_location
#> 1          <NA>
#> 2          <NA>
#> 3          <NA>
#> 4          <NA>
#> 5          <NA>
#> 6          <NA>
#> 7          <NA>
#> 8          <NA>
#> 9          <NA>
```
