# Creates summary of R files in folder with functions defined within and locations.

Applies find_function_definitions to each file in a folder and aggregate
results

## Usage

``` r
find_folder_function_definitions(
  foo_folder = ".",
  f_excl = NULL,
  d_excl = NULL
)
```

## Arguments

- foo_folder:

  A folder to apply find_function_definitions to each script in.

- f_excl:

  A regular expression for files to NOT process (basename)

- d_excl:

  A regular expression for directories to NOT process (dirname)

## Value

A dataframe containing a column for function string and a column for
function location.

## Examples

``` r
# Skip listed files "somefile.R", and "another_file.R"
folder_path <- assertHE_example("example_project")
find_folder_function_definitions(
   foo_folder = folder_path,
   f_excl = "\\b(somefile\\.R|another_file\\.R)\\b"
)
#>                      foo_string
#> 1               calculate_QALYs
#> 2                      mat_mult
#> 3               calculate_costs
#> 4 calculate_discounting_weights
#> 5           create_Markov_trace
#> 6      define_transition_matrix
#> 7          run_sickSicker_model
#> 8               utility_example
#> 9              utility_example2
#>                                                                                             foo_location
#> 1               /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/calculate_QALYs.R#L40
#> 2              /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/calculate_QALYs.R#L107
#> 3               /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/calculate_costs.R#L39
#> 4 /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/calculate_discounting_weights.R#L29
#> 5           /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/create_markov_trace.R#L44
#> 6      /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/define_transition_matrix.R#L29
#> 7          /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/run_sickSicker_model.R#L36
#> 8                          /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/utils.R#L3
#> 9                         /home/runner/work/_temp/Library/assertHE/extdata/example_project/R/utils.R#L14
```
