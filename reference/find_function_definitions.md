# Parses an R source file, returns function names defined within.

Using utils::getParseData(), searches for function definitions by
matching the FUNCTION keyword (i.e. "function") with it's associated
SYMBOL (i.e the function name)

## Usage

``` r
find_function_definitions(filename)
```

## Arguments

- filename:

  A string containing a path to an R source file

## Value

A dataframe with interesting information

## Examples

``` r
file_path <- assertHE_example("example_scripts/example_tricky_functions.R")
find_function_definitions(filename = file_path)
#>     line1 col1 line2 col2  id parent  token terminal                 text
#> 6       3    1     3   19   6      8 SYMBOL     TRUE  do_something_random
#> 65      9    1     9   19  65     67 SYMBOL     TRUE  calculate_something
#> 153    17    1    17   12 153    155 SYMBOL     TRUE         find_matches
#> 308    37    1    37   15 308    310 SYMBOL     TRUE      combine_strings
#> 387    43    1    43   12 387    389 SYMBOL     TRUE         process_data
#> 457    55    1    55   14 457    459 SYMBOL     TRUE       transform_data
#> 564    65    1    65   11 564    566 SYMBOL     TRUE          sort_values
#> 629    72    1    72   15 629    631 SYMBOL     TRUE      generate_output
#> 692    78    1    78   13 692    694 SYMBOL     TRUE        do_everything
#> 858    99    6    99   25 858    860 SYMBOL     TRUE lots_of_comments_foo
#> 967   115    1   115   11 967    969 SYMBOL     TRUE          foo_outside
#> 977   116    3   116   12 977    979 SYMBOL     TRUE           foo_inside
#>                                                                                          source
#> 6   /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 65  /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 153 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 308 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 387 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 457 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 564 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 629 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 692 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 858 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 967 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
#> 977 /home/runner/work/_temp/Library/assertHE/extdata/example_scripts/example_tricky_functions.R
```
