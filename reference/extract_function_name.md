# Extract function name from a string

Extract function name from a long string. This works by identifying
"function(" in the string and then finding the operand before and
splitting on that before keeping the character there.

## Usage

``` r
extract_function_name(string)
```

## Arguments

- string:

  A string containing a function definition, this must contain the word
  'function'

## Value

A string containing the function name

## Examples

``` r
extract_function_name("better_name <- function(x){\n more code} asfdas <- function(x){}")
#> [1] "better_name" "asfdas"     
extract_function_name("better_name <- function(x){\n more code}")
#> [1] "better_name"
```
