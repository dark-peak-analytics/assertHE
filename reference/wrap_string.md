# Wrap a string to lines of a specified width

This function takes an input string and wraps it to lines of a specified
width, breaking the string at word boundaries.

## Usage

``` r
wrap_string(input_string, width = 80)
```

## Arguments

- input_string:

  The input string to be wrapped.

- width:

  The maximum width of each line. Default is 80 characters.

## Value

A character vector where each element represents a line of the wrapped
string.

## Examples

``` r
input_string <- "This is a long string that needs to be wrapped to fit within
                a specified width."
wrapped_lines <- wrap_string(input_string, width = 30)
cat(wrapped_lines, sep = "\n")
#> 
#>  This is a long string that
#> needs to be wrapped to fit
#> within
#>                 a
#> specified width.
```
