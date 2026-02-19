# locate_funcs

locates the lines which define a function within a single file

## Usage

``` r
locate_funcs(file)
```

## Arguments

- file:

  = a connection object or a character string path to a file.

## Value

Returns a data frame with the following columns: func_num: The ID of the
function - monotonic increasing from 1. func_start: The line number
(within the file) of the function start. func_end: The line number of
the function end.
