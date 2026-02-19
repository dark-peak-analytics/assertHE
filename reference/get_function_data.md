# Retrieve Function data to a list

This function retrieves data about the arguments and body of a specified
function.

## Usage

``` r
get_function_data(foo_name, envir = environment())
```

## Arguments

- foo_name:

  The name of the function to retrieve data from.

- envir:

  The environment in which to look for the function.

## Value

A list with elements for 'arguments' and 'body' of the specified
function.
