# source_funcs

Sources *only* the functions discovered in an R file.

## Usage

``` r
source_funcs(file, env)
```

## Arguments

- file:

  a connection object or a character string path to a file.

- env:

  the environment in which to source the functions.

## Value

No return value, called for side effects.

## IMPORTANT !!!

Sourcing *this* file is a mistake - may result in infinite recursion.
