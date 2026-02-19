# source_lines

Sources specified lines within a single file.

## Usage

``` r
source_lines(file, lines, env)
```

## Arguments

- file:

  a connection object or a character string path to a file.

- lines:

  A vector of integers specifying the lines to be sourced.

- env:

  the environment in which to source the lines.

## Value

No return value, called for side effects.

## IMPORTANT !!!

Sourcing *this* file is a mistake - may result in infinite recursion.
