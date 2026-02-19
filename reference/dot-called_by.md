# Called By

Identify functions called by a given function within a specified project
folder

## Usage

``` r
.called_by(fname, all_functions, pkg_env)
```

## Arguments

- fname:

  The name of the target function.

- all_functions:

  A character vector of all function names in the project.

- pkg_env:

  The package environment where the functions are defined (e.g. global).

## Value

A dataframe with two columns ("from" and "to") representing the
dependencies of the target function. Returns NA if no dependencies are
found.

## Details

The function identifies functions called by the target function `fname`
within the specified package environment `pkg_env`. It searches for
dependencies within the literal code of the function body and returns a
dataframe with two columns ("from" and "to") representing the
dependencies. If no dependencies are found, it returns a dataframe with
"from" as the target function and "to" as NA.

Note: This function may potentially miss calls if they are in attributes
of the closure. For example when function is defined within another
function, capturing the environment of the outer function.
