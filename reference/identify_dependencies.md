# Identify Dependencies

Identify dependencies between functions.

## Usage

``` r
identify_dependencies(v_unique_foo, pkg_env = environment())
```

## Arguments

- v_unique_foo:

  Vector of unique function strings.

- pkg_env:

  The package environment where the functions are defined (e.g. global).

## Value

A dataframe with two columns ("from" and "to") representing the
dependencies.
