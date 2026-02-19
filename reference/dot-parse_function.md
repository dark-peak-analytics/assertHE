# Parse Function

This function parses an R expression, breaking it down into its
components.

## Usage

``` r
.parse_function(x)
```

## Arguments

- x:

  An R expression to be parsed.

## Value

A character string or a list of parsed components, depending on the
input expression.

## Details

If the input expression `x` is not an atomic value, symbol, or an
environment pointer, the function breaks it up into a list of
components. It also handles expressions of the form `foo$bar` by
splitting them up, keeping only the relevant parts for parsing.

If `x` is a list of expressions, the function recursively parses each
expression until they can no longer be listed, filtering out atomic
values in the process.

If `x` is not listable (e.g. a function), it is deparsed into a
character string.
