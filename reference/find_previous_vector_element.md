# Find the previous element of the vector before a value

Find the previous element of the vector before a value

## Usage

``` r
find_previous_vector_element(value, vector, LTE = FALSE)
```

## Arguments

- value:

  A value of numeric values

- vector:

  A vector of numeric values

- LTE:

  a boolean to determine collection on "less than" or "less than equal"

## Value

The previous element of the vector before the value

## Examples

``` r
find_previous_vector_element(value = 5, vector = 1:10)
#> [1] 4
find_previous_vector_element(value = 5, vector = 6:10)
#> [1] NA
find_previous_vector_element(value = 5, vector = 5:10, LTE = FALSE)
#> [1] NA
find_previous_vector_element(value = 5, vector = 5:10, LTE = TRUE)
#> [1] 5
```
