# Find the next element of the vector after a value

Find the next element of the vector after a value

## Usage

``` r
find_next_vector_element(value, vector, LTE = FALSE)
```

## Arguments

- value:

  A value of numeric values

- vector:

  A vector of numeric values

- LTE:

  a boolean to determine collection on "greater than or equal"

## Value

The next element of the vector after the value

## Examples

``` r
find_next_vector_element(value = 5, vector = 1:10)
#> [1] 6
find_next_vector_element(value = 5, vector = 1:4)
#> [1] NA
find_next_vector_element(value = 5, vector = 1:5, LTE = FALSE)
#> [1] NA
find_next_vector_element(value = 5, vector = 1:5, LTE = TRUE)
#> [1] 5
```
