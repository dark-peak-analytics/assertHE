# Check and initialize a vector

This function checks a given vector for several conditions, including
values being within the range 0 to 1 inclusive and the sum of values
being equal to 1. If the vector is named, the function checks all
elements have names and no names are duplicates.

## Usage

``` r
check_init(x)
```

## Arguments

- x:

  A numeric vector with named elements.

## Value

If successful there is no message, otherwise, it issues warnings with
informative messages for each failed condition.

## Examples

``` r
x <- setNames(object = c(0.2, 0.3, 0.4, 0.1), nm = letters[1:4])
check_init(x) # x is a valid input, no warnings issued

x <- setNames(c(0.2, 0.3, 0.4, 0.1), nm = c("H", NA, "NA", "D"))
check_init(x) # Should issue a warning about missing names
#> Warning: Some elements of the vector are missing names: Element(s) 2

x <- c(-2, 0.3, 0.4, 0.1)
check_init(x) # Should issue a warning about a value below 0 and about not summing to 1
#> Warning: Values outside the range [0, 1]: -2
#> Warning: Sum of values is not equal to 1. It is: -1.2
```
