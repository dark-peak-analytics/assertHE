# Get Title and Description from Parsed List

This function extracts the title and description from a parsed list.

## Usage

``` r
get_roxygen_description(parsed_list)
```

## Arguments

- parsed_list:

  A list containing parsed elements.

## Value

A list containing the title and description.

## Examples

``` r
parsed_list <- list(list(tag = "title", val = "Sample Title"),
                     list(tag = "description", val = "This is a sample description."))
get_roxygen_description(parsed_list)
#> $title
#> [1] "Sample Title"
#> 
#> $description
#> [1] "This is a sample description."
#> 
```
