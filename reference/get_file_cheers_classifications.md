# Get cheers classification tags from a given file

For a provided filepath, identify the cheers classification tags and the
function names that follow them.

## Usage

``` r
get_file_cheers_classifications(
  filename,
  cheers_pattern,
  function_pattern = "(\\s|=|-)function\\("
)
```

## Arguments

- filename:

  A string containing the filepath to the file to be checked

- cheers_pattern:

  A string containing the roxygen tag for cheers which is used as an
  identifier

- function_pattern:

  A string containing the pattern to identify functions

## Value

A list containing the cheers tags and the function names that follow
them

## See also

Other cheers:
[`get_folder_cheers_classifications()`](https://dark-peak-analytics.github.io/assertHE/reference/get_folder_cheers_classifications.md)
