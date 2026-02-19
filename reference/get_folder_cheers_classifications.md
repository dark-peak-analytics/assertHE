# Get cheers classification tags from a given folder

For a provided folder path, identify the cheers classification tags and
the function names that follow them.

## Usage

``` r
get_folder_cheers_classifications(path, cheers_pattern, path_ignore = "tests/")
```

## Arguments

- path:

  A string containing the filepath to the folder to be checked

- cheers_pattern:

  A string containing the roxygen tag for cheers which is used as an
  identifier

- path_ignore:

  A string containing the pattern to identify files to ignore

## Value

A list containing the cheers tags and the function names that follow
them

## See also

Other cheers:
[`get_file_cheers_classifications()`](https://dark-peak-analytics.github.io/assertHE/reference/get_file_cheers_classifications.md)
