# find_files

Find files based upon regular expression searching IMPORTANT - a
Directory is NOT a file. (for most instances of file systems)

## Usage

``` r
find_files(
  file_regx = ".R",
  path = ".",
  recursive = TRUE,
  exclude_files = NULL,
  exclude_dirs = NULL
)
```

## Arguments

- file_regx:

  = ".\*" - a regular expression for files to source

- path:

  = "." - a path to search

- recursive:

  = TRUE - recurse into subdirectories

- exclude_files:

  = NULL - regx for files to exclude

- exclude_dirs:

  = NULL - regx for directories to exclude

## Value

list of files

## Examples

``` r
find_files(file_regx = ".*",  ## any file name
 path = ".*",   # the current directory and all subdirectories
 recursive = FALSE,  # don't recurse
 exclude_files = ".*utility.*", # exclude "utility" anywhere in basename
 exclude_dirs = "\\<tmp\\>|/tmp/|/tmp\\>|\\<tmp/"  # exclude any directory named "tmp", or subdirs
 )
#> character(0)
```
