# assertHE 1.0.1

* Fixes issue arising from `dplyr` updates. `dplyr` recently removed `dplyr::location()`. `assertHE` referenced a column named `location` but does not note this as a global variable with `utils::globalVariables("location")` which did not cause an issue because `dplyr` was exporting a function with the same name `location()`.
* Unifies coverage percentage where test location/folder is `NA`.

# assertHE 1.0.0

* Initial CRAN submission.
