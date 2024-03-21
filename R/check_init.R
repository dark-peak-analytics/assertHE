#' Check and initialize a vector
#'
#' This function checks a given vector for several conditions, including values being
#' within the range 0 to 1 inclusive, the sum of values being equal to 1, no duplicate names,
#' and all elements having names.
#'
#' @param x A numeric vector with named elements.
#'
#' @return If successful there is no message, otherwise, it issues warnings with
#' informative messages for each failed condition.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- setNames(object = c(0.2, 0.3, 0.4, 0.1), nm = letters[1:4])
#' check_init(x) # Should issue warnings for values outside 0-1 and the sum not equal to 1
#'
#' x <- c(0.2, 0.3, 0.4, 0.1) # Missing names
#' check_init(x) # Should issue a warning about missing names
#'
#' x <- c(-2, 0.3, 0.4, 0.1) # Missing names
#' check_init(x) # Should issue a warning about a value below 0 and about not summing to 1
#' }
check_init <- function(x) {
  # Check that all values of x are between 0 and 1
  if (any(x < 0) || any(x > 1) || any(is.na(x))) {
    out_of_range <- x[x < 0 | x > 1]
    message <- paste("Values outside the range [0, 1]:", toString(out_of_range))
    warning(message)
  }

  # Check that the sum of values in x is equal to 1
  if (abs(sum(x, na.rm = T) - 1) > 1e-12) {
    message <- paste("Sum of values is not equal to 1. It is:", sum(x))
    warning(message)
  }

  # Check for duplicate names
  duplicate_names <- names(x)[duplicated(names(x))]
  if (length(duplicate_names) > 0) {
    message <- paste("Duplicate names detected:", toString(duplicate_names))
    warning(message)
  }

  # Check for missing names
  names_missing <- sapply(X = names(x),
                          FUN = function(x) is.character(x) & !is.na(x) & !is.null(x))
  names_missing_ind <- which(!names_missing)
  if (length(names_missing_ind) > 0) {
    message <- paste0("Some elements of the vector are missing names: Element(s) ", toString(names_missing_ind))
    warning(message)
  }

}
