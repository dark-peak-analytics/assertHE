#' Calculate Discounting Weights
#'
#' @description Calculate the discounting weights that correspond to a defined
#' model time-horizon. Discounting could be employed starting from the initial
#' cycle.
#'
#' @param discount_rate_ Numeric, the rate used to estimate the discounting
#' weights.
#' @param time_horizon_ Numeric, time horizon, number of cycles for which the
#' discounting weights are to be calculated.
#' @param first_cycle_ Logical, for whether to start discounting from the first
#' cycle. Default is FALSE, where the discounting weight corresponding to the
#' first cycle is equal to one.
#'
#' @return A numeric vector containing the discounting weights.
#'
#' @family discounting
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("sicksickerPack")
#' calculate_discounting_weights(
#'   discount_rate_ = 0.035,
#'   time_horizon_ = 1,
#'   first_cycle_ = FALSE
#' )
#' }
calculate_discounting_weights <- function(discount_rate_ = 0.035,
                                          time_horizon_,
                                          first_cycle_ = FALSE) {
  ## Sanity checks - inputs:

  # ensure discount_rate_ is a positive double
  assertthat::assert_that(
    assertthat::is.number(discount_rate_),
    msg = paste(
      "The object passed to the discount_rate_ argument is not of class numeric"
    )
  )
  assertthat::assert_that(
    all(discount_rate_ > 0, discount_rate_ < 1),
    msg = paste(
      "The object passed to the discount_rate_ argument is less than zero or",
      "more than 1"
    )
  )
  # confirm time_horizon_ is numeric of length 1
  assertthat::assert_that(
    assertthat::is.count(time_horizon_),
    msg = paste(
      "The object passed to the time_horizon_ argument is not a positive",
      "integer"
    )
  )
  # ensure the object passed to argument first_cycle_ is of class logical
  assertthat::assert_that(
    assertthat::is.flag(first_cycle_),
    msg = paste(
      "The object passed to the first_cycle_ argument is not a logical; please",
      "use TRUE or FALSE, T or F, only"
    )
  )

  ## Calculate discount weights:

  v_dw <- if(!first_cycle_) {
    1 / (1 + discount_rate_) ^ (0:(time_horizon_ - 1))
  } else {
    1 / (1 + discount_rate_) ^ (1:(time_horizon_))
  }

  return(v_dw)
}
