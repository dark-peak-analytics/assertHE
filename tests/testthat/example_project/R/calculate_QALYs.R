#' Calculate Quality Adjusted Life Years (QALYs)
#'
#' @description Calculate the Quality Adjusted Life Years (QALYs) associated
#' with certain states in a Markov model.
#'
#' @param Markov_trace_ Numeric matrix representing the Markov trace of the
#' model.
#' @param utilities_ Numeric vector defining the utilities associated with each
#' state in a Markov model.
#' @param discounting_weights_ Numeric vector containing the discounting weights
#' associated with each cycle.
#'
#' @return A numeric vector containing the QALYs accrued over a Markov model
#' time horizon, for each state.
#'
#' @family utilities
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Markov_trace <- matrix(
#'   data = c(1, 0, 0, 0,
#'            0.845, 0.15, 0, 0.005,
#'            0.789025, 0.1837612, 0.01575, 0.01146377,
#'            0.7586067, 0.1881968, 0.03427491, 0.01892157,
#'            0.7351211, 0.1853199, 0.05235988, 0.02719916),
#'   nrow = 5,
#'   byrow = TRUE,
#'   dimnames = list(1:5,
#'                  c("H", "S1", "S2", "D"))
#'  )
#'
#' discounted_QALYs <- calculate_QALYs(
#'   Markov_trace_ = Markov_trace,
#'   utilities_ = c("u_H" = 1, "u_S1" = 0.75, "u_S2" = 0.5, "u_D" = 0),
#'   discounting_weights_ = c(0.9661836, 0.9335107, 0.901942, 0.871442, 0.841973)
#' )
#' }
calculate_QALYs <- function(Markov_trace_,
                            utilities_,
                            discounting_weights_) {
  ## Sanity checks - inputs:

  # confirm no missing values in values passed to each argument
  for(x in c("discounting_weights_", "Markov_trace_", "utilities_")) {
    assertthat::assert_that(
      assertthat::noNA(get(x)),
      msg = paste(
        "The object passed to the",
        x,
        "argument contains one or more missing value(s)"
      )
    )
  }
  # ensure passed objects are of the correct type/class
  assertthat::assert_that(
    any(is.matrix(Markov_trace_), is.array(Markov_trace_)),
    msg = paste(
      "The object passed to the Markov_trace_ argument is not of class",
      "matrix/array"
    )
  )
  for(x in c("utilities_", "discounting_weights_")) {
    assertthat::assert_that(
      assertthat::noNA(get(x)),
      msg = paste(
        "The object passed to the",
        x,
        "argument is not of class numeric"
      )
    )
  }
  # confirm all utilities_ are 1 or less
  assertthat::assert_that(
    all(utilities_ <= 1),
    msg = paste(
      "The object passed to the utilities_ argument contains one or more",
      "value(s) more than 1"
    )
  )
  # ensure discount rates are more than 0 and less than or equal to 1
  assertthat::assert_that(
    all(discounting_weights_ > 0, discounting_weights_ <= 1),
    msg = paste(
      "The object passed to the discounting_weights_ argument contains one or",
      "more negative value(s) or value(s) more than 1"
    )
  )
  # ensure inputs were of appropriate dimensions
  assertthat::assert_that(
    length(utilities_) == dim(Markov_trace_)[2],
    msg = paste(
      "The number of values in the vector passed to the utilities_ argument is",
      "unequal to the number of states in the Markov_trace_ object"
    )
  )
  assertthat::assert_that(
    length(discounting_weights_) == dim(Markov_trace_)[1],
    msg = paste(
      "The number of values in the vector passed to the discounting_weights_",
      "argument is unequal to the number of cycles in the Markov_trace_ object"
    )
  )

  ## Calculate QALYs:
  mat_mult <- function(m_TR, v_u) {
    m_TR %*% v_u
  }

  QALYs <- mat_mult(m_TR = Markov_trace_, v_u = utilities_)
  discounted_QALYs <- QALYs * discounting_weights_

  return(discounted_QALYs)
}
