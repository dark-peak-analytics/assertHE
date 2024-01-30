#' Create Markov Trace
#'
#' @description Create a Markov trace for a State-Transition Model (STM) Markov
#' providing a transition matrix, time horizon, states' names and the starting
#' state-occupancy.
#'
#' @param transition_matrix_ Numeric matrix containing the model's transition
#' matrix.
#' @param time_horizon_ Numeric scalar defining the number of cycles in the
#' model.
#' @param states_nms_ Character vector containing the names of the Markov model
#' states.
#' @param initial_trace_ Named numeric vector describing the states' occupancy
#' in the first cycle of the model.
#'
#' @return A matrix containing the Markov trace.
#'
#' @family simulation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("sicksickerPack")
#' transition_matrix <- matrix(
#'   data = c(0.845, 0.15, 0, 0.005,
#'            0.5, 0.3800749, 0.105, 0.01492512,
#'            0, 0, 0.9511101, 0.04888987,
#'            0, 0, 0, 1),
#'   nrow = 4,
#'   byrow = TRUE,
#'   dimnames = list(
#'     c("H", "S1", "S2", "D"),
#'     c("H", "S1", "S2", "D")
#'   )
#' )
#'
#' Markov_trace <- create_Markov_trace(
#'   transition_matrix_ = transition_matrix,
#'   time_horizon_ = 5,
#'   states_nms_ = c("H", "S1", "S2", "D"),
#'   initial_trace_ = c("H" = 1, "S1" = 0, "S2" = 0, "D" = 0)
#' )
#' }
create_Markov_trace <- function(transition_matrix_,
                                time_horizon_,
                                states_nms_,
                                initial_trace_) {
  ## Sanity testing - inputs:

  # confirm inputs are of correct type
  assertthat::assert_that(
    is.vector(x = states_nms_, mode = "character"),
    msg = paste(
      "The states_nms_ argument is not of class character"
    )
  )
  assertthat::assert_that(
    is.vector(x = initial_trace_, mode = "numeric"),
    msg = paste(
      "The initial_trace_ argument is not of class numeric"
    )
  )
  # confirm inputs are concordant
  assertthat::assert_that(
    all(length(states_nms_) == length(initial_trace_),
        length(states_nms_) == nrow(transition_matrix_),
        ncol(transition_matrix_) == nrow(transition_matrix_)),
    msg = paste(
      "The number of states in the trace or transition matrix do not match the",
      "number of named states"
    )
  )

  ## Markov trace:

  # create empty Markov trace
  Markov_trace <- matrix(
    data = NA,
    nrow = time_horizon_,
    ncol = length(states_nms_),
    dimnames = list(
      1:time_horizon_,
      states_nms_)
  )

  # initialize Markov trace
  Markov_trace[1, ] <- initial_trace_

  # loop throughout the number of cycles
  for (t in 2:time_horizon_) {

    # estimate cycle of Markov trace
    Markov_trace[t, ] <- Markov_trace[t - 1, ] %*% transition_matrix_

  }

  return(Markov_trace)
}
