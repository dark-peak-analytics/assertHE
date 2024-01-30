#' Define a transition matrix
#'
#' @description Define the transition matrix of a State-Transition Markov (STM)
#' Model.
#'
#' @param states_nms_ A character vector containing the names of the states of a
#' Markov model.
#' @param transition_probs_ A numeric vector containing the transition
#' probabilities of length `n x n`, where `n` is the length of, number of names
#' in, the `states_nms_` vector.
#'
#' @return An `n x n`, where `n` is the number of states in an STM model,
#' containing the transition probabilities between the model states.
#'
#' @export
#'
#' @family transitions
#'
#' @examples
#' \dontrun{
#' library("sicksickerPack")
#' define_transition_matrix(
#'   states_nms_ = c("A", "B"),
#'   transition_probs_ = c(
#'     0.2, 0.8, # transitions from state A -> A and A -> B
#'     0,     1  # transitions from state B -> A and B -> B
#'   )
#'  )
#' }
define_transition_matrix <- function(states_nms_,
                                     transition_probs_) {
  ## Sanity testing - inputs:

  # confirm names vector is of class character, n = length(states_nms_)
  assertthat::assert_that(
    is.vector(x = states_nms_, mode = "character"),
    msg = paste(
      "The states_nms_ argument is not of class character"
    )
  )
  # confirm transition probabilities vector is of class numeric
  assertthat::assert_that(
    is.vector(x = transition_probs_, mode = "numeric"),
    msg = paste(
      "The transition_probs_ argument is not of class numeric"
    )
  )
  # ensure all transition probabilities are between 0 and 1
  assertthat::assert_that(
    all(transition_probs_ >= 0, transition_probs_ <= 1),
    msg = paste(
      "One or more of the values passed to the transition_probs_ argument are",
      "not between 0 and 1."
    )
  )
  # confirm transition probabilities vector is, n = sqrt(transition_probs_)
  assertthat::assert_that(
    length(states_nms_) == sqrt(length(transition_probs_)),
    msg = paste(
      "Please pass",
      length(transition_probs_),
      "probabilties for the transition between the",
      length(states_nms_),
      "markov states."
    )
  )

  ## Construct the transition probabilities' matrix:

  # fill matrix with transition probabilities
  tranistion_matrix <- matrix(
    data = transition_probs_,
    nrow = length(states_nms_),
    ncol = length(states_nms_),
    byrow = TRUE,
    dimnames = list(
      states_nms_,
      states_nms_
    )
  )

  ## Sanity testing - outputs:

  # check tranistion_matrix rows add up to 1
  assertthat::assert_that(
    all(round(rowSums(tranistion_matrix), digits = 5) == 1),
    msg = paste(
      "Transition probabilities from the",
      sub(
        pattern = ",([^,]*)$",
        replacement = " &\\1",
        paste(
          which(round(rowSums(tranistion_matrix), digits = 5) != 1) |> names(),
          collapse = ", "
        )
      ),
      "state(s) do not add up to 1."
    )
  )

  return(tranistion_matrix)
}
