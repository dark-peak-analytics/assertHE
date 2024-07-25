#' Check Markov Trace
#'
#' This function checks the properties of a markov trace conform to expectations.
#' That it is: numeric, values are between 0 and 1 with all rows summing to 1.
#' Also allows users to check that the dead state is monotonically decreasing (if provided)
#'
#' @param m_TR The markov trace to be checked.
#' @param stop_if_not return error messages. The default (F) returns warnings.
#' @param confirm_ok if OK, return a message confirming all checks passed.
#' @param dead_state character vector length 1 denoting dead state (e.g. "D")
#'
#' @examples
#' \dontrun{
#' v_hs_names <- c("H", "S", "D")
#' n_hs <- length(v_hs_names)
#' n_t <- 10
#'
#' m_TR <- matrix(data = NA,
#'                nrow = n_t,
#'                ncol = n_hs,
#'                dimnames = list(NULL, v_hs_names))
#'
#' m_TR[, "H"] <- seq(1, 0, length.out = n_t)
#' m_TR[, "S"] <- seq(0, 0.5, length.out = n_t)
#' m_TR[, "D"] <- 1 - m_TR[, "H"] - m_TR[, "S"]
#' check_markov_trace(m_TR = m_TR, dead_state = "D", confirm_ok = T)
#'
#' m_TR[10, "D"] <- 0
#' m_TR[9, "S"] <- 1
#' check_markov_trace(m_TR = m_TR, stop_if_not = T, dead_state = "D", confirm_ok = T)
#' }
#'
#' @return A message indicating whether the matrix passed all the checks or an error message if any check failed.
#'
#' @import assertthat
#'
#' @export
check_markov_trace <- function(m_TR,
                               dead_state = NULL,
                               confirm_ok = F,
                               stop_if_not = F){

  # Check that the trace has two dimensions
  if (length(dim(m_TR)) != 2) stop("Markov Trace is not two-dimensional")

  # check the trace is named!
  m_TR_colnames <- colnames(m_TR)
  if(any(is.na(m_TR_colnames)) | any(is.null(m_TR_colnames))) warning("m_TR is missing one or more column names")
  if(length(unique(m_TR_colnames)) != ncol(m_TR)) stop("m_TR has duplicate column names")

  # Start with no warnings
  no_warnings <- T

  # Check that the matrix contains numeric values
  if (!all(apply(m_TR, MARGIN = 2, is.numeric)))  stop("Markov trace is not numeric")

  # Check that matrix values are between 0 and 1
  if (!all(m_TR >= 0 & m_TR <= 1)) {
    message <- "Markov Trace has values below 0 or above 1"
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  # Check that rows sum to 1, indicating valid transition probabilities
  if (any(abs(rowSums(m_TR) - 1) > 1E-08)){
    message <- "Rows of Markov Trace don't sum to 1."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  # check dead state values are monotonic increasing
  if(!is.null(dead_state)){
    # Check that rows sum to 1, indicating valid transition probabilities
    if(!all(diff(x = m_TR[, dead_state]) >= 0)){
      no_warnings <- F
      message <- "Decreasing proportion in the dead state of trace, is this correct?"
      if (stop_if_not) {
        stop(message)
      } else{
        warning(message)
      }
    }
  }

  # Return a message indicating successful checks
  if(confirm_ok & no_warnings) return("Markov Trace passed all checks.")
}
