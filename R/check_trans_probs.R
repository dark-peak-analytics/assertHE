#' Check Transition Probability Matrix
#'
#' This function checks the properties of a transition probability matrix conform to
#' standard expectations. That it is: symmetric, numeric, values are between 0
#' and 1 with all rows summing to 1.
#'
#' @param m_P The transition probability matrix to be checked.
#' @param confirm_ok if OK, return a message confirming all checks passed.
#' @param stop_if_not return error messages. The default (F) returns warnings.
#'
#' @examples
#' \dontrun{
#' v_hs_names <- c("H", "S", "D")
#' n_hs <- length(v_hs_names)
#' m_P <- matrix(data = 0, nrow = n_hs, ncol = n_hs,
#'               dimnames = list(v_hs_names, v_hs_names))
#' m_P["H", "S"] <- 0.3
#' m_P["H", "D"] <- 0.01
#' m_P["S", "D"] <- 0.1
#' m_P["S", "H"] <- 0.5
#' diag(m_P) <- (1 - rowSums(m_P))
#' check_trans_prob_mat(m_P)
#' # introduce error
#' m_P["H", "S"] <- 0.2
#' check_trans_prob_mat(m_P,  confirm_ok = T, stop_if_not = T)
#' }
#'
#' @return A message indicating whether the matrix passed all the checks or a warning/error message if any check failed.
#'
#' @export
#'
#' @importFrom utils capture.output
check_trans_prob_mat <- function(m_P,
                                 confirm_ok = F,
                                 stop_if_not = F){

  # no warnings
  no_warnings <- T

  # Check that the matrix is symmetric
  if (ncol(m_P) != nrow(m_P)) {
    message <- "Transition matrix is not symmetric."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  # Check that the row and column names match in the same order
  if (no_warnings == T) {
    if (any(rownames(m_P) != colnames(m_P))) {
      message <- "Row and column names do not match."
      no_warnings <- F
      if (stop_if_not) {
        stop(message)
      } else{
        warning(message)
      }
    }

  }

  # Check that the matrix contains numeric values
  if (!is.numeric(m_P)) {
    message <- "Transition matrix is not numeric."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  # Check that matrix values are between 0 and 1
  if (!all(m_P >= 0 & m_P <= 1)) {
    message <- "Transition matrix has values below 0 or above 1."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  # Check that rows sum to 1, indicating valid transition probabilities
  if (any(abs(rowSums(m_P) - 1) > 1E-08)){
    message <- "Rows of transition matrix don't sum to 1."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  # Return a message indicating successful checks
  if(confirm_ok & no_warnings) return("Transition matrix passed all checks.")
}


# Runs checks on the external validity of a deterministic model

check_array_values <- function(a_P, stop_if_not = F){

  # Check which entries are not valid
  m_indices_notvalid <- arrayInd(which(a_P < 0 | a_P > 1),
                                 dim(a_P))

  if(dim(m_indices_notvalid)[1] != 0){
    v_rows_notval   <- rownames(a_P)[m_indices_notvalid[, 1]]
    v_cols_notval   <- colnames(a_P)[m_indices_notvalid[, 2]]
    v_cycles_notval <- dimnames(a_P)[[3]][m_indices_notvalid[, 3]]

    df_notvalid <- data.frame(`Transition probabilities not valid:` =
                                matrix(paste0(paste(v_rows_notval, v_cols_notval, sep = "->"),
                                              "; at cycle ",
                                              v_cycles_notval), ncol = 1),
                              check.names = FALSE)

    if (stop_if_not) {
      stop("Not valid transition probabilities\n",
           paste(utils::capture.output(df_notvalid),
                 collapse = "\n"))
    } else{
      warning("Not valid transition probabilities\n",
              paste(utils::capture.output(df_notvalid),
                    collapse = "\n"))
    }

  }
}


check_array_rows_balanced <- function(a_P, stop_if_not = F){

  a_P <- as.array(a_P)

  m_rowsums <- t(apply(X = a_P,
                       MARGIN = 3,
                       FUN = rowSums))

  m_indices_notvalid  <- arrayInd(which(abs(m_rowsums - 1) > 0.0001),
                                  .dim = dim(m_rowsums))

  if(dim(m_indices_notvalid)[1] != 0){
    v_cycles_notval   <- rownames(m_rowsums)[m_indices_notvalid[, 1]]
    v_cols_notval     <- colnames(m_rowsums)[m_indices_notvalid[, 2]]

    df_notvalid <- data.frame(`Transition probabilities not valid from Health States:` =
                                matrix(paste0(paste(v_cols_notval),
                                              "; at cycle ",
                                              v_cycles_notval), ncol = 1),
                              check.names = FALSE)

    if (stop_if_not) {
      stop("Not valid transition probabilities\n",
           paste(utils::capture.output(df_notvalid),
                 collapse = "\n"))
    } else{
      warning("Not valid transition probabilities\n",
              paste(utils::capture.output(df_notvalid),
                    collapse = "\n"))
    }

  }

}


check_array_names_complete <- function(a_P, stop_if_not = F){

  # no warnings
  no_warnings <- T

  n_row <- dim(a_P)[1]
  n_col <- dim(a_P)[2]

  # Check that the array is symmetric on dim 1 and 2
  if (n_row != n_col) {
    message <- paste0("Transition array is not symmetric: ", n_row, " rows, and ", n_col, " columns")
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }

  if (no_warnings == T) {
    if (any(dimnames(a_P)[[1]] != dimnames(a_P)[[2]])){
      message <- "Row and column names of the array do not match."
      no_warnings <- F
      if (stop_if_not) {
        stop(message)
      } else{
        warning(message)
      }
    }
  }

}

#' Check Transition Probability Array
#'
#' This function checks the properties of a transition probability array with
#' 2 or three dimensions conform to standard expectations. That it is that each slice is:
#' symmetric, numeric, values are between 0 and 1 with all rows summing to 1.
#'
#' @param a_P The transition probability array to be checked.
#' @param stop_if_not return error messages. The default (F) returns warnings.
#'
#' @examples
#' \dontrun{
#' v_hs_names <- c("H", "S", "D")
#' n_hs <- length(v_hs_names)
#' a_P <- array(
#'   data = 0,
#'   dim = c(n_hs, n_hs, 1000),
#'   dimnames = list(v_hs_names, v_hs_names, 1:1000)
#' )
#' a_P["H", "S",] <- 0.3
#' a_P["H", "D",] <- 0.01
#' a_P["S", "D",] <- 0.1
#' a_P["S", "H",] <- 0.5
#'
#' for(x in 1:1000){
#'   diag(a_P[,,x]) <- 1 - rowSums(a_P[,,x])
#' }
#'
#' check_trans_prob_array(a_P = a_P, stop_if_not = F)
#' # introduce error
#' a_P["H", "S", 1:10] <- 0
#'
#' check_trans_prob_array(a_P = a_P, stop_if_not = F)
#'
#' }
#'
#' @return A message indicating whether the array passed all the checks or a warning/error message if any check failed.
#'
#' @export
check_trans_prob_array <- function(a_P, stop_if_not = F){

  if(!is.numeric(a_P) | length(dim(a_P)) != 3) stop("a_P must be a numeric 3 dimensional transition probability array")

  check_array_names_complete(a_P, stop_if_not = stop_if_not)

  check_array_rows_balanced(a_P, stop_if_not = stop_if_not)

  check_array_values(a_P, stop_if_not = stop_if_not)

}
