# Check Transition Probability Matrix

This function checks the properties of a transition probability matrix
conform to standard expectations. That it is: square, numeric, values
are between 0 and 1 with all rows summing to 1. If a dead state is
provided, it checks that the dead state -\> dead state probability is 1.

## Usage

``` r
check_trans_prob_mat(
  m_P,
  dead_state = NULL,
  confirm_ok = FALSE,
  stop_if_not = FALSE
)
```

## Arguments

- m_P:

  The transition probability matrix to be checked.

- dead_state:

  character vector length 1 denoting dead state (e.g. "D")

- confirm_ok:

  if OK, return a message confirming all checks passed.

- stop_if_not:

  return error messages. The default (FALSE) returns warnings.

## Value

A message indicating whether the matrix passed all the checks or a
warning/error message if any check failed.

## Examples

``` r
v_hs_names <- c("H", "S", "D")
n_hs <- length(v_hs_names)
m_P <- matrix(data = 0, nrow = n_hs, ncol = n_hs,
              dimnames = list(v_hs_names, v_hs_names))
m_P["H", "S"] <- 0.3
m_P["H", "D"] <- 0.01
m_P["S", "D"] <- 0.1
m_P["S", "H"] <- 0.5
diag(m_P) <- (1 - rowSums(m_P))
check_trans_prob_mat(m_P)

# introduce error
m_P["H", "S"] <- 0.2
try(check_trans_prob_mat(m_P,  confirm_ok = TRUE, stop_if_not = TRUE))
#> Error in check_trans_prob_mat(m_P, confirm_ok = TRUE, stop_if_not = TRUE) : 
#>   Rows of transition matrix don't sum to 1.
```
