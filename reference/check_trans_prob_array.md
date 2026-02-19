# Check Transition Probability Array

This function checks the properties of a transition probability array
with 2 or three dimensions conform to standard expectations. That it is
that each slice is: square, numeric, values are between 0 and 1 with all
rows summing to 1. If a dead state is provided, it checks that the dead
state -\> dead state probability in each slice is equal to 1.

## Usage

``` r
check_trans_prob_array(a_P, dead_state = NULL, stop_if_not = FALSE)
```

## Arguments

- a_P:

  The transition probability array to be checked.

- dead_state:

  character vector length 1 denoting dead state (e.g. "D")

- stop_if_not:

  return error messages. The default (FALSE) returns warnings.

## Value

A message indicating whether the array passed all the checks or a
warning/error message if any check failed.

## Examples

``` r
v_hs_names <- c("H", "S", "D")
n_hs <- length(v_hs_names)
a_P <- array(
  data = 0,
  dim = c(n_hs, n_hs, 1000),
  dimnames = list(v_hs_names, v_hs_names, 1:1000)
)
a_P["H", "S",] <- 0.3
a_P["H", "D",] <- 0.01
a_P["S", "D",] <- 0.1
a_P["S", "H",] <- 0.5

for(x in 1:1000){
  diag(a_P[,,x]) <- 1 - rowSums(a_P[,,x])
}

check_trans_prob_array(a_P = a_P, stop_if_not = FALSE)
# introduce error
a_P["H", "S", 1:10] <- 0

try(check_trans_prob_array(a_P = a_P, stop_if_not = FALSE))
#> Warning: Not valid transition probabilities
#>    Transition probabilities not valid from Health States:
#> 1                                           H; at cycle 1
#> 2                                           H; at cycle 2
#> 3                                           H; at cycle 3
#> 4                                           H; at cycle 4
#> 5                                           H; at cycle 5
#> 6                                           H; at cycle 6
#> 7                                           H; at cycle 7
#> 8                                           H; at cycle 8
#> 9                                           H; at cycle 9
#> 10                                         H; at cycle 10
```
