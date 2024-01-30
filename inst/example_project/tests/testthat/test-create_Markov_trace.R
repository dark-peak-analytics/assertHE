test_that(desc = "Markov trace defined correctly", {
  ## Define expected outputs:
  expected_output <- matrix(
    data = c(1, 0, 0, 0,
             0.845, 0.15, 0, 0.005,
             0.789025, 0.1837612, 0.01575, 0.01146377,
             0.7586067, 0.1881968, 0.03427491, 0.01892157,
             0.7351211, 0.1853199, 0.05235988, 0.02719916),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(
      1:5,
      c("H", "S1", "S2", "D")
    )
  )

  ## Define outputs:
  transition_matrix <- matrix(
    data = c(0.845, 0.15, 0, 0.005,
             0.5, 0.3800749, 0.105, 0.01492512,
             0, 0, 0.9511101, 0.04888987,
             0, 0, 0, 1),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      c("H", "S1", "S2", "D"),
      c("H", "S1", "S2", "D")
    )
  )
  output <- create_Markov_trace(
    transition_matrix_ = transition_matrix,
    time_horizon_ = 5,
    states_nms_ = c("H", "S1", "S2", "D"),
    initial_trace_ = c("H" = 1, "S1" = 0, "S2" = 0, "D" = 0)
  )

  ## Run test:
  expect_equal(
    expected_output,
    output,
    tolerance = 0.00001
  )
  expect_type(
    object = output,
    type = "double"
  )
})
