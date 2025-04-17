test_that(desc = "transition matrix defined correctly", {
  ## Define expected outputs:
  expected_output <- matrix(
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

  ## Define outputs:
  output <- define_transition_matrix(
    states_nms_ = c("H", "S1", "S2", "D"),
    transition_probs_ = c(0.845, 0.15, 0, 0.005,
                          0.5, 0.3800749, 0.105, 0.01492512,
                          0, 0, 0.9511101, 0.04888987,
                          0, 0, 0, 1)
  )

  ## Run tests:
  expect_equal(
    expected_output,
    output,
    tolerance = 0.00001
  )
  expect_error(
    define_transition_matrix(
      states_nms_ = c("H", "S1", "S2", "D"),
      transition_probs_ = c(0, -0.15, 0, 1.15,
                            0.5, 0.3800749, 0.105, 0.01492512,
                            0, 0, 0.9511101, 0.04888987,
                            0, 0, 0, 1)
    )
  )
  expect_error(
    define_transition_matrix(
      states_nms_ = c("H", "S1", "S2", "D"),
      transition_probs_ = c(0.845, 0.15, 0, 0.005,
                            0.5, 0.4, 0.105, 0.01492512,
                            0, 0, 0.9511101, 0.04888987,
                            0, 0, 0, 1)
    )
  )
})
