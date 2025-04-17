test_that(desc = "QALYs estimated correctly", {
  ## Define expected outputs:
  expected_output <- matrix(
    data = c(0.9661836, 0.8938365, 0.8430640, 0.7990180, 0.7580207),
    ncol = 1,
    dimnames = list(
      1:5,
      NULL
    )
  )

  ## Define outputs:
  Markov_trace <- matrix(
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
  output <- calculate_QALYs(
    Markov_trace_ = Markov_trace,
    utilities_ = c("u_H" = 1, "u_S1" = 0.75, "u_S2" = 0.5, "u_D" = 0),
    discounting_weights_ = c(0.9661836, 0.9335107, 0.901942, 0.871442, 0.841973)
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
