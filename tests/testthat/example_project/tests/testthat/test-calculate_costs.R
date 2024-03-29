test_that(desc = "costs estimated correctly", {
  ## Define expected outputs:
  expected_output <- matrix(
    data = c(1932.367, 2193.750, 2423.437, 2624.071, 2798.137),
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
  output <- calculate_costs(
    Markov_trace_ = Markov_trace,
    costs_ = c("c_H" = 2000, "c_S1" = 4000, "c_S2" = 15000, "c_Trt" = 12000),
    discounting_weights_ = c(0.9661836, 0.9335107, 0.901942, 0.871442, 0.841973)
  )


  output2 <- calculate_costs(
    Markov_trace_ = Markov_trace,
    costs_ = c("c_H" = 3000, "c_S1" = 4000, "c_S2" = 15000, "c_Trt" = 12000),
    discounting_weights_ = c(0.9661836, 0.9335107, 0.901942, 0.871442, 0.841973)
  )

  ## Run tests:
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
