test_that(desc = "discounting weights calculated correctly", {
  ## Define expected outputs:
  expected_output1 <- 1
  expected_output2 <- c(1, 0.9661836)
  expected_output3 <- 0.9661836

  ## Define outputs:
  output1 <- calculate_discounting_weights(time_horizon = 1)
  output2 <- calculate_discounting_weights(time_horizon = 2)
  output3 <- calculate_discounting_weights(
    time_horizon = 1,
    first_cycle = TRUE
  )

  ## Run tests:
  expect_equal(expected_output1, output1, tolerance = 0.00001)
  expect_equal(expected_output2, output2, tolerance = 0.00001)
  expect_equal(expected_output3, output3, tolerance = 0.00001)
  expect_error(calculate_discounting_weights(
    discount_rate = -0.1,
    time_horizon = 1
  ))
  expect_vector(
    object = calculate_discounting_weights(time_horizon = 1),
    ptype = numeric(),
    size = 1
  )
  expect_vector(
    object = calculate_discounting_weights(time_horizon = 5),
    ptype = numeric(),
    size = 5
  )
})
