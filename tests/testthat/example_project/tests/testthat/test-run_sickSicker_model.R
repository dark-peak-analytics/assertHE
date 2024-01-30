test_that(desc = "sickSicker model simulated correctly", {
  ## Define expected outputs:
  expected_output <- c("Cost_no_treatment" = 11318.999306,
                       "Cost_treatment" = 19886.585405,
                       "QALYs_no_treatment" = 4.260124,
                       "QALYs_treatment" = 4.385285)

  ## Define outputs:
  params <- data.frame(
    age_init_ = 25,
    age_max_  = 30,
    discount_rate_ = 0.035,
    p_HD    = 0.005,
    p_HS1   = 0.15,
    p_S1H   = 0.5,
    p_S1S2  = 0.105,
    hr_S1   = 3,
    hr_S2   = 10,
    c_H     = 2000,
    c_S1    = 4000,
    c_S2    = 15000,
    c_Trt   = 12000,
    c_D     = 0,
    u_H     = 1,
    u_S1    = 0.75,
    u_S2    = 0.5,
    u_D     = 0,
    u_Trt   = 0.95
  )

  output <- run_sickSicker_model(params_ = params)

  ## Run tests:
  expect_equal(
    expected_output,
    output,
    tolerance = 0.00001
  )
  expect_vector(
    object = run_sickSicker_model(params_ = params),
    ptype = numeric(),
    size = 4
  )
})
