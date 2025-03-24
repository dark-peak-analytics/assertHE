# Create an example PSA results dataset.
n_psa <- 1000

# Effects (e.g. QALYs)
m_eff <-
  data.frame(
    A = runif(n = n_psa, min = 20, max = 30),
    B = runif(n = n_psa, min = 15, max = 20),
    C = runif(n = n_psa, min = 10, max = 15),
    D = runif(n = n_psa, min = 16, max = 21),
    E = runif(n = n_psa, min = 11, max = 25)
  ) |> as.matrix()

# Costs
m_cost <-
  data.frame(
    A = runif(n = n_psa, min = 1E+5, max = 3E+5),
    B = runif(n = n_psa, min = 15E+4, max = 2E+5),
    C = runif(n = n_psa, min = 1E+5, max = 15E+4),
    D = runif(n = n_psa, min = 16E+4, max = 21E+4),
    E = runif(n = n_psa, min = 11E+4, max = 23E+4)
  ) |> as.matrix()

# labels for each strategy
v_strategy_labels <-
  setNames(object = paste0("Strategy ", colnames(m_eff)),
           nm = colnames(m_eff))
# colors for each strategy
v_strategy_colors <-
  setNames(object = grDevices::palette.colors(n = ncol(m_eff)),
           nm = colnames(m_eff))

#===========#
# run tests #
#===========#

test_that("plot_PSA_stability handles valid input", {


  # don't expect an error here:
  expect_no_error({
    plot_PSA_stability(
      m_eff = m_eff,
      m_cost = m_cost,
      lambda = 20000,
      currency_symbol = "\u0024",
      v_strategy_labels = v_strategy_labels,
      v_strategy_colors = v_strategy_colors,
      comparator  = colnames(m_eff)[1],
      output = "icer",
      include_reference_line = TRUE,
      log_x = FALSE
    )
  })

  # or here with defaults

  expect_no_error({
    plot_PSA_stability(
      m_eff = m_eff,
      m_cost = m_cost,
      lambda = 20000
    )
  })

  # or here for different outputs
  for (output in c("inmb", "icer", "costs", "effects")) {
    expect_no_error(
      plot_PSA_stability(
        m_eff = m_eff,
        m_cost = m_cost,
        lambda = 20000,
        currency_symbol = "\u0024",
        v_strategy_labels = v_strategy_labels,
        v_strategy_colors = v_strategy_colors,
        comparator  = colnames(m_eff)[1],
        output = output,
        include_reference_line = TRUE,
        log_x = TRUE
      )
    )
  }

  # for different combinations of columns check the function runs
  for (x in 2:3) {

    columns_chosen <- sample(x = 3:5,
                             size = x,
                             replace = FALSE)

    expect_no_error(
      plot_PSA_stability(
        m_eff = m_eff[, columns_chosen],
        m_cost = m_cost[, columns_chosen],
        lambda = 20000,
        currency_symbol = "\u0024",
        v_strategy_labels = v_strategy_labels,
        v_strategy_colors = v_strategy_colors,
        comparator  = colnames(m_eff)[columns_chosen[1]],
        output = "icer",
        include_reference_line = TRUE,
        log_x = TRUE
      )
    )
  }

})



test_that("plot_PSA_stability flags errors", {
  # expect an error here as columns don't match
  expect_error({
    plot_PSA_stability(
      m_eff = m_eff[, 1:5],
      m_cost = m_cost[, 1:4],
      lambda = 20000,
      currency_symbol = "\u0024",
      v_strategy_labels = v_strategy_labels,
      v_strategy_colors = v_strategy_colors,
      comparator  = colnames(m_eff)[1],
      output = "icer",
      include_reference_line = TRUE,
      log_x = FALSE
    )
  })

  # and here with defaults

  expect_error({
    plot_PSA_stability(
      m_eff = m_eff[, 1:2],
      m_cost = m_cost[, 1:3],
      lambda = 20000
    )
  })

  # and here where wrong input given
  expect_error(
    plot_PSA_stability(
      m_eff = m_eff,
      m_cost = m_cost,
      lambda = 20000,
      currency_symbol = "\u0024",
      v_strategy_labels = v_strategy_labels,
      v_strategy_colors = v_strategy_colors,
      comparator  = colnames(m_eff)[1],
      output = "abc",
      include_reference_line = TRUE,
      log_x = TRUE
    )
  )

  # and where comparator doesn't exist
  expect_error(
    plot_PSA_stability(
      m_eff =  m_eff,
      m_cost =  m_cost,
      lambda = 20000,
      currency_symbol = "\u0024",
      v_strategy_labels = v_strategy_labels,
      v_strategy_colors = v_strategy_colors,
      comparator  = "Z",
      output = "icer",
      include_reference_line = TRUE,
      log_x = TRUE
    )
  )

})
