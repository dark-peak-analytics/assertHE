test_that(desc = "check_markov_trace shouldn't issue warnings for valid input",
          code = {
            v_hs_names <- c("H", "S", "D")
            n_hs <- length(v_hs_names)
            n_t <- 10
            m_TR <-
              matrix(
                data = NA,
                nrow = n_t,
                ncol = n_hs,
                dimnames = list(NULL, v_hs_names)
              )

            m_TR[, "H"] <- seq(1, 0, length.out = n_t)
            m_TR[, "S"] <- seq(0, 0.5, length.out = n_t)
            m_TR[, "D"] <- 1 - m_TR[, "H"] - m_TR[, "S"]


            expect_silent(check_markov_trace(m_TR))

          })




test_that(desc = "check_markov_trace shouldn't issue warnings for valid input",
          code = {
            v_hs_names <- c("H", "S", "D")
            n_hs <- length(v_hs_names)
            n_t <- 10
            m_TR <-
              matrix(
                data = NA,
                nrow = n_t,
                ncol = n_hs,
                dimnames = list(NULL, v_hs_names)
              )

            m_TR[, "H"] <- seq(1, 0, length.out = n_t)
            m_TR[, "S"] <- seq(0, 0.5, length.out = n_t)
            m_TR[, "D"] <- 1 - m_TR[, "H"] - m_TR[, "S"]
            m_TR[10, "D"] <- 0
            m_TR[9, "S"] <- 1

            expect_warning(
              check_markov_trace(m_TR = m_TR, stop_if_not = F)
            )

            expect_error(
              check_markov_trace(m_TR = m_TR, stop_if_not = T)
            )

          })
