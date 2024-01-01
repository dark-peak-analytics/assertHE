# tests on check-init for initialization of trace
length_greater_than_zero <- function(x) length(x) > 0

test_that(desc = "check_init shouldn't issue warnings for valid input",
          code = {

            check_init(c(.2, .3, .4, .1)) |>
              capture_warnings() |>
              expect_length(n = 0)

            check_init(c("H" = .2, "S1" = .3, "S2" = .4, "D" = .1)) |>
              capture_warnings() |>
              expect_length(n = 0)

          })

test_that(desc = "check_init should issue warnings for invalid input",
          code = {

            check_init(c(-0.1, .6, .4, .1)) |>
              capture_warnings() |>
              expect_equal(expected = "Values outside the range [0, 1]: -0.1")

            check_init(c(.1, .3, .4, .1)) |>
              capture_warnings() |>
              expect_equal(expected = "Sum of values is not equal to 1. It is: 0.9")

            check_init(setNames(c(.2, .3, .4, .1),
                                nm = c("H", "S1", "S1", "D"))) |>
              capture_warnings() |>
              expect_equal(expected = "Duplicate names detected: S1")

            check_init(setNames(c(.2, .3, .4, .1),
                                nm = c("H", "S1", "S2", NA))) |>
              capture_warnings() |>
              length_greater_than_zero() |>
              expect_true()

            check_init(setNames(c(.2, .3, .4, .1),
                                nm = c("H", "S1", "S1", NULL))) |>
              capture_warnings() |>
              length_greater_than_zero() |>
              expect_true()
          })
