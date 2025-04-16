# utility function

test_that("example code prints correctly",
          {
            output <- utility_example()

            expect_equal(object = output,
                         expected = "example_utility_function")


          })
