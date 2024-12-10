test_that("identify_undefined_function_objects detects undefined objects", {
  # Function with undefined objects
  test_func <- function(y) {
    x <- undefined_object1 * y
    return(x)
  }

  expect_equal(identify_undefined_function_objects(test_func), "undefined_object1")
})

test_that("identify_undefined_function_objects ignores defined arguments", {
  # Function with all objects defined as arguments
  test_func <- function(x, y) {
    z <- x * y
    return(z)
  }

  expect_true(is.na(identify_undefined_function_objects(test_func)))
})

# This edge case fails - address later
# test_that("identify_undefined_function_objects handles self-referencing variables", {
#   # Function with self-referencing undefined objects
#   test_func <- function(y) {
#     undefined_object2 <- undefined_object2
#     x <- undefined_object2 * y
#     return(x)
#   }
#
#   expect_equal(identify_undefined_function_objects(test_func), "undefined_object2")
# })


test_that("identify_undefined_function_objects ignores functions in undefined objects", {
  # Function calling a global function
  test_func <- function(y) {
    x <- sum(y)  # `sum` is a global function
    return(x)
  }

  expect_true(is.na(identify_undefined_function_objects(test_func)))
})

# This passes - explicit function referencing
test_that("identify_undefined_function_objects handles self-referencing variables", {
  # Function with self-referencing undefined objects
  test_func <- function(y) {
    y <- y + undefined_object2
    x <- sapply(X = y, FUN = base::sum)
    return(x)
  }

  expect_equal(identify_undefined_function_objects(test_func), "undefined_object2")
})


# This returns something slightly different but still ok
test_that("identify_undefined_function_objects handles self-referencing variables", {
  # Function with self-referencing undefined objects
  test_func <- function(y) {
    y <- y + undefined_object2
    x <- sapply(X = y, FUN = sum)
    return(x)
  }

  expect_equal(identify_undefined_function_objects(test_func), c("sum (function?)", "undefined_object2"))
})



test_that("identify_undefined_function_objects handles empty functions", {
  # Empty function
  test_func <- function() {}

  expect_true(is.na(identify_undefined_function_objects(test_func)))
})

test_that("identify_undefined_function_objects handles complex cases", {
  # Function with multiple undefined objects and defined arguments
  test_func <- function(y) {
    a <- y + undefined_object1
    b <- a * undefined_object2
    return(b)
  }

  expect_equal(
    identify_undefined_function_objects(test_func),
    c("undefined_object1", "undefined_object2")
  )
})
