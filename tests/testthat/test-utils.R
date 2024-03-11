test_that("source_files works", {
  expect_silent({
    source_files(path = testthat::test_path("example_project", "R"))
  })

  source_files(path = testthat::test_path("example_project", "R"))

  # now we should have the functions in the global environment.
  expect_true(object = "calculate_costs" %in% ls(globalenv()),
              label = "calculate costs exists in environment")


})







test_that("source_files works at sourcing a file with no functions", {
  expect_silent({
    source_files(path = testthat::test_path("example_source"))
  })

  # now we should have the functions in the global environment.
  expect_true(object = script_has_been_run)

})






test_that("dirExclRegx works as intended", {

  #expect_output({
  #  source_files(path = testthat::test_path("example_project"))
  #})

  #expect_silent({
  #  source_files(path = testthat::test_path("example_project"),
  #               dirExclRegx = "tests",
  #               fileIncRegx = "\\.R$")
  #})

 # expect_equal(object = {
 #   rm(list = ls(globalenv()))
 #   source_files(path = testthat::test_path("example_project"),
 #                fileExclRegx = ".R")
 #   ls(globalenv())
 # },expected =  as.character(NULL))



})



