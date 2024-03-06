test_that("Visualiser Sick Sicker ", {

  sicksicker_project_path <- testthat::test_path("example_project")

  expect_no_error({

  visualise_project(project_path = sicksicker_project_path,
                    foo_path = "R",
                    test_path = "tests/testthat",
                    run_coverage = T)

  })

})



test_that("Visualiser cx2cea model ", {

  cdx2cea_project_path <- testthat::test_path("cdx2cea")

  expect_no_error({

    visualise_project(project_path = cdx2cea_project_path,
                      foo_path = "R",
                      test_path = "tests/testthat",
                      run_coverage = F) # note coverage doesn't work on this!

  })

})


