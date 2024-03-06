test_that("test that nothing blows up with visualiser run ", {

  project_path <- testthat::test_path("example_project")

  expect_no_error({

  visualise_project(project_path = project_path,
                    foo_path = "R",
                    test_path = "tests/testthat")

  })

})
