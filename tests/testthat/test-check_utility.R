

    temp_dir <- "TEST_TEMP"
    sub_dir <- file.path(temp_dir, "subdir")
    
    dir.create(temp_dir)
    dir.create(sub_dir)

    # Create some R files in the temporary directory
    filename1 <- file.path(temp_dir, "test1.R")
    filename2 <- file.path(temp_dir, "test2.R")
    filename3 <- file.path(sub_dir, "test3.R")

    temp1 <- file.create(filename1)
    temp2 <- file.create(filename2)
    # create some noisy files
    file.create(file.path(temp_dir, "file.txt"))
    file.create(file.path(temp_dir, "smile.txt"))
    
    temp3 <- file.create(filename3)
    file.create(file.path(sub_dir, "while.txt"))

    # Unique objects in each file
    test1_text <- 'a_string <-  "success - A" '
    test2_text <- 'b_string <-  "success - B" '
    test3_text <- 'c_string <-  "success - C" '

    # Write the objects into the temporary files
    writeLines(test1_text, filename1)
    writeLines(test2_text, filename2)
    writeLines(test3_text, filename3)

# NOTE !!!!
    # after running these tests, the global environment is polluted !
    # If you run them again, this one - at least - will fail.
    # HINT:  rm("a_string");rm("b_string");rm("c_string");  
test_that("check that only desired sources are read into the environment", {

    source_files('.*test.*', temp_dir)
    expect_equal(a_string, "success - A" )
    expect_equal(b_string, "success - B" )
    
    # subdir/test3.R should not have been read yet
    expect_error(c_string, "object 'c_string' not found" )  
    
    source_files('.*test3.*', sub_dir)
    expect_equal(c_string, "success - C" )
})

# Check files list makes sense
test_that("returns expected list of files - exclude_files behavior test", {
    
  # Define expected result
  expected_files <- list.files(path=temp_dir, pattern = ".*", full.names = TRUE)
  
  # Filter out directories - BECAUSE - source_all filters them!
  expected_files <- expected_files[file.info(expected_files)$isdir == FALSE]
  
  # Call the function
  actual_files <- source_files(exclude_files='test1\\.R', dir_regx=temp_dir)

  # We don't expect these to be the same, because we excluded a file
  expect_lt(length(actual_files), length(expected_files))
  
  # adjust expected - remove excluded
  expected_files <- expected_files[basename(expected_files) != "test1.R"]
  
  # Check if the actual result matches the expected result
  expect_equal(actual_files, expected_files)
})



# Clean up our test files
unlink(temp_dir, recursive = TRUE)
