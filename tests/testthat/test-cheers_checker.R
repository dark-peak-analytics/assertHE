test_that("Extracting function names works as intended",
          {

  example1 <- " .function.name. <-

  function
  (a, b, c){


  }"

  example2 <- "function_name <- function(a, b, c){}"

  example3 <-"__function.name__ =



                      function(a, b, c){}"

  example4 <- "extract_function_name = function(string) {
  # does the string 'function' exist in the string
"

  example5 <- "function_name <-function(a){}"
  example6 <- "function_name =function(a){}"
  example7 <- "function_name<-function(a){}"
  example8 <- "function_name=function(a){}"

  
  expect_equal(extract_function_name(example1), ".function.name.")
  expect_equal(extract_function_name(example2), "function_name")
  expect_equal(extract_function_name(example3), "__function.name__")
  expect_equal(extract_function_name(example4), "extract_function_name")
  
  expect_equal(extract_function_name(example5), "function_name")
  expect_equal(extract_function_name(example6), "function_name")
  expect_equal(extract_function_name(example7), "function_name")
  expect_equal(extract_function_name(example8), "function_name")

  # run through all four examples.
  expect_equal(extract_function_name2(example1), ".function.name.")
  expect_equal(extract_function_name2(example2), "function_name")
  expect_equal(extract_function_name2(example3), "__function.name__")
  expect_equal(extract_function_name2(example4), "extract_function_name")

  expect_equal(extract_function_name2(example5), "function_name")
  expect_equal(extract_function_name2(example6), "function_name")
  expect_equal(extract_function_name2(example7), "function_name")
  expect_equal(extract_function_name2(example8), "function_name")

  exampleHORRID <- " .function.name. =  #comment
    function       # more comment
       (a)   # yet more comment
          {}"
  expect_equal(extract_function_name2(exampleHORRID), ".function.name.")

  
  exampleComplicated <- "# Here are some comments for foo
    #' @family test

      foo <- 
  
    #a comment here - function indented by whitespace too !

      function  ( a ) 

  {

            a <- some_function(x = a)
            a = another_function(x = 'hello')

          }"


# expect_equal(extract_function_name2(exampleComplicated), "foo")

  simple <- "   test4  <-
  #' adding text here

        function(   x)   {y <- x/2
  return(y)

  }"

  expect_equal(extract_function_name2(simple), "test4")


    function_commented <- "#' test_my_foo <-  function(){}
      myfoo <- function(){

    }"
  expect_equal(extract_function_name2(function_commented), "myfoo")   # NOT test_my_foo
  })





test_that("Next element after integer in vector works as intended",
{
  expect_equal(find_next_vector_element(10, 1:12), 11)
  expect_equal(find_next_vector_element(4, seq(1, 120, 5)), 6)
  expect_equal(find_next_vector_element(120, seq(1, 120, 5)), 116)
})


test_that("Previous element before integer in vector works as intended",
{
  expect_equal(find_previous_vector_element(10, 1:12), 9)
  expect_equal(find_previous_vector_element(4, seq(1, 120, 5)), 1)
  expect_equal(find_previous_vector_element(1, seq(1, 120, 5)), 1)
})









# test_that("get_file_cheers_classifications works for a few example scripts",
#           {
#           if (testthat::testing_package() != ""){
#               path <- dirname(dirname(getwd()))
#             }else{
#               path <- getwd()
#             }
#
#             expect_silent({
#
#               expect_equal(
#                get_file_cheers_classifications(filename = paste0(path, "/inst/example_script/create_markov_trace.R"),
#                                                cheers_pattern = "@family"),
#                 stats::setNames(object = "create_Markov_trace", nm =  "simulation")
#               )
#
#               expect_equal(
#                 get_file_cheers_classifications(filename = paste0(path, "/inst/example_script/define_transition_matrix.R"),
#                                                 cheers_pattern = "@family"),
#                 stats::setNames(object = "define_transition_matrix", nm =  "transitions")
#               )
#
#               #expect_equal(
#               #  get_file_cheers_classifications(filename = paste0(path,"/inst/example_script/example_script.R"),
#               #                                  cheers_pattern = "@family"),
#               #  NA
#               #)
#
#             })
#           })



test_that("get_folder_cheers_classifications works for a few example folders",
          {

            if (testthat::testing_package() != ""){
              path <- dirname(dirname(getwd()))
            }else{
              path <- getwd()
            }

            expect_silent({

              get_folder_cheers_classifications(path = "./inst",
                                                cheers_pattern =  "@family")

            })
          })





