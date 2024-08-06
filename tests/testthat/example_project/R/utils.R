# utility function

utility_example <- function(x = T) {
  if (x) {
    if(!x){
    "not run"
    }
    "example_utility_function"
  } else{
    "something_else"
  }
}

utility_example2 <- function(){

  "function without a test"
}

# Bare code - not a function
x = 15
if(x == 20) {
  x <- "impossible"
} else {
  x <- "Hello World"
}
