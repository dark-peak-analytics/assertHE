# Example of a script with a object used in the function but
# not defined in the arguments

noArgFunction <- function(y) {
  x <- undefined_object1 * y
  return(x)
}

# add extra examples
# noArgFunction2 <- function(y) {
#   undefined_object2 <- undefined_object2
#   x <- undefined_object2 * y
#   return(x)
# }

noArgFunction3 <- function(y) {
  x <- y * T
  return(x)
}

noArgFunction4 <- function() {
  x <- undefined_object2
  return(x)
}

noArgFunction5 <- function() {
  undefined_object3
}

noArgFunction6 <- function() {
  return(undefined_object4)
}
