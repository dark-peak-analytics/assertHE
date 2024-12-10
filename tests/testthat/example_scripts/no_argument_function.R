# Example of a script with a object used in the function but
# not defined in the arguments

noArgFunction <- function(y) {
  x <- another_object * y
  return(x)
}
