
# Messy function 1: Unclear variable names, inconsistent formatting
do_something_random <- function(x, y) {
  result <- x + y * 2  # No spaces around operators
  return(result)  # Unnecessary return statement
}

# Messy function 2: Redundant parentheses, unnecessary comments
calculate_something <- function(data) {
  filtered_data <- data[data$value > 10, ]  # Redundant parentheses
  # Calculate the mean (average)
  mean_value <- mean(filtered_data$value)  # Unnecessary comment
  return(mean_value)
}

# Messy function 3: Nested loops, inconsistent indentation
find_matches <- function(list1, list2) {
  matches <- vector()
  for (item1 in list1) {
    for (item2 in list2) {
      if (item1 == item2) {
        matches <- append(matches, item1)  # Inefficient use of append
      }
    }
  }
  return(matches)
}

# # Messy function 4: Global variables, side effects - commented-out: CRAN sub
# global_var <- 0
# perform_task <- function(input) {
#   global_var <<- global_var + input  # Modifying global variable
#   return(global_var)
# }

# Messy function 5: Long lines, no comments
combine_strings <- function(str1, str2, sep = " ") {
  combined_string <- paste(str1, str2, sep = sep)  # Long line, no comment
  return(combined_string)
}

# Messy function 6: Unclear logic, repetitive code
process_data <- function(data) {
  if (some_condition) {
    # Do something
  } else if (another_condition) {
    # Do something else
  } else {
    # Do something different
  }
  # More repetitive code
}

# Messy function 7: Deeply nested function calls, hard to read
transform_data = function(data) {
  result <- apply(data, 2,  #comment
  # comment
    function(x) {
    sqrt(mean(x^2))  # Nested function call, multiple operations
  })
  return(result)
}

# Messy function 8: Unnecessary temporary variables, inefficient sorting
sort_values <- function(values) {
  temp <- sort(values)  # Unnecessary temporary variable
  sorted_values <- temp  # Inefficient sorting
  return(sorted_values)
}

# Messy function 9: Magic numbers, unclear purpose
generate_output <- function() {
  output <- list(value1 = 42, value2 = 3.14)  # Magic numbers
  return(output)
}

# Messy function 10: add comment between
do_everything <-
  # comment between equals and function call
  function(data) {
  # Combine multiple steps, hard to follow
  result <- process_data(data)
  result <- transform_data(result)
  output <- generate_output(result)
  return(output)
}

# Example Lambda (anonymous) function
# Tests that Lambdas are detected as named functions.
output <-
  (function(x, y) x * y)(3, 4)
print(output)



# Example function with lots of comments everywhere
# Messy function 11: add comment between
#' a comment
     lots_of_comments_foo <- # comment here
  # comment between equals and function call
     function( # weirdly, a commment here
    data # comment here
           ) {
    # Combine multiple steps, hard to follow
    result <- process_data(data) # comment here
    result <- transform_data(result) # comment here
    output <- generate_output(result) # comment here
    return(output) # another comment here
    # final comment
     }



# Function defined inside another
foo_outside <- function(x){
  foo_inside <- function(y){
    return(x + y)
  }
  return(foo_inside(10))
}
