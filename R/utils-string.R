
#' Wrap a string to lines of a specified width
#'
#' This function takes an input string and wraps it to lines of a specified
#' width, breaking the string at word boundaries.
#'
#' @param input_string The input string to be wrapped.
#' @param width The maximum width of each line. Default is 80 characters.
#' @return A character vector where each element represents a line of the
#'   wrapped string.
#' @examples
#' input_string <- "This is a long string that needs to be wrapped to fit within
#'                 a specified width."
#' wrapped_lines <- wrap_string(input_string, width = 30)
#' cat(wrapped_lines, sep = "\n")
#'
#' @export
wrap_string <- function(input_string,
                        width = 80) {

  words <- unlist(strsplit(input_string, " "))
  lines <- ""
  current_line <- ""

  for (word in words) {
    if (nchar(current_line) + nchar(word) > width) {
      lines <- c(lines, current_line)
      current_line <- word
    } else {
      current_line <- paste(current_line, word, sep = " ")
    }
  }

  lines <- c(lines, current_line)

  return(lines)

}
