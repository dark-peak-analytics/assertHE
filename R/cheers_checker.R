#' @title Find the next element of the vector after a value
#' @description Find the next element of the vector after a value
#' @param value A value of numeric values
#' @param vector A vector of numeric values
#' @return The next element of the vector after the value
#' @export
#' @examples
#' \dontrun{
#' find_next_vector_element(value = 5, vector = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' }
#'
find_next_vector_element <- function(value, vector) {
  # Find the elements in the vector that are greater than the specified value
  greater_than_value <- vector[vector > value]

  # If there are no elements greater than the specified value, return NULL or a default value
  if (length(greater_than_value) == 0) {
    return(max(vector))  # or return a default value as needed
  }

  # Find the minimum value among the elements greater than the specified value
  next_element <- min(greater_than_value)

  return(next_element)
}


#' @title Find the previous element of the vector before a value
#' @description Find the previous element of the vector before a value
#' @param value A value of numeric values
#' @param vector A vector of numeric values
#' @param LTE a boolean to determine collection on "less than" or "less than equal"
#' @return The previous element of the vector before the value
#' @export
#' @examples
#' \dontrun{
#' find_previous_vector_element(value = 5, vector = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' }
#'
find_previous_vector_element <- function(value, vector, LTE=FALSE){
   
   # Find the elements in the vector that are less than the specified value
   if(LTE==TRUE) {
      less_than_value <- vector[vector <= value]
   } else {
      less_than_value <- vector[vector < value]
   }

  # If there are no elements less than the specified value, 
  # return the minimum value found in the vector
   if (length(less_than_value) == 0) {
     return (min(vector))
   }

  # Find the maximum value among the elements less than the specified value
   previous_element <- max(less_than_value)

   return(previous_element)
 }



#' @title Extract function name from a string
#'
#' @description Extract function name from a long string. This works by
#' identifying "function(" in the string and then finding the operand before and splitting
#' on that before keeping the character there.
#'
#' @param string A string containing a function definition, this must contain the word 'function'
#'
#' @return A string containing the function name
#' @importFrom stringr str_locate_all str_replace_all
#'
#' @export
#'
#'
extract_function_name <- function(string){

   # regex pattern to match comments (note: greedy match with '?')
  # assumes comments won't appear in quoted strings  (i.e. print("this # will match") )
  pattern <- "#.*?\\n"

  # Replace the comment (to end of line) with an empty string
  string <- gsub(pattern, "", string, perl = TRUE)

  # Convert newlines to spaces (remove newlines)
  string <- stringr::str_replace_all(string, pattern = c("\n"), replacement = " ")

  assign_op <- stringr::str_locate_all(string, pattern = "(=|<-)\\s*function\\s*\\(")
  assign_op <- unlist(x = assign_op)
  assign_op <- assign_op[1]

  string <- substr(string, 1, assign_op-1)
  string <- gsub("\\s*", "", string, perl = TRUE)

  return(string)

}


#' @title Helper function for find_function_definitions
#'
#' @description Do not try to call this - it is only a help 
#' and is only to keep find_function_definitions clean
#' These two functions could really be combined - a task for another day
#'
filter_func_lnums <- function(v_func_lnums, v_assign_lnums, v_symbol_lnums, parsed_data) {

  # Create an empty vector to store filtered v_func_lnums
  filtered_v_func_lnums <- c()

  # Iterate over each v_func_lnum
  for (func_lnum in v_func_lnums) {

    # Find the line number of the nearest v_assign_lnum before the v_func_lnum
    assign_lnum <- max(v_assign_lnums[v_assign_lnums <= func_lnum])

    # Find the line number of the nearest v_symbol_lnum before the v_func_lnum
    symbol_lnum <- max(v_symbol_lnums[v_symbol_lnums <= func_lnum])

    # Check if both assign_lnum and symbol_lnum exist 
    # and that the assign doesn't come before the symbol.
    #
    # Ignoring uninteresting parse data like "expr", "NUM_CONST", etc
    # R parse data for a function definition can only exist in this order:
    # SYMBOL, ASSIGN, FUNCTION
    if ( is.na(assign_lnum) || is.na(symbol_lnum)  || assign_lnum < symbol_lnum) {
      next
    }

    # if assign_lnum == symbol_lnum, 
    # Then we might have a single line like this:
    #
    # foo <- function(A){}   # i.e. a function definition
    #
    # or we may have the use of a Lambda (anonymous) function like this:
    #
    # result <- apply(data, 2, function(x) { sqrt(mean(x^2)) })
    #
    # This is NOT a funtion definition (well, it is, but, not one we care about) !
    # in this case, there will (hopefully) always be a SYMBOL between 
    # FUNCTION and XXX_ASSIGN.
    #
    # check that the col1 value for assign is greater than for symbol
    if(assign_lnum == symbol_lnum){

      assign_col1 <-  max(  parsed_data[ parsed_data$line1 == assign_lnum 
                                        & (parsed_data$token == "LEFT_ASSIGN" 
                                            | parsed_data$token == "EQ_ASSIGN") , 
                                        "col1"] )

      symbol_col1 <-  max(  parsed_data[ parsed_data$line1 == symbol_lnum 
                                         & parsed_data$token == "SYMBOL" ,
                                        "col1"] )

      if(symbol_col1 >= assign_col1){
        # These aren't the droids you're looking for.
        next
      }
    }

    # If we get this far, we're pretty confident we have a function definition.
    # Add the v_func_lnum to the filtered vector
    filtered_v_func_lnums <- c(filtered_v_func_lnums, func_lnum)

  }

  # Return the filtered vector
  return(filtered_v_func_lnums)
}

#' @title Parses an R source file, returns function names defined within.
#'
#' @description Using utils::getParseData(), searches for function definitions
#' by matching the FUNCTION keyword (i.e. "function") 
#' with it's associated SYMBOL (i.e the function name)
#'
#' @param filename A string containing a path to an R source file
#'
#' @return A string containing the function names
#'
#' @importFrom stringr str_locate_all str_replace_all
#'
#' @export
#'
find_function_definitions <- function(filename) {

  # Parse the R code
  parsed_data <-
    utils::getParseData(parse(filename, keep.source = TRUE))

  # get the line numbers containing the function keyword 
  # identified by token value "FUNCTION"
  v_func_lnums <-  parsed_data[parsed_data$token == "FUNCTION", "line1"]

# find line numbers which are identified as assignments 
# named functions get assigned names - Lambdas (anonymous funcitons) don't !
  v_assign_lnums <- parsed_data[parsed_data$token == "LEFT_ASSIGN" | parsed_data$token == "EQ_ASSIGN", "line1"]

  # find line numbers which are identified as symbol definitions
  # Function names are identiffied as SYMBOLs
  v_symbol_lnums <- parsed_data[parsed_data$token == "SYMBOL", "line1"]

  # Try to filter the use of the keyword "function" when it is used
  # as a Lambda (anonymous) function.
  v_func_lnums <- filter_func_lnums(v_func_lnums, v_assign_lnums, v_symbol_lnums, parsed_data)
  
  # for each function location find the immediately preceeding symbols location
  v_symbols_preceding_functions <- sapply(X = v_func_lnums,
                                          FUN = find_previous_vector_element, 
                                          vector=v_symbol_lnums, LTE=TRUE)
  
  # Extract the symbol names (stored in the "text" element)
  # from the data.frame located at the line numbers that identify symbols
  function_symbols <- parsed_data[  parsed_data$line1 %in% v_symbols_preceding_functions 
                                  & parsed_data$token == "SYMBOL", "text"]
  return(function_symbols)

}

#' @title Get cheers classification tags from a given file
#' @description For a provided filepath, identify the cheers classification tags
#' and the function names that follow them.
#' @param filename A string containing the filepath to the file to be checked
#' @param cheers_pattern A string containing the roxygen tag for cheers which is used as an identifier
#' @param function_pattern A string containing the pattern to identify functions
#' @return A list containing the cheers tags and the function names that follow them
#' @family cheers
#' @importFrom stringr str_replace str_replace_all
#'
#' @export
#'
get_file_cheers_classifications <- function(filename,
                                            cheers_pattern,
                                            function_pattern = "(\\s|=|-)function\\("){
lines <- NULL
# check files exists
#if(file.exists(filename))
lines <- readLines(filename) #else stop("Not a file")

# find the rows on which the cheers pattern occurs
cheers_indices <-
  sapply(X = lines,
         FUN = function(line) grepl(pattern = cheers_pattern, x =  line)
  ) |> which()

# remove the cheers pattern from that row name
cheers_indices <- stats::setNames(
  object = cheers_indices,
  nm = stringr::str_replace(
    string = stringr::str_replace_all(
      string = names(cheers_indices),
      pattern = " ",
      replacement = ""
    ),
    pattern = paste0("#'", cheers_pattern),
    replacement = ""
  )
)

# find the row number for each function
function_indices <-
  sapply(X = lines,
         FUN = function(line) grepl(pattern = function_pattern, x =  line)
  ) |> which()


# for each cheers row, find the next function row.
next_function_index <- sapply(X = cheers_indices,
                              FUN = find_next_vector_element,
                              vector = function_indices)

if(length(as.vector(stats::na.omit(next_function_index))) == 0) return(NA)

# for each function identified after a cheers tag, extract the function name
v_function_names <- sapply(
  X = next_function_index,
  FUN = function(function_index) {

    function_index_minus_one <- max(c(function_index - 1, 1))

    function_line <-
      lines[function_index_minus_one:function_index] |>
      paste(collapse = "\n")

    return(extract_function_name(function_line))

  }
)

if(length(v_function_names) == 0) return(NA)

# return the function names as a vector, named with the cheers tag.
return(v_function_names)

}


#' @title Get cheers classification tags from a given folder
#'
#' @description For a provided folder path, identify the cheers classification tags
#' and the function names that follow them.
#'
#' @param path A string containing the filepath to the folder to be checked
#' @param cheers_pattern A string containing the roxygen tag for cheers which is used as an identifier
#' @param path_ignore A string containing the pattern to identify files to ignore
#'
#' @return A list containing the cheers tags and the function names that follow them
#'
#' @family cheers
#'
#' @export
#'
get_folder_cheers_classifications <- function(path,
                                              cheers_pattern,
                                              path_ignore = "tests/"){

my_R_scripts <- list.files(
  path = path,
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

my_R_scripts <- my_R_scripts[!grepl(x = my_R_scripts, pattern = path_ignore) ]

tmp <- lapply(X = my_R_scripts,
              FUN = get_file_cheers_classifications,
              cheers_pattern = cheers_pattern)

names(tmp) <- my_R_scripts

df_foos <- do.call(rbind.data.frame,
                   lapply(
                     tmp,
                     FUN = function(x) {
                       if (all(!is.na(x)))
                         utils::stack(x, drop = T)
                       else
                         NULL
                     }
                   ))

df_foos$script <- rownames(df_foos)
rownames(df_foos) <- NULL

if(ncol(df_foos) == 3){
colnames(df_foos) <- c("function_name", "tag", "script")
}

return(df_foos)

}




#' @title get all active functions that exist in the global environment
#'
#' @description get all active functions that exist in the global environment
#'
#' @param packages a vector containing the names of packages to include in the search
#'
#' @return a vector containing the names of all active functions in the global environment
#'
#' @export
get_active_functions <- function(packages = "assertHE") {
  # set environment to global
  v_global_objects <- ls(envir = .GlobalEnv)

  v_global <- v_global_objects[sapply(v_global_objects,
                                      function(x)
                                        is.function(x = get(x)))]

  v_packages_include <- packages[!packages %in% c(".GlobalEnv", "NA", NA)]

  v_packages <- v_packages_include |>
    lapply(
      FUN = function(x) {
        ls(eval(paste0("package:", x)))[
          sapply(ls(eval(paste0("package:", x))),
            FUN = function(x) {
              is.function(x = get(x))
            }
          )]
      }
    ) |>
    unlist()  |>
    unique()

  return(c(v_global, v_packages))

}
