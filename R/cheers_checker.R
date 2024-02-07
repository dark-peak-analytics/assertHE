#' @title Find the next element of the vector after a value
#' @description Find the next element of the vector after a value
#' @param value A value of numeric values
#' @param vector A vector of numeric values
#' @param LTE a boolean to determine collection on "greater than or equal"
#' @return The next element of the vector after the value
#' @export
#' @examples
#' \dontrun{
#' find_next_vector_element(value = 5, vector = 1:10)
#' find_next_vector_element(value = 5, vector = 1:4)
#' find_next_vector_element(value = 5, vector = 1:5, LTE = F)
#' find_next_vector_element(value = 5, vector = 1:5, LTE = T)
#' }
#'
find_next_vector_element <- function(value, vector, LTE=FALSE) {

  # Find the elements in the vector that are greater than the specified value
  if(LTE) {
    greater_than_value <- vector[vector >= value]
  } else {
    greater_than_value <- vector[vector > value]
  }

  # If there are no elements greater than the specified value, return NULL or a default value
  if (length(greater_than_value) == 0) {
    return(NA)  # or return a default value as needed
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
#' find_previous_vector_element(value = 5, vector = 1:10)
#' find_previous_vector_element(value = 5, vector = 6:10)
#' find_previous_vector_element(value = 5, vector = 5:10, LTE = F)
#' find_previous_vector_element(value = 5, vector = 5:10, LTE = T)
#' }
#'
find_previous_vector_element <- function(value, vector, LTE=FALSE){

   # Find the elements in the vector that are less than the specified value
   if(LTE) {
      less_than_value <- vector[vector <= value]
   } else {
      less_than_value <- vector[vector < value]
   }

  # If there are no elements less than the specified value,
  # return NA
   if (length(less_than_value) == 0) {
     return(NA)
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


#' @title Parses an R source file, returns function names defined within.
#'
#' @description Using utils::getParseData(), searches for function definitions
#' by matching the FUNCTION keyword (i.e. "function")
#' with it's associated SYMBOL (i.e the function name)
#'
#' @param filename A string containing a path to an R source file
#'
#' @return A dataframe with interesting information
#'
#' @importFrom utils  getParseData
#' @importFrom dplyr  lead mutate
#'
#' @export
#'
#' @example
#' \dontrun{
#' find_function_definitions(filename = "tests/testthat/example_scripts/example_tricky_functions.R")

#' }
find_function_definitions <- function(filename){

  df <- utils::getParseData(parse(filename, keep.source = TRUE), includeText = TRUE)

  # remove comments before doing anything else
  df <- df[df$token != "COMMENT",]

  # Get the records of all the function and assign keywords
  left_assign <- (df$token == "EQ_ASSIGN" | df$token == "LEFT_ASSIGN")
  fun_decs <- df$token == "FUNCTION"

  # This indicates a the current index (type SYMBOL) is followed by
  # an XXX_ASSIGN, then the FUNCTION keyword, anything else isn't a named function.

  #  So, even though df is not directly referenced within which(),
  #  the logical vectors left_assign and fun_decs, which are derived
  #  from df$token, are used to determine the positions where the
  #  conditions are met. These positions are then used to subset df.
  name_pos <- which(  dplyr::lead(left_assign, n = 2, default = FALSE)
                      & dplyr::lead(fun_decs, n = 4, default = FALSE)    )

  # only return the pd rows matching name_pos IDs
  funcs <- df[name_pos, ]
  # Add in the source file name to the result set
  funcs <- dplyr::mutate(funcs, source = filename)
  return(funcs)
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
