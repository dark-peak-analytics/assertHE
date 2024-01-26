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
find_next_vector_element <- function(value, vector){
  greater_than <- vector[vector > value]

  if(length(greater_than) == 0){
    return(NA)
  }

  next_one <- min(greater_than, na.rm = T)

  return(next_one)
}

#' @title Find the previous element of the vector before a value
#' @description Find the previous element of the vector before a value
#' @param value A value of numeric values
#' @param vector A vector of numeric values
#' @return The previous element of the vector before the value
#' @export
#' @examples
#' \dontrun{
#' find_previous_vector_element(value = 5, vector = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' }
#'
find_previous_vector_element <- function(value, vector){
  greater_than <- vector[vector < value]
  previous_one <- max(greater_than)
  return(previous_one)
}




#' @title Extract function name from a string
#'
#' @description Extract function name from a long string. This works by
#' splitting the string by "<-", "=", and " " and then keeping the word
#' before the first of these characters.
#'
#' @param string A string containing a function definition, this must contain the word 'function'
#'
#' @return A string containing the function name
#'
#' @export
#'
extract_function_name <- function(string) {
  # does the string 'function' exist in the string
  if (!grepl(pattern = "function", x = string))
    stop("No function found in string")

  # trim whitespace and replace all punctuation except "_" and "." with " ".
  tmp <- trimws(string) |>
    gsub(pattern = "\n", replacement = "") |>
    strsplit(split = c("<-", "=", " ")) |>
    gsub(pattern = "[^_.[:^punct:]]",
         replacement =  " ",
         perl = T) |>
    strsplit(split = c(" ")) |>
    unlist()

  tmp <- tmp[tmp != ""]

  function_name <- tmp[min(which(tmp == "function")) - 1]

  # check that the characters before are either "=" or "<-"
  index_function_name <-
    stringr::str_locate(pattern = function_name,
                        string = trimws(string))[[2]]

  index_operand_name <-
    stringr::str_locate_all(pattern = c("<-|="),
                            string = trimws(string))[[1]][[1]]

  if((index_operand_name - index_function_name) < 0 | (index_operand_name - index_function_name) > 100){
    return(NA)
  }

  return(function_name)

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
extract_function_name2 <- function(string){

  string <- stringr::str_replace_all(string,
                           pattern = c("\n"),
                           replacement = " ")

  foo_assign_operand_location <- stringr::str_locate_all(pattern = "(\\s|=|<-)function\\s*\\(",
                                           string = string) |>
                                  unlist() |>
                                  head(1)

  #assign_operand_locations <- stringr::str_locate_all(pattern = c("=|<-"),
  #                                                    string = string)[[1]][, "start"]

  #foo_assign_operand_location <- find_previous_vector_element(value  = foo_def_start,
  #                                                            vector = assign_operand_locations)

  v_chars <- substr(x = string,
                     start = 1,
                     stop =  foo_assign_operand_location - 1) |>
    stringr::str_replace_all(pattern = c("\n"),
                             replacement = " ") |>
    strsplit(split = " ") |>
    unlist()


    foo_name <- v_chars[which(!(v_chars %in% c("", "=", "<-")))] |>
                  utils::tail(n = 1)

    # replace any persisting assignment
    foo_name <- stringr::str_replace_all(pattern = c("=|<-"),
                             string = foo_name,
                             replacement = "")

 return(foo_name)

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
      paste(collapse = "")

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
