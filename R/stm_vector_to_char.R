#' Convert vector to a character string
#'
#' \code{stm_vector_to_char()} takes a vector and converts it into a character 
#' string. Optionally, if \code{is_stop} is \code{TRUE}, it checks if the 
#' specified character is in the vector and raises an error if not.
#'
#' @param vec A vector of values to be converted to a character string.
#'            The vector is assumed to contain non-numeric values.
#' @param find_char A character to check against the vector. If \code{is_stop} 
#'                  is \code{TRUE}, the function will stop if \code{find_char} 
#'                  is not in \code{vec}.
#' @param is_stop A logical value. If \code{TRUE}, an error is raised if the 
#'                \code{find_char} is not found in \code{vec}.
#' @param par_name A string used in error and warning messages to refer to 
#'                 the parameter. Defaults to \code{"vec"}.
#'
#' @return The function returns the vector as a single concatenated character 
#'         string, or stops if \code{is_stop} is \code{TRUE} and the character 
#'         is not found in the vector.
#'
#' @details
#' The function first checks whether the input vector is valid and then 
#' concatenates it into a single string. If \code{is_stop} is \code{TRUE}, 
#' the function checks whether \code{find_char} is in the vector and stops 
#' execution if it is not.
#' 
#' @export
#' 
#' @examples
#' fruits <- c("apple", "banana", "cherry")
#' print(fruits)
#' stm_vector_to_char(fruits, find_char = "apple")
#' colors <- c("red", "blue", "green")
#' print(colors)
#' stm_vector_to_char(colors, find_char = "Blue")
#' 
stm_vector_to_char <- function(
    vec, find_char, is_stop = FALSE, par_name = 'vec') {
  vec <- check_vector(vec, is_numeric = FALSE)
  is_stop = check_logical(lgl = is_stop, default = FALSE)
  vec <- paste0(vec, collapse = "', '")
  if (is_stop) {
    find_char <- check_character(char = find_char)
    stop("Expected '", par_name, "' to be one of: '", vec, "', got --> '",
         find_char, "'")
  }
  return(vec)
}