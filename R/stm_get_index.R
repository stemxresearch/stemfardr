#' Get index of a name in a vector, row, or column
#'
#' \code{stm_get_index()} searches for a specified name in a given vector, 
#' row, or column of a data structure (vector, matrix, or data frame), and 
#' returns its index. It supports stopping the search if the name is not found.
#'
#' @param dat A vector, matrix, or data frame in which to search for the 
#'             specified name. It must be iterable and contain character 
#'             elements.
#' @param name_to_find A string representing the name to search for in 
#'                     \code{data}.
#' @param is_columns Optional. A logical value indicating whether to search 
#'                    in columns (\code{TRUE}) or rows (\code{FALSE}) of a 
#'                    matrix or data frame. Defaults to \code{TRUE}.
#' @param is_stop Optional. A logical value indicating whether to stop the 
#'                function and raise an error if the name is not found. 
#'                Defaults to \code{FALSE}.
#'
#' @return An integer index if the name is found, otherwise \code{NULL} if 
#'         \code{is_stop} is \code{FALSE}, or an error if \code{is_stop} is 
#'         \code{TRUE}.
#'
#' @details
#' The function checks if the provided \code{data} is a vector, matrix, or 
#' data frame, and performs a search for \code{name_to_find} in the specified 
#' row or column. If the name is not found and \code{is_stop} is \code{TRUE}, 
#' the function stops and raises an error with a message containing the 
#' search target and data structure. If \code{is_stop} is \code{FALSE}, the 
#' function returns \code{NULL}.
#'
#' @seealso \code{\link{stm_vector_to_char}}
#' 
#' @export
#' 
#' @examples
#' v <- c("A", "B", "C")
#' print(v)
#' stm_get_index(v, name_to_find = "B")
#'
#' df <- data.frame(A = 1:3, B = 4:6, C = 7:9)
#' print(df)
#' stm_get_index(df, name_to_find = "C")
#'
#' A <- matrix(1:6, nrow = 2, byrow = TRUE,
#'   dimnames = list(c("X", "Y"), c("A", "B", "C"))
#' )
#' print(A)
#' stm_get_index(A, name_to_find = "A")
#'
#' stm_get_index(A, name_to_find = "Y", is_columns = FALSE)
#' 
#' stm_get_index(A, name_to_find = "Z", is_columns = FALSE)
#' 
stm_get_index <- function(
    dat, name_to_find, is_columns = TRUE, is_stop = FALSE
) {
  check_iterable(dat, includes_character = TRUE, par_name = "dat")
  check_character(char = name_to_find)
  is_columns <- check_logical(lgl = is_columns, default = TRUE)
  is_stop <- check_logical(lgl = is_stop, default = FALSE)
  
  if (is.vector(dat)) {
    data_structure <- "Vector"
  } else if (is.data.frame(dat)) {
    data_structure <- "DataFrame"
  } else {
    data_structure <- "Matrix"
  }
  
  row_or_col_name <- ifelse(is_columns, "column", "row")
  
  if (data_structure %in% c("DataFrame", "Matrix")) {
    df_labels <- if (is_columns) colnames(dat) else rownames(dat)
    if (is.null(df_labels)) {
      stop(data_structure, " 'dat' does not have ", row_or_col_name, " names")
    }
    vector_items <- df_labels
  } else { # this is a vector
    vector_items <- dat
  }
  
  if (name_to_find %in% vector_items) {
    index <- which(name_to_find == vector_items)
  } else {
    if (is_stop) {
      stm_vector_to_char(
        vec = vector_items, find_char = name_to_find, is_stop = TRUE
      )
    } else {
      index <- NULL
    }
  }
  return(index)
}
