#' Create a constant matrix
#'
#' \code{stm_constant()} creates a matrix of a specified size where all 
#' elements are the same constant value. If no constant value is provided, 
#' one is randomly selected from the range -5 to 9.
#'
#' @param k The constant value to fill the matrix. If \code{NULL}, a random 
#'          value is selected from the range -5 to 9.
#' @param nrows The number of rows in the matrix. Must be at least 1.
#' @param ncols The number of columns in the matrix. Must be at least 1.
#' @param digits Optional. The number of decimal places to round the matrix 
#'               values. Default is \code{NULL}.
#'
#' @return A matrix filled with the constant value \code{k}.
#'
#' @details
#' This function creates a matrix of the specified size, and all elements 
#' are filled with the constant value \code{k}. If no value is specified, 
#' one is randomly selected from the range -5 to 9.
#'
#' @export
#'
#' @examples
#' stm_constant(k = 3, nrows = 4)
#' stm_constant(nrows = 3, ncols = 5)
#'
stm_constant <- function(k = NULL, nrows = 1, ncols = nrows, digits = NULL) {
  if (is.null(k)) {
    k <- sample(x = -5:9, size = 1)
  } else {
    check_numeric(k, par_name = "k")
  }
  
  check_nrows(nrows, min = 1)
  check_nrows(ncols, min = 1)

  if (any(nrows == 0, ncols == 0)) {
    return(matrix(NA, 0, 0))
  }

  y <- matrix(k, nrow = nrows, ncol = ncols)
  stm_round(y, digits = digits)
}
