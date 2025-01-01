#' Diagonal matrix or vector with offset
#'
#' \code{stm_diag()} extracts the diagonal elements of a matrix or vector,
#' allowing for an optional offset, and returns the result as a matrix or
#' vector.
#'
#' @param vmat A vector or matrix from which the diagonal is extracted.
#'             If a matrix, the function extracts the diagonal elements, 
#'             with an optional offset \code{k}.
#' @param k Optional. An integer specifying the diagonal offset. The default 
#'          is \code{0} for the main diagonal. A positive value retrieves 
#'          diagonals above the main diagonal, while a negative value 
#'          retrieves diagonals below the main diagonal.
#'
#' @return A vector or matrix with the extracted diagonal elements, or an 
#'         empty numeric vector if no elements are present.
#'
#' @details
#' The function extracts the diagonal elements of the input \code{vmat} with 
#' an optional offset. The offset \code{k} shifts the diagonal. If \code{k} 
#' is positive, it retrieves diagonals above the main diagonal; if negative, 
#' it retrieves diagonals below the main diagonal. If \code{vmat} is a matrix, 
#' the output is a matrix; if it is a vector, the result is a vector.
#'
#' @seealso \code{\link{stm_eye}}
#'
#' @export
#'
#' @examples
#' A <- matrix(1:12, nrow = 3, byrow = TRUE)
#' print(A)
#' stm_diag(A)
#' stm_diag(vmat = c(1, 2, 3))
#' stm_diag(vmat = c(1, 2, 3), k = 2)
#' stm_diag(vmat = c(1, 2, 3), k = -1)
#'
stm_diag <- function(vmat, k = 0) {
  x <- check_vector_or_matrix(vmat = vmat, par_name = "vmat")
  k <- check_numeric(num = k, is_integer = TRUE, par_name = "k")
  
  if (is.matrix(x)) {
    y <- x[col(x) == row(x) + k]
  } else { # it's a vector
    y <- stm_zeros(nrows = length(x) + abs(k))
    y[col(y) == row(y) + k] <- x
  }
  
  if (length(y) == 0) numeric() else y
}