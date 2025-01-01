#' Generate a Vandermonde matrix from a vector
#'
#' \code{stm_vander()} creates a Vandermonde matrix from a given vector. It 
#' raises each element of the vector to the power of integers from 0 to 
#' \code{n-1}, where \code{n} is the length of the vector. The user can 
#' specify the number of columns in the matrix and whether the matrix should 
#' be in increasing order.
#'
#' @param vec A numeric vector for which the Vandermonde matrix will be 
#'             created.
#' @param ncols Optional. An integer specifying the number of columns in the 
#'              matrix. Default is \code{NULL}, which keeps all columns.
#' @param is_increasing A logical value indicating whether the matrix should 
#'                      be in increasing order. Default is \code{FALSE}.
#'
#' @return A matrix representing the Vandermonde matrix, either in the default 
#'         order or reversed if \code{is_increasing = TRUE}.
#'
#' @details
#' The function generates a Vandermonde matrix using the outer product, where 
#' the elements of \code{vec} are raised to successive powers from 0 to 
#' \code{n-1}. The user can specify the number of columns using the
#' \code{ncols} parameter. The matrix can also be returned in increasing order
#' if \code{is_increasing} is set to \code{TRUE}.
#' 
#' @export
#' 
#' @examples
#' v <- c(1, 2, 3, 4)
#' print(v)
#' stm_vander(v)
#' stm_vander(v, is_increasing = TRUE)
#'
stm_vander <- function(vec, ncols = NULL, is_increasing = FALSE) {
  vec <- check_vector(vec, allow_scalar= TRUE, par_name = "vec")
  n <- length(vec)
  if (n == 0) {
    y <- numeric()
  } else {
    y <- outer(X = vec, Y = seq(n - 1, 0), FUN = "^")
    ycols = ncol(y)
    if (!is.null(ncols)) {
      check_numeric(
        num = ncols, min = 1, max = ycols, is_integer = TRUE, par_name = "ncols"
      )
      ncols_last <- (ycols - ncols + 1):ycols
      y <- y[, ncols_last]
    }
  }
  if (is_increasing) stm_fliplr(y) else y
}
