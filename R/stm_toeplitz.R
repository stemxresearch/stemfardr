#' Create a Toeplitz matrix
#'
#' \code{stm_toeplitz()} generates a Toeplitz matrix from two vectors. A 
#' Toeplitz matrix is a matrix where each descending diagonal from left to 
#' right is constant.
#'
#' @param vec1 A numeric vector to form the first column of the matrix.
#' @param vec2 A numeric vector to form the first row of the matrix. If 
#'              not specified, defaults to \code{vec1}.
#'
#' @return A matrix of size \code{length(vec1)} x \code{length(vec2)} 
#'         with the structure of a Toeplitz matrix.
#'
#' @details The function checks that the two vectors have the same length. 
#'          It then constructs a matrix where the first column is \code{vec1} 
#'          and the first row is \code{vec2}. Subsequent rows are filled 
#'          following the Toeplitz matrix structure.
#' 
#' @export
#' 
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(1, 5, 6, 7)
#' stm_toeplitz(x, y)
#'
stm_toeplitz <- function(vec1, vec2 = vec1) {
  x <- check_vector(vec1, par_name = "vec1")
  y <- check_vector(vec2, par_name = "vec2")
  if (x[1] != y[1]) {
    warning("x[1] is not equal to y[1]")
  }
  m <- length(x)
  n <- length(y)
  z <- matrix(NA, nrow = m, ncol = n)
  z[1, ] <- y
  z[, 1] <- x
  for (i in 2:m) {
    z[i, 2:n] <- z[i - 1, 1:(n - 1)]
  }
  return(z)
}
