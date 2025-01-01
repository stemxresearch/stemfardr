#' Create Hankel matrix
#'
#' \code{stm_hankel()} generates a Hankel matrix from two vectors, where
#' the first vector forms the first column and the second vector forms the
#' first row of the matrix. If the second vector is not provided, the last
#' element of the first vector is used to initialize the first element of
#' the second vector.
#'
#' @param vec1 A numeric vector to form the first column of the Hankel matrix.
#' @param vec2 Optional. A numeric vector to form the first row of the Hankel
#'              matrix. If not provided, the last element of \code{vec1} is 
#'              used to initialize the first element of \code{vec2}.
#'
#' @return A Hankel matrix of size \code{m x n}, where \code{m} is the length 
#'         of \code{vec1} and \code{n} is the length of \code{vec2}.
#'
#' @details
#' The function constructs a Hankel matrix where the first column is formed 
#' from \code{vec1} and the first row is formed from \code{vec2}. If \code{vec2} 
#' is not provided, the last element of \code{vec1} is used as the first element 
#' of \code{vec2}. The matrix is populated such that each element in the 
#' matrix is determined by the sum of its row and column index.
#' 
#' @export
#' 
#' @examples
#' x <- c(1, 2, 3, 5)
#' y <- c(5, 6, 7)
#' stm_hankel(x, y)
#'
stm_hankel <- function(vec1, vec2 = NULL) {
  x <- check_vector(vec = vec1, allow_scalar= TRUE, par_name = "vec1")
  m <- length(x)
  if (is.null(vec2)) {
    y <- c(x[m], rep(0, m - 1))
  } else {
    y <- check_vector(vec = vec2, allow_scalar= TRUE, par_name = "vec2")
  }
  n <- length(y)
  if (x[m] != y[1]) {
    warning("Last element of 'vec1' not equal to first element of 'vec2', ",
            "set vec2[1] = vec1[m]")
  }
  h <- stm_zeros(nrows = m, ncols = n)
  for (i in 2:(m + n)) {
    indices <- row(h) + col(h) == i
    values <- ifelse(i <= m + 1, x[i - 1], y[i - m])
    h[indices] <- values
  }
  return(h)
}
