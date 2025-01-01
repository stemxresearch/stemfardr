#' Generate Pascal's Triangle
#'
#' \code{stm_pascal()} generates Pascal's Triangle up to the \code{n}-th row
#' and returns the triangle matrix along with the coefficients in the 
#' last row.
#'
#' @param n The number of rows to generate in Pascal's Triangle. Must be a 
#'          positive integer.
#'
#' @return A list containing:
#'   \item{y}{A matrix representing Pascal's Triangle.}
#'   \item{coefs}{A vector of the coefficients in the last row of the triangle.}
#'
#' @export
#'
#' @examples
#' stm_pascal(3)
#' stm_pascal(5)
#'
stm_pascal <- function(n) {
  check_numeric(num = n, min = 1, par_name = "n")
  
  if (n == 1) {
    y <- coefs <- 1
  } else {
    y <- stm_ones(nrows = n, ncols = n)
    for (i in 2:n) {
      for (j in 2:n) {
        y[i, j] <- y[i - 1, j] + y[i, j - 1]
      }
    }
    coefs <- y[n, ]
    coefs <- c(coefs, rev(coefs[seq_len(length(coefs) - 1)]))
  }
  
  list(y = y, coefs = coefs)
}
