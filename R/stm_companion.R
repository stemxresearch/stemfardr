#' Compute the companion matrix from a given vector
#'
#' \code{stm_companion()} computes the companion matrix from a given vector 
#' by generating the matrix form for a linear recurrence relation.
#'
#' @param vec A numeric vector. The first element represents the coefficient 
#'             of the highest degree term.
#'
#' @return A companion matrix constructed from the input vector.
#'
#' @details
#' The companion matrix is created from the vector by setting the first row 
#' to the negative of the elements (except the first) divided by the first 
#' element, and setting subdiagonal elements to 1.
#'
#' @export
#'
#' @examples
#' v <- c(2, 8, 5, 10, 18)
#' print(v)
#' stm_companion(v)
#'
stm_companion <- function(vec) {
  x <- check_vector(vec, par_name = "vec")
  n <- length(x)
  
  # Remove leading zeros from the vector
  while (n >= 2 && x[1] == 0) x <- x[2:n]
  
  if (n == 2) {
    y <- -x[2] / x[1]
  } else {
    y <- diag(0, n - 1, n - 1)
    y[1, ] <- -x[2:n] / x[1]
    for (i in 2:(n - 1)) {
      y[i, i - 1] <- 1
    }
  }
  return(y)
}

