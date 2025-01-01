#' Generate Hadamard matrix
#'
#' \code{stm_hadamard()} generates a Hadamard matrix of size n, where n
#' must be a power of 2. The matrix is recursively built by combining
#' smaller Hadamard matrices.
#'
#' @param n An integer specifying the size of the matrix. It must be a
#'          power of 2.
#'
#' @return A Hadamard matrix of size n x n. 
#'
#' @details
#' The function checks whether n is a valid power of 2. If n is not a power 
#' of 2, an error is raised. The function starts with a 1x1 matrix and 
#' recursively expands it by creating larger matrices by joining smaller ones.
#'
#' @export
#' 
#' @examples
#' stm_hadamard(4)
#' stm_hadamard(8)
#' stm_hadamard(16)
#'
stm_hadamard <- function(n) {
  check_numeric(num = n, min = 1, is_integer = TRUE, par_name = "n")
  
  # Check if n is a power of 2
  if (log2(n) %% 1 != 0) {
    stop("Expected 'n' to be of the form 2^e, 12*2^e, or 20*2^e, got --> ", n)
  }
  
  # Create initial 1x1 matrix
  y <- matrix(1, nrow = 1, ncol = 1)
  
  # Expand matrix until its dimension matches n
  while (dim(y)[1] < n) {
    y <- rbind(cbind(y, y), cbind(y, -y))
  }
  
  return(y)
}