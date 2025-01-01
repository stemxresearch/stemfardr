#' Generate identity matrix
#'
#' \code{stm_identity()} creates an \code{n x n} identity matrix where all 
#' diagonal elements are 1, and all off-diagonal elements are \code{0}.
#'
#' @param n A positive integer specifying the size of the identity matrix. 
#'          It represents both the number of rows and columns.
#'
#' @return An \code{n x n} identity matrix.
#'
#' @details The identity matrix is a square matrix with ones on the diagonal 
#' and zeros elsewhere. It is used in various mathematical operations such 
#' as matrix inversion and solving systems of linear equations.
#'
#' @seealso \code{\link{diag}}, \code{\link{stm_eye}}
#' 
#' @export
#' 
#' @examples
#' stm_identity(3)
#' stm_identity(5)
#'
stm_identity <- function(n) {
  check_numeric(num = n, min = 1, is_integer = TRUE, par_name = "n")
  diag(x = 1, nrow = n, ncol = n)
}