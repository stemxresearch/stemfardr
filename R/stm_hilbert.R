#' Generate Hilbert matrix
#'
#' \code{stm_hilbert()} creates an \code{n x n} Hilbert matrix, where each 
#' element is defined as \code{Hij = 1 / (i + j - 1)}, where \code{i} and 
#' \code{j} are the row and column indices.
#'
#' @param n A positive integer specifying the size of the Hilbert matrix. 
#'          It represents both the number of rows and columns.
#'
#' @return An \code{n x n} matrix with elements \code{Hij = 1 / (i + j - 1)}.
#'
#' @details The Hilbert matrix is used in numerical analysis and is known for 
#' its ill-conditioning as n increases. This function uses the
#' \code{outer()} function to generate the matrix efficiently.
#' 
#' @export
#' 
#' @examples
#' stm_hilbert(3)
#' stm_hilbert(5)
#'
stm_hilbert <- function(n) {
  check_numeric(num = n, min = 1, is_integer = TRUE, par_name = "n")
  1 / (outer(1:n, 1:n, "+") - 1)
}
