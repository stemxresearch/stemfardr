#' Compute the characteristic polynomial of a matrix
#'
#' \code{stm_charpoly()} calculates the characteristic polynomial of a square 
#' matrix and optionally returns the polynomial as an equation.
#'
#' @param x A square matrix for which the characteristic polynomial will be 
#'          computed.
#' @param equation Logical. If \code{TRUE}, the polynomial is returned as a 
#'                 string equation. Default is \code{FALSE}.
#' @param variable A string specifying the variable to be used in the polynomial. 
#'                 Default is \code{"x"}.
#' @param digits Optional. An integer specifying the number of decimal places 
#'               to round the result. Default is \code{NULL}, which means no 
#'               rounding.
#'
#' @return A numeric vector of polynomial coefficients or a string representation 
#'         of the polynomial if \code{equation = TRUE}.
#'
#' @details
#' The function calculates the characteristic polynomial of a square matrix 
#' using \code{pracma::charpoly()}. If \code{equation = TRUE}, the coefficients 
#' are formatted as a polynomial equation.
#'
#' @seealso \code{\link[pracma]{charpoly}}
#' 
#' @export
#'
#' @examples
#' A <- matrix(c(4, 1, 2, 3), nrow = 2)
#' stm_charpoly(A)
#' stm_charpoly(A, equation = TRUE)
#' stm_charpoly(A, equation = TRUE, variable = "t")
#'
stm_charpoly <- function(x, equation = FALSE, variable = "x", digits = NULL) {
  check_matrix(mat = x, is_square = TRUE, par_name = "x")
  y <- pracma::charpoly(x, info = FALSE)
  y <- stm_round(y, digits = digits)
  if (equation) {
    ystr <- paste0(y, " * ", paste0(variable, "^", rev(seq_len(length(y))))) 
    y <- gsub("+ -", "- ", paste0(ystr, collapse = " + "), fixed = TRUE)
  }
  return(y)
}