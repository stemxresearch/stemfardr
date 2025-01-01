stm_arrays_like <- function() {
  return(invisible(NULL))
}


#' Generate a matrix or vector of zeros like an existing matrix or vector
#'
#' \code{stm_zeros_like()} generates a matrix or vector of zeros that matches
#' the dimensions of the provided input matrix or vector.
#'
#' @param vmat A matrix or vector whose dimensions will be used to generate
#'             the matrix or vector of zeros.
#'
#' @return A matrix or vector of the same dimensions as \code{vmat}, filled
#'         with zeros.
#'
#' @details
#' The function checks the validity of the input \code{vmat}, converting it
#' to a matrix if it is a vector. It then generates a matrix of zeros of the
#' same size as \code{vmat}.
#'
#' @seealso \code{\link{stm_zeros}}
#' 
#' @export
#'
#' @examples
#' A <- matrix(1:12, nrow = 3, byrow = TRUE)
#' stm_zeros_like(A)
#' stm_zeros_like(c(1, 2, 3))
#'
stm_zeros_like <- function(vmat) {
  check_vector_or_matrix(vmat = vmat, par_name = "vmat")
  vmat <- if (is.vector(vmat)) as.matrix(vmat) else vmat
  stm_zeros(nrows = nrow(vmat), ncols = ncol(vmat))
}


#' Generate a matrix or vector of ones like an existing matrix or vector
#'
#' \code{stm_ones_like()} generates a matrix or vector of ones that matches
#' the dimensions of the provided input matrix or vector.
#'
#' @param vmat A matrix or vector whose dimensions will be used to generate
#'             the matrix or vector of ones.
#'
#' @return A matrix or vector of the same dimensions as \code{vmat}, filled
#'         with ones.
#'
#' @details
#' The function checks the validity of the input \code{vmat}, converting it
#' to a matrix if it is a vector. It then generates a matrix of ones of the
#' same size as \code{vmat}.
#'
#' @seealso \code{\link{stm_ones}}
#' 
#' @export
#'
#' @examples
#' A <- matrix(1:12, nrow = 3, byrow = TRUE)
#' stm_ones_like(A)
#' stm_ones_like(c(1, 2, 3))
#'
stm_ones_like <- function(vmat) {
  check_vector_or_matrix(vmat = vmat, par_name = "vmat")
  vmat <- if (is.vector(vmat)) as.matrix(vmat) else vmat
  stm_ones(nrows = nrow(vmat), ncols = ncol(vmat))
}


#' Generate a constant matrix or vector like an existing matrix or vector
#'
#' \code{stm_constant_like()} generates a constant matrix or vector that
#' matches the dimensions of the provided input matrix or vector.
#'
#' @param vmat A matrix or vector whose dimensions will be used to generate
#'             the constant matrix or vector.
#' @param k A numeric value representing the constant to fill the generated
#'          matrix or vector. Default is \code{2}.
#'
#' @return A matrix or vector of the same dimensions as \code{vmat}, filled
#'         with the constant value \code{k}.
#'
#' @details
#' The function checks the validity of the input \code{vmat} and \code{k},
#' converting \code{vmat} to a matrix if it is a vector. It then generates
#' a matrix of the same size as \code{vmat} filled with the constant value
#' \code{k}.
#'
#' @seealso \code{\link{stm_constant}}
#' 
#' @export
#'
#' @examples
#' A <- matrix(1:12, nrow = 3, byrow = TRUE)
#' print(A)
#' stm_constant_like(A, k = 5)
#' stm_constant_like(c(1, 2, 3), k = 7)
#'
stm_constant_like <- function(vmat, k = 2) {
  check_vector_or_matrix(vmat = vmat, par_name = "vmat")
  check_numeric(num = k, par_name = "k")
  vmat <- if (is.vector(vmat)) as.matrix(vmat) else vmat
  stm_constant(k, nrows = nrow(vmat), ncols = ncol(vmat))
}
