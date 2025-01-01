#' @noRd 
stm_tri_lower_upper <- function(mat, k = 0, is_lower = TRUE) {
  mat <- check_matrix(mat, par_name = "mat")
  k <- check_numeric(num = k, is_integer = TRUE)
  if (is_lower) {
    mat[col(mat) >= row(mat) + k + 1] <- 0
  } else {
    mat[col(mat) <= row(mat) + k - 1] <- 0
  }
  return(mat)
}


#' Extract lower triangular part of a matrix
#'
#' \code{stm_tril()} returns the lower triangular part of a matrix, optionally 
#' with a specified diagonal offset.
#'
#' @param mat A numeric matrix.
#' @param k An integer indicating the diagonal to be used. Default is \code{0},
#'          which returns the lower triangular part including the main diagonal.
#'
#' @return A matrix containing the lower triangular part of \code{mat}, 
#'         with the specified diagonal offset.
#'
#' @details
#' This function calls \code{stm_tri_lower_upper()} to extract the lower 
#' triangular part of the matrix.
#'
#' @seealso \code{\link{stm_triu}}
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:16, nrow = 4, byrow = TRUE)
#' print(A)
#' stm_tril(A)
#'
stm_tril <- function(mat, k = 0) {
  stm_tri_lower_upper(mat, k = k, is_lower = TRUE)
}


#' Extract upper triangular part of a matrix
#'
#' \code{stm_triu()} returns the upper triangular part of a matrix, optionally 
#' with a specified diagonal offset.
#'
#' @param mat A numeric matrix.
#' @param k An integer indicating the diagonal to be used. Default is \code{0},
#'          which returns the upper triangular part including the main diagonal.
#'
#' @return A matrix containing the upper triangular part of \code{mat}, 
#'         with the specified diagonal offset.
#'
#' @details
#' This function calls \code{stm_tri_lower_upper()} to extract the upper 
#' triangular part of the matrix.
#'
#' @seealso \code{\link{stm_tril}}
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:16, nrow = 4, byrow = TRUE)
#' print(A)
#' stm_triu(A)
#'
stm_triu <- function(mat, k = 0) {
  stm_tri_lower_upper(mat, k = k, is_lower = FALSE)
}
