#' Create a matrix with ones on the diagonal and zeros elsewhere
#'
#' \code{stm_eye()} creates a matrix with ones along the diagonal, allowing 
#' for an offset of the diagonal (parameter \code{k}).
#'
#' @param nrows An integer specifying the number of rows in the matrix. Must 
#'              be greater than or equal to zero.
#' @param ncols An integer specifying the number of columns in the matrix. 
#'              Defaults to \code{nrows}. Must be greater than or equal to zero.
#' @param k Optional. An integer specifying the diagonal offset. The default 
#'          is \code{0} for the main diagonal. A positive value retrieves 
#'          diagonals above the main diagonal, while a negative value 
#'          retrieves diagonals below the main diagonal.
#'
#' @return A matrix of dimensions \code{nrows} x \code{ncols} with ones along 
#'         the diagonal (or at the offset specified by \code{k}), and zeros 
#'         elsewhere.
#'
#' @details
#' The function generates a matrix where ones appear on the diagonal (or an 
#' offset diagonal determined by \code{k}), and zeros appear everywhere else. 
#'
#' If \code{k} exceeds the valid range (i.e., the diagonal index is out of 
#' bounds), a matrix of zeros is returned.
#'
#' @seealso \code{\link{stm_identity}}
#'
#' @export
#'
#' @examples
#' stm_eye(nrows = 3)
#' stm_eye(nrows = 4, k = 1)
#' stm_eye(nrows = 4, ncols = 5, k = -2)
#'
stm_eye <- function(nrows, ncols = nrows, k = 0) {
  check_nrows(nrows, min = 0)
  check_ncols(ncols, min = 0)

  if (any(nrows == 0, ncols == 0)) {
    return(matrix(NA, nrow = 0, ncol = 0))
  }

  check_numeric(num = k, is_integer = TRUE, par_name = "k")

  y <- stm_zeros(nrows = nrows, ncols = ncols)

  # Check if the diagonal index is valid, if not, return zero matrix
  if (k > 0 && k >= ncols || k < 0 && abs(k) >= nrows) {
    return(y)
  }

  ones_vector <- rep(1, length(stm_diag(y, k = k)))
  y <- stm_diag(ones_vector, k = k)

  # Adjust row count if necessary
  if (nrow(y) < nrows) {
    y <- rbind(y, stm_zeros(nrows = nrows - nrow(y), ncols = ncols))
  } else if (nrow(y) > nrows) {
    y <- y[1:nrows, ]
  }

  # Adjust column count if necessary
  if (ncol(y) < ncols) {
    y <- cbind(y, stm_zeros(nrows = nrows, ncols = ncols - ncol(y)))
  } else if (ncol(y) > ncols) {
    y <- y[, 1:ncols]
  }

  return(y)
}
