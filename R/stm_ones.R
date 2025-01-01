#' Create a matrix of ones
#'
#' \code{stm_ones()} creates a matrix of ones with the specified number of
#' rows and columns. It uses the \code{stm_constant()} function to fill
#' the matrix with ones.
#'
#' @param nrows The number of rows in the matrix.
#' @param ncols The number of columns in the matrix. Defaults to \code{nrows}.
#'
#' @return A matrix filled with ones, with dimensions specified by 
#'         \code{nrows} and \code{ncols}.
#'
#' @seealso \code{\link{stm_ones_like}}, \code{\link{stm_zeros}}, \code{\link{stm_zeros_like}}, \code{\link{stm_constant}}, \code{\link{stm_constant_like}}
#'
#' @export
#'
#' @examples
#' stm_ones(nrows = 4)
#' stm_ones(nrows = 3, ncols = 3)
#' stm_ones(nrows = 2, ncols = 4)
#'
stm_ones <- function(nrows, ncols = nrows) {
  stm_constant(k = 1, nrows = nrows, ncols = ncols)
}