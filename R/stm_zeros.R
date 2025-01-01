#' Generate a zero matrix
#'
#' Returns a matrix filled with zeros, with dimensions specified by the user.
#' If dimensions are not provided, a 1 x 1 zero matrix will be returned.
#'
#' @param nrows Optional. The number of rows in the matrix. Defaults to
#'              \code{1} if not specified. Must be a positive integer.
#' @param ncols Optional. The number of columns in the matrix. If \code{NULL},
#'              it defaults to the same value as \code{nrows}. Must also be
#'              a positive integer.
#'
#' @return A matrix filled with zeros, with dimensions defined by \code{nrows}
#'         and \code{ncols}. Returns an empty matrix with dimensions
#'         \code{(0, 0)} if either dimension is zero.
#'
#' @examples
#' stm_zeros(nrows = 4)
#' stm_zeros(nrows = 3, ncols = 3)
#' stm_zeros(nrows = 2, ncols = 4)
#'
#' @export
stm_zeros <- function(nrows, ncols = nrows) {
  stm_constant(k = 0, nrows = nrows, ncols = ncols)
}
