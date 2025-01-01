#' @noRd
stm_matrix_lus <- function(
    vec, matrix_type = c("symmetric", "lower", "upper")) {
  x <- check_vector(vec, par_name = "vec")
  matrix_type <- match.arg(matrix_type)
  y <- stm_ones(nrows = as.integer(sqrt(length(x) * 2)))
  switch(
    matrix_type,
    lower = y <- stm_tril(y),
    upper = y <- stm_triu(y),
    symmetric = {
      y <- stm_tril(y)
      y <- y + t(y)
    }
  )
  y[y == 1] <- x
  return(y)
}


#' Create a lower triangular matrix from a vector
#'
#' \code{stm_tril_from_vector()} generates a lower triangular matrix from
#' a given vector. The function calls another matrix function to generate
#' the matrix.
#'
#' @param vec A numeric vector. The vector from which the lower triangular
#'             matrix is created.
#'
#' @return A lower triangular matrix created from the input vector.
#'
#' @details This function relies on \code{stm_matrix_lus()} to generate the 
#'          matrix.
#'
#' @seealso \code{\link{stm_triu_from_vector}}, \code{\link{stm_symmetric_from_vector}}
#' 
#' @export
#'
#' @examples
#' v <- c(1, 2, 3, 4, 5, 6)
#' print(v)
#' stm_tril_from_vector(v)
#'
stm_tril_from_vector <- function(vec) {
  stm_matrix_lus(vec, matrix_type = "lower")
}


#' Create an upper triangular matrix from a vector
#'
#' \code{stm_triu_from_vector()} generates an upper triangular matrix from
#' a given vector. The function calls another matrix function to generate
#' the matrix.
#'
#' @param vec A numeric vector. The vector from which the upper triangular
#'             matrix is created.
#'
#' @return An upper triangular matrix created from the input vector.
#'
#' @details This function relies on \code{stm_matrix_lus()} to generate the 
#'          matrix.
#'
#' @seealso \code{\link{stm_tril_from_vector}}, \code{\link{stm_symmetric_from_vector}}
#' 
#' @export
#'
#' @examples
#' v <- c(1, 2, 3, 4, 5, 6)
#' print(v)
#' stm_triu_from_vector(v)
#'
stm_triu_from_vector <- function(vec) {
  stm_matrix_lus(vec, matrix_type = "upper")
}


#' Create a symmetric matrix from a vector
#'
#' \code{stm_symmetric_from_vector()} generates a symmetric matrix from
#' a given vector. The function calls another matrix function to create
#' the matrix.
#'
#' @param vec A numeric vector. The vector from which the symmetric
#'             matrix is created.
#'
#' @return A symmetric matrix created from the input vector.
#'
#' @details This function relies on \code{stm_matrix_lus()} to generate the 
#'          matrix.
#'
#' @seealso \code{\link{stm_tril_from_vector}}, \code{\link{stm_triu_from_vector}}
#' 
#' @export
#'
#' @examples
#' v <- c(1, 2, 3, 4, 5, 6)
#' print(v)
#' stm_symmetric_from_vector(v)
#'
stm_symmetric_from_vector <- function(vec) {
  stm_matrix_lus(vec, matrix_type = "symmetric")
}

