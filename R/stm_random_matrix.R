#' Generate a random matrix
#'
#' \code{stm_random_matrix()} generates a matrix of random numbers with 
#' specified size and range. Optionally, it allows the user to set the seed 
#' for reproducibility and assign row and column names.
#'
#' @param min Minimum value in the matrix. Default is \code{-4}.
#' @param max Maximum value in the matrix. Default is \code{9}.
#' @param nrows Number of rows in the matrix. Default is \code{1}.
#' @param ncols Number of columns in the matrix. Default is equal to \code{nrows}.
#' @param replace Logical value indicating whether sampling should be with 
#'        replacement. Default is \code{TRUE}.
#' @param set_seed Optional seed value for random number generation.
#' @param row_names Optional character vector of row names.
#' @param col_names Optional character vector of column names.
#'
#' @return A matrix of random numbers with the specified size, range, and names.
#'
#' @details
#' The function generates a matrix of random numbers within the specified 
#' range (\code{min} to \code{max}) and with the specified dimensions 
#' (\code{nrows} and \code{ncols}). If \code{replace} is \code{TRUE}, sampling 
#' is done with replacement. If \code{set_seed} is provided, the random number 
#' generation is reproducible. Additionally, row and column names can be specified. 
#' If the names don't match the matrix dimensions, an error is raised.
#'
#' @seealso \code{\link{stm_random_char}}, \code{\link{stm_random_number}}, \code{\link{stm_random_vector}}, \code{\link{stm_random_matrix}}
#' 
#' @export
#' 
#' @examples
#' # note that your values will be different
#' stm_random_matrix()
#' stm_random_matrix(min = 0, max = 1, nrows = 3, ncols = 3)
#' stm_random_matrix(min = 10, max = 99, nrows = 5, ncols = 4)
#'
stm_random_matrix <- function(
    min = -4, max = 9, nrows = 1, ncols = nrows, replace = TRUE,
    set_seed = NULL, row_names = NULL, col_names = NULL) {
  check_numeric(num = min, par_name = "min")
  check_numeric(num = max, par_name = "max")
  check_limits(min, max, lower_par_name = "lower", upper_par_name = "upper")
  check_nrows(n = nrows)
  check_ncols(n = ncols)
  replace <- check_logical(lgl = replace, default = TRUE)
  if (!is.null(set_seed)) {
    check_numeric(num = set_seed, is_integer = TRUE, par_name = "set_seed")
    set.seed(set_seed)
  }
  y <- sample(x = min:max, size = nrows * ncols, replace = replace)
  A <- matrix(y, nrow = nrows, ncol = ncols)
  if (!is.null(row_names)) {
    s <- check_singular_plural(n = nrows)
    if (length(row_names) != nrow(A)) {
      stop("Expected ", nrows, " row", s, ", got --> ", nrow(A), " row", s)
    }
    rownames(A) <- row_names
  }
  if (!is.null(col_names)) {
    s <- check_singular_plural(n = ncols)
    if (length(col_names) != ncol(A)) {
      stop("Expected ", ncols, " column", s, ", got --> ", nrow(A),
           " column", s)
    }
    colnames(A) <- col_names
  }
  return(A)
}
