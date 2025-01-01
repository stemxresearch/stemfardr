#' Generate a random vector
#'
#' \code{stm_random_vector()} generates a random vector with values between 
#' the specified minimum and maximum. Optionally, the vector can be sorted 
#' in increasing or decreasing order.
#'
#' @param min The minimum value of the vector elements. Default is 10.
#' @param max The maximum value of the vector elements. Default is 99.
#' @param n The number of elements in the vector. Default is 10.
#' @param replace A logical indicating whether sampling is with replacement.
#'                Default is \code{TRUE}.
#' @param set_seed Optional seed for random number generation.
#' @param is_sorted A logical indicating whether to sort the vector. 
#'                  Default is \code{FALSE}.
#' @param is_decreasing A logical indicating whether to sort the vector 
#'                      in decreasing order. Default is \code{FALSE}.
#'
#' @return A numeric vector of random values, optionally sorted.
#'
#' @details
#' This function generates a random vector with the specified number of 
#' elements and value range. The vector can be sorted in increasing or 
#' decreasing order based on the \code{is_sorted} and \code{is_decreasing} 
#' parameters.
#'
#' @seealso \code{\link{stm_random_char}}, \code{\link{stm_random_number}}, \code{\link{stm_random_matrix}}
#' 
#' @export
#' 
#' @examples
#' stm_random_vector()
#' stm_random_vector(n = 5, is_sorted = TRUE)
#' stm_random_vector(
#'   n = 8, is_sorted = TRUE, is_decreasing = TRUE, set_seed = 42
#' )
#'
stm_random_vector <- function(
    min = 10, max = 99, n = 10, replace = TRUE, set_seed = NULL,
    is_sorted = FALSE, is_decreasing = FALSE
) {
  check_numeric(num = min, par_name = "min")
  check_numeric(num = max, par_name = "max")
  check_numeric(num = n, min = 1, is_integer = TRUE, par_name = "n")
  replace = check_logical(lgl = replace, default = TRUE)
  is_sorted <- check_logical(lgl = is_sorted, default = FALSE)
  is_decreasing <- check_logical(lgl = is_decreasing, default = FALSE)

  y <- as.vector(stm_random_matrix(
    min, max, nrows = n, ncols = 1, replace = replace, set_seed = set_seed
  ))

  if (is_sorted) sort(y, decreasing = is_decreasing) else y
}
