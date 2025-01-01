#' Generate a random number from a vector
#'
#' \code{stm_random_number()} generates a random number from a specified 
#' vector. Optionally, the user can set the seed for reproducibility.
#'
#' @param vec A vector from which to sample. Default is \code{10:99}.
#' @param set_seed Optional seed value for random number generation. Default 
#'                 is \code{NULL}
#'
#' @return A random number sampled from the specified vector.
#'
#' @details
#' The function samples one random number from the provided vector \code{vec}. 
#' If \code{set_seed} is specified, the random number generation is 
#' reproducible. The function returns a single sampled value.
#'
#' @seealso \code{\link{stm_random_char}}, \code{\link{stm_random_vector}}, \code{\link{stm_random_matrix}}
#' 
#' @export
#' 
#' @examples
#' stm_random_number()
#' stm_random_number(vec = c(1, 2, 3, 4, 5))
#' stm_random_number(set_seed = 42)
#'
stm_random_number <- function(vec = 10:99, set_seed = NULL) {
  check_vector(vec, par_name = "vec")

  if (!is.null(set_seed)) {
    check_numeric(num = set_seed, is_integer = TRUE, par_name = "set_seed")
    set.seed(set_seed)
  }

  sample(vec, size = 1)
}
