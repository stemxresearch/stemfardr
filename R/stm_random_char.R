#' Generate a random string of characters
#'
#' \code{stm_random_char()} generates a random string of characters of 
#' specified length, including numbers, lowercase, and uppercase letters
#' based on user-defined options.
#'
#' @param n The length of the random string to generate. Defaults to 8.
#' @param numbers Whether to include numeric digits (0-9). Defaults to 
#'                \code{TRUE}.
#' @param include_lowercase Whether to include lowercase letters. Defaults to 
#'                          \code{TRUE}.
#' @param include_uppercase Whether to include uppercase letters. Defaults to 
#'                          \code{TRUE}.
#' @param set_seed Optional. An integer seed for random number generation.
#'                 Defaults to \code{NULL} (no fixed seed).
#'
#' @return A random string of characters of length \code{n}.
#'
#' @seealso \code{\link{stm_random_number}}, \code{\link{stm_random_vector}}, \code{\link{stm_random_matrix}}
#'
#' @export
#'
#' @examples
#' # Generate a random string with default parameters
#' stm_random_char()
#'
#' # Generate a random string with only lowercase letters
#' stm_random_char(include_uppercase = FALSE)
#'
#' # Generate a random string with only numbers
#' stm_random_char(include_lowercase = FALSE, include_uppercase = FALSE)
#'
stm_random_char <- function(
    n = 8, numbers = TRUE, include_lowercase = TRUE, include_uppercase = TRUE,
    set_seed = NULL
) {
  check_numeric(num = n, min = 1, is_integer = TRUE, par_name = "n")
  numbers <- check_logical(lgl = numbers, default = TRUE)
  include_lowercase <- check_logical(lgl = include_lowercase, default = TRUE)
  include_uppercase <- check_logical(lgl = include_uppercase, default = TRUE)
  if (!is.null(set_seed)) {
    check_numeric(
      num = set_seed, min = 1, is_integer = TRUE, par_name = "set_seed"
    )
    set.seed(set_seed)
  }
  if (!numbers && !include_lowercase && !include_uppercase) {
    numbers <- TRUE
    include_lowercase <- TRUE
    include_uppercase <- TRUE
  }
  characters <- character()
  if (numbers) {
    characters <- c(characters, rep(0:9, times = 3))
  }
  if (include_lowercase) {
    characters <- c(characters, letters)
  }
  if (include_uppercase) {
    characters <- c(characters, LETTERS)
  }
  paste0(sample(x = characters, size = n, replace = TRUE), collapse = "")
}
