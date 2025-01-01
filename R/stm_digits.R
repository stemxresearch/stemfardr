#' Set default number of digits for computation
#'
#' \code{stm_digits()} determines the number of decimal digits to be used 
#' for computations, providing a default value if no valid input is 
#' supplied.
#'
#' This function checks if the provided \code{digits} parameter is a valid 
#' integer. If the input is invalid or \code{NULL}, the function defaults 
#' to 14 decimal digits.
#'
#' @param digits An integer specifying the number of decimal digits. Default 
#'               is 14.
#'
#' @return An integer representing the number of decimal digits to use.
#' 
#' @export
#'
#' @examples
#' stm_digits(10)
#'
#' stm_digits(NULL)
#'
#' stm_digits("abc")
#' 
#' stm_digits()
#'
stm_digits <- function(digits = 14) {
  if (check_is_scalar_integer(digits)) digits else 14
}