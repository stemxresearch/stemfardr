#' Convert degrees to radians
#'
#' \code{stm_deg_to_radians()} converts a value in degrees to radians. 
#' The result can be rounded to a specified number of decimal places.
#'
#' @param degrees A numeric value representing the angle in degrees.
#' @param digits Optional. The number of decimal places to round the result.
#'              Default is \code{NULL} for no rounding.
#'
#' @return The value in radians, rounded to the specified number of decimal 
#'         places if \code{digits} is provided.
#'
#' @details
#' The function converts the given angle in degrees to radians by multiplying 
#' by \code{pi / 180}. If \code{digits} is specified, the result is rounded 
#' to that number of decimal places.
#'
#' @seealso \code{\link{stm_rad_to_degrees}}
#' 
#' @export
#' 
#' @examples
#' stm_deg_to_radians(90)
#' stm_deg_to_radians(c(0, 45, 90))
#' stm_deg_to_radians(180, digits = 2)
#' 
stm_deg_to_radians <- function(degrees, digits = NULL) {
  check_vector(vec = degrees, allow_scalar = TRUE, par_name = "degrees")
  stm_round(dat = (pi / 180) * degrees, digits = digits)
}


#' Convert radians to degrees
#'
#' \code{stm_rad_to_degrees()} converts a value in radians to degrees. The 
#' result can be rounded to a specified number of decimal places.
#'
#' @param radians A numeric value representing the angle in radians.
#' @param digits Optional. The number of decimal places to round the result. 
#'               Default is \code{NULL} for no rounding.
#'
#' @return The value in degrees, rounded to the specified number of decimal 
#'         places if \code{digits} is provided.
#'
#' @details
#' The function converts the given angle in radians to degrees by multiplying 
#' by \code{180 / pi}. If \code{digits} is specified, the result is rounded 
#' to that number of decimal places.
#'
#' @seealso \code{\link{stm_deg_to_radians}}
#' 
#' @export
#' 
#' @examples
#' stm_rad_to_degrees(pi)
#' stm_rad_to_degrees(c(pi/2, pi, 2 * pi))
#' stm_rad_to_degrees(pi/180)
#' 
stm_rad_to_degrees <- function(radians, digits = NULL) {
  check_vector(vec = radians, allow_scalar = TRUE, par_name = "radians")
  stm_round(dat = radians * (180 / pi), digits = digits)
}
