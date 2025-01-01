#' Round numeric data to specified digits
#'
#' \code{stm_round()} rounds numeric data to a specified number of digits.
#' If the input is a list, the rounding is applied element-wise to each 
#' numeric element.
#'
#' @param dat A numeric vector, list, or matrix to round.
#' @param digits Optional. The number of decimal places to round to. If not 
#'               provided or invalid, the original data is returned.
#'
#' @return The rounded data.
#'
#' @details
#' The function applies rounding to numeric data. If \code{dat} is a list, 
#' it will apply rounding element-wise to each numeric element in the list. 
#' If an error occurs during rounding, the original data is returned.
#' 
#' @export
#' 
#' @examples
#' stm_round(3.14159, digits = 2)
#' stm_round(3.14159, digits = "two")
#' a <- list(3.14159, "hello", 2.71828, list(1.61803, "world"))
#' stm_round(a, digits = 3)
#'
stm_round <- function(dat, digits = NULL) {
  if ((is.null(digits) || is.character(digits))) {
    return(dat)
  }

  if (length(digits) == 1 && as.integer(digits) == digits && digits >= 0) {
    if (check_is_list(dat)) {
      return(
        lapply(dat, function(y) {
          return(if (is.numeric(y)) round(y, digits) else y)
        })
      )
    } else {
      tryCatch({
        return(round(dat, digits))
      }, error = function(e) {
        return(dat)
      })
    }
  } else {
    return(dat)
  }
}
