#' Generate the Rosser matrix
#'
#' \code{stm_rosser()} generates a specific 8x8 matrix used in numerical 
#' examples. The matrix elements are predefined and arranged in a specific 
#' pattern.
#'
#' @return An 8x8 matrix of predefined values.
#'
#' @details
#' The function returns a matrix that is commonly used in numerical linear 
#' algebra problems, known as the Rosser matrix.
#' 
#' @export
#' 
#' @examples
#' stm_rosser()
#'
stm_rosser <- function() {
  y <- c(
    611, 196, -192, 407, -8, -52, -49, 29,
    196, 899, 113, -192, -71, -43, -8, -44,
    -192, 113, 899, 196, 61, 49, 8, 52,
    407, -192, 196, 611, 8, 44, 59, -23,
    -8, -71, 61, 8, 411, -599, 208, 208,
    -52, -43, 49, 44, -599, 411, 208, 208,
    -49, -8, 8, 59, 208, 208, 99, -911,
    29, -44, 52, -23, 208, 208, -911, 99
  )
  matrix(y, nrow = 8, byrow = TRUE)
}