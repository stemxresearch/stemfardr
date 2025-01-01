#' Repeat elements of a vector or matrix
#'
#' \code{stm_repeat()} repeats elements of a vector or matrix. It can repeat 
#' based on the entire vector or column-wise for matrices. Optionally, 
#' it can repeat each element individually.
#'
#' @param dat A vector, matrix, or scalar to be repeated.
#' @param times A numeric vector or scalar indicating the number of times 
#'              elements should be repeated.
#' @param is_each Logical. If \code{TRUE}, each element of \code{dat} will 
#'                be repeated individually. Default is \code{FALSE}.
#' @param on_columns Logical. If \code{TRUE}, the repetition is done 
#'                   column-wise for matrices. Default is \code{TRUE}.
#'
#' @return A vector or matrix with repeated elements.
#'
#' @details
#' This function repeats the elements of a vector or matrix. For vectors, 
#' elements can be repeated individually. For matrices, repetition can 
#' be done either row-wise or column-wise.
#' 
#' @export
#' 
#' @examples
#' v = c(4, 5, 8, 3)
#' stm_repeat(v, times = 3)
#' stm_repeat(v, times = c(3, 2, 1, 5))
#' A <- matrix(1:6, nrow = 2, byrow = TRUE)
#' print(A)
#' stm_repeat(A, times = 3)
#' stm_repeat(A, times = 3, on_columns = FALSE)
#'
#' @export
stm_repeat <- function(dat, times, is_each = FALSE, on_columns  = TRUE) {
  dat <- check_vector_or_matrix(
    vmat = dat, allow_scalar = TRUE, par_name = "vmat"
  )
  times <- check_vector(
    vec = times, is_numeric = TRUE, allow_scalar = TRUE, par_name = "times"
  )
  if (is.vector(dat)) {
    is_each <- check_logical(lgl = is_each, default = TRUE)
    if (length(times) > 1) { # if 2 or more elements
      check_length_equal(
        x = dat, y = times, allow_scalar = TRUE, xpar_name = "dat",
        ypar_name = "times"
      )
    }
    if (is_each && check_scalar_integer(times)) {
      times <- round(rep(times, length(dat)))
    }
    dat <- rep(dat, times = times)
  } else {
    on_columns <- check_logical(lgl = on_columns, default = TRUE)
    if (!check_scalar_integer(times)) {
      stop("'times' must be an integer when 'dat' is a matrix")
    }
    if (on_columns) {
      dat <- matrix(rep(dat, times = times), nrow = nrow(dat), byrow = FALSE)
    } else {
      dat <- t(dat)
      dat <- t(
        matrix(rep(dat, times = times), nrow = nrow(dat), byrow = FALSE)
      )
    }
  }
  return(dat)
}
