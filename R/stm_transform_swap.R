#' @noRd 
stm_tranform_swap <- function() {
  return(invisible(NULL))
}


#' @noRd
stm_swap <- function(dat, i, j, is_rows = TRUE) {

  dat <- check_vector_or_matrix(vmat = dat, par_name = "vmat")
  upper_limit <- ifelse(
    is.vector(dat), length(dat), ifelse(is_rows, nrow(dat), ncol(dat))
  )
  i <- check_numeric(
    num = i, min = 1, max = upper_limit, is_integer = TRUE, par_name = "i"
  )
  j <- check_numeric(
    num = j, min = 1, max = upper_limit, is_integer = TRUE, par_name = "j"
  )
  is_rows <- check_logical(is_rows, default = TRUE)
  is_vector <- is.vector(dat)
  if (is_vector) {
    dat <- if (is_rows) t(t(dat)) else t(dat)
  }
  if (is_rows) {
    dat[c(i, j), ] <- dat[c(j, i), ]
  } else {
    dat[, c(i, j)] <- dat[, c(j, i)]
  }
  if (is_vector) as.vector(dat) else dat
}


#' Swap elements in a vector, rows, or columns
#'
#' The \code{stm_swap...()} set of functions swap the elements at specified 
#' indices in a vector, rows in a matrix or data frame, or columns in a matrix 
#' or data frame.
#' 
#' @rdname swap
#'
#' @param dat The input vector, matrix, or data frame. In
#'   \code{stm_swap_vector}, it should be a vector. In
#'   \code{stm_swap_rows} and \code{stm_swap_cols}, it should be a matrix or 
#'   data frame.
#' @param i The index of the first element (or row/column) to swap.
#' @param j The index of the second element (or row/column) to swap.
#'
#' @return A vector, matrix, or data frame with the specified elements swapped.
#'
#' @examples
#' # vector
#' v <- c(1, 2, 3, 4, 5)
#' print(v)
#' stm_swap_vector(v, i = 2, j = 4)
#' 
#' # swap matrix rows
#' A <- matrix(1:9, ncol = 3, byrow = TRUE)
#' print(A)
#' stm_swap_rows(A, 1, 3)
#' 
#' # swap matrix columns
#' A <- matrix(1:9, ncol = 3, byrow = TRUE)
#' print(A)
#' stm_swap_cols(A, 1, 3)
#'
#' @export
stm_swap_vector <- function(dat, i, j) {
  stm_swap(dat, i = i, j = j, is_rows = FALSE)
}


#' @rdname swap
#' @export
stm_swap_rows <- function(dat, i, j) {
  stm_swap(dat, i = i, j = j, is_rows = TRUE)
}


#'
#' @rdname swap
#'
#' @export
#' 
stm_swap_cols <- function(dat, i, j) {
  stm_swap(dat, i = i, j = j, is_rows = FALSE)
}
