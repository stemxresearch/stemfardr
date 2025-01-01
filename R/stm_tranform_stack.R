#' @noRd 
stm_tranform_stack <- function() {
  return(invisible(NULL))
}


#' Stack columns together into a data frame or matrix
#'
#' \code{stm_stack_cols()} combines multiple data frames or vectors by
#' column binding them together using \code{cbind()}.
#'
#' @param ... Data frames, matrices, or vectors to be combined as columns.
#'
#' @return A data frame or matrix resulting from column-wise binding of the 
#'         input data.
#'
#' @details
#' This function stacks the provided data frames, matrices, or vectors as 
#' columns using the \code{cbind()} function. The resulting object is returned
#' as a data frame or matrix.
#'
#' @seealso \code{\link{cbind}}, \code{\link{rbind}}, \code{\link{stm_stack_rows}}
#'
#' @export
#'
#' @examples
#' A <- matrix(1:4, nrow = 2, byrow = TRUE)
#' print(A)
#' B <- matrix(5:8, nrow = 2, byrow = TRUE)
#' print(B)
#' stm_stack_cols(A, B)
#'
stm_stack_cols <- function(...) {
  cbind(...)
}


#' Stack rows together into a data frame or matrix
#'
#' \code{stm_stack_rows()} combines multiple data frames or vectors by
#' row binding them together using \code{rbind()}.
#'
#' @param ... Data frames, matrices, or vectors to be combined as rows.
#'
#' @return A data frame or matrix resulting from row-wise binding of the 
#'         input data.
#'
#' @details
#' This function stacks the provided data frames, matrices, or vectors as rows
#' using the \code{rbind()} function. The resulting object is returned as a data
#' frame or matrix.
#'
#' @seealso \code{\link{cbind}}, \code{\link{rbind}}, \code{\link{stm_stack_cols}}
#'
#' @export
#'
#' @examples
#' A <- matrix(1:4, ncol = 2, byrow = TRUE)
#' print(A)
#' B <- matrix(5:8, ncol = 2, byrow = TRUE)
#' print(B)
#' stm_stack_rows(A, B)
#'
stm_stack_rows <- function(...) {
  rbind(...)
}
