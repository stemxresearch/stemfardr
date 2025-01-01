#' Transpose data frame, matrix, or tibble
#'
#' \code{stm_transpose()} transposes a data frame, matrix, or tibble, 
#' converting it into the appropriate format after transposition.
#' 
#' @rdname transpose
#'
#' @param dat A matrix, data frame, or tibble to be transposed.
#'
#' @return The transposed \code{dat}, either as a matrix, data frame,
#'         or tibble.
#'
#' @details
#' The function first checks the class of \code{dat} and transposes it 
#' accordingly. If it's a data frame, it converts it into a standard data 
#' frame after transposition. If it's a tibble, it also cleans up the column 
#' names using \code{janitor::clean_names()}. If the data is a matrix or 
#' vector, it is directly transposed.
#' 
#' @export
#' 
#' @examples
#' # Matrix example
#' A <- matrix(11:22, nrow = 3, byrow = TRUE)
#' print(A)
#' stm_transpose(A)
#'
#' # Data frame example
#' df <- data.frame(
#'   A = c(1, 2, 3),
#'   B = c(4, 5, 6)
#' )
#' print(df)
#' stm_transpose(df)
#'
#' # Tibble example
#' library(tibble)
#' tbl <- tibble(
#'   A = c(1, 2, 3),
#'   B = c(4, 5, 6)
#' )
#' print(tbl)
#' stm_transpose(tbl)
#'
stm_transpose <- function(dat) {
  if (check_is_dataframe_not_tibble(dat)) {
    dat <- as.data.frame(t(dat))
  } else if (inherits(dat, "tbl_df")) {
    dat <- tibble::as_tibble(t(dat), .name_repair = "minimal", row_names = NA)
    dat <- janitor::clean_names(dat)
  } else if (is.matrix(dat) || is.vector(dat)){
    dat <- t(dat)
  } else {
    stop("Expected 'dat' to be a matrix, data frame, or tibble, got --> ",
         class(dat))
  }
  return(dat)
}


#' @rdname transpose
#' @export 
stm_t <- function(dat) {
  stm_transpose(dat)
}