#' @noRd 
stm_tranform_fliprotate <- function() {
  return(invisible(NULL))
}


#' @noRd 
stm_flip <- function(mat, left_right = TRUE) {
  check_matrix(mat, par_name = "mat")
  left_right <- check_logical(lgl = left_right, default = TRUE)
  if (left_right) { # stm_fliplr()
    mat <- mat[, rev(seq_len(ncol(mat)))]
  } else { # stm_flipud()
    mat <- mat[rev(seq_len(nrow(mat))), ]
  }
  return(mat)
}


#' Flip a matrix left to right
#'
#' \code{stm_fliplr()} flips a matrix left to right, i.e., it reverses the 
#' order of columns. This is achieved by calling the \code{stm_flip()} function 
#' with \code{left_right = TRUE}.
#'
#' @param mat A numeric matrix to be flipped left to right.
#'
#' @return A matrix that is flipped left to right.
#'
#' @details This function uses the \code{stm_flip()} function with the argument 
#'          \code{left_right = TRUE} to flip the matrix horizontally.
#'
#' @seealso \code{\link{stm_flipud}}
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:12, ncol = 4, byrow = TRUE)
#' print(A)
#' stm_fliplr(A)
#'
stm_fliplr <- function(mat) {
  stm_flip(mat, left_right = TRUE)
}


#' Flip a matrix upside down
#'
#' \code{stm_flipud()} flips a matrix upside down, i.e., it reverses the 
#' order of rows. This is achieved by calling the \code{stm_flip()} function 
#' with \code{left_right = FALSE}.
#'
#' @param mat A numeric matrix to be flipped upside down.
#'
#' @return A matrix that is flipped upside down.
#'
#' @details This function uses the \code{stm_flip()} function with the argument 
#'          \code{left_right = FALSE} to flip the matrix vertically.
#'
#' @seealso \code{\link{stm_fliplr}}
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:16, ncol = 4, byrow = TRUE)
#' print(A)
#' stm_flipud(A)
#'
#' @export
stm_flipud <- function(mat) {
  stm_flip(mat, left_right = FALSE)
}


#' Rotate a matrix by 90 degrees
#'
#' \code{stm_rot90()} rotates a matrix by 90 degrees counterclockwise. The 
#' number of 90-degree rotations is determined by the argument \code{k}.
#'
#' @param mat A numeric matrix to be rotated.
#' @param k The number of 90-degree rotations. Default is \code{1}, which 
#'          rotates the matrix once. The matrix can be rotated multiple 
#'          times by specifying higher values for \code{k}.
#'
#' @return A rotated matrix.
#'
#' @details The function supports multiple rotations by accepting an integer 
#'          \code{k}. The matrix is rotated counterclockwise by 90 degrees 
#'          for each rotation. The parameter \code{k} is modulo 4 to handle 
#'          the cases where more than 4 rotations are requested.
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:12, nrow = 3, byrow = TRUE)
#' print(A)
#' stm_rot90(A)
#' stm_rot90(A, k = 2)
#' stm_rot90(A, k = -2)
#' stm_rot90(A, k = 3)
#' stm_rot90(A, k = 4) # original matrix
#'
stm_rot90 <- function(mat, k = 1) {
  check_matrix(mat = mat, par_name = "mat")
  check_numeric(num = k, is_integer = TRUE, par_name = "k")
  row_indices <- rev(seq_len(nrow(mat)))
  col_indices <- rev(seq_len(ncol(mat)))
  switch(
    EXPR = 1 + (k %% 4),
    mat,
    t(mat[, col_indices]),
    mat[row_indices, col_indices],
    {
      t(mat)[, row_indices] # used `row_indices` because of t(mat)
    }
  )
}
