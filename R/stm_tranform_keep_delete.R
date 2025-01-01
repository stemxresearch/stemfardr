#' @noRd 
stm_transform_keep_delete <- function(
    dat, indices, is_keep = TRUE, is_rows = TRUE, as_matrix = TRUE
) {
  dat <- check_vector_or_matrix(vmat = dat)
  indices <- check_vector(
    vec = indices, is_numeric = TRUE, allow_scalar= TRUE,
    par_name = "indices"
  )
  indices <- floor(indices)
  indices <- check_all_positive(indices, par_name = 'indices')
  indices <- indices[indices <= ifelse(is_rows, nrow(dat), ncol(dat))]
  is_keep <- check_logical(is_keep, default = TRUE)
  is_rows <- check_logical(is_rows, default = TRUE)
  as_matrix <- check_logical(as_matrix, default = TRUE)
  indices <- if (!is_keep) -indices else indices
  dat <- if (is_rows) dat[indices, ] else dat[, indices]
  if (is.vector(dat) && as_matrix) {
    dat <- if (is_rows) t(as.matrix(dat)) else as.matrix(dat)
  }
  return(dat)
}


#' Keep specific rows in a data frame or matrix
#'
#' \code{stm_keep_rows()} keeps the specified rows from a data frame or 
#' matrix based on the provided indices.
#'
#' @param dat A data frame or matrix from which rows are to be kept.
#' @param indices A vector of row indices to retain.
#' @param as_matrix A logical value indicating whether the result should 
#'                  be converted to a matrix. Default is \code{TRUE}.
#'
#' @return A data frame or matrix with the specified rows retained.
#'
#' @details
#' This function uses \code{stm_transform_keep_delete()} to retain the rows 
#' specified in \code{indices}. The result can optionally be returned as a 
#' matrix based on the \code{as_matrix} argument.
#'
#' @seealso \code{\link{stm_keep_cols}}, \code{\link{stm_delete_rows}}, \code{\link{stm_keep_cols}}
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:15, ncol = 3, byrow = TRUE)
#' print(A)
#' stm_keep_rows(A, c(1, 3))
#'
stm_keep_rows <- function(dat, indices, as_matrix = TRUE) {
  stm_transform_keep_delete(
    dat, indices = indices, is_keep = TRUE, is_rows = TRUE,
    as_matrix = as_matrix
  )
}


#' Delete specific rows from a data frame or matrix
#'
#' \code{stm_delete_rows()} deletes the specified rows from a data frame or 
#' matrix based on the provided indices.
#'
#' @param dat A data frame or matrix from which rows are to be deleted.
#' @param indices A vector of row indices to delete.
#' @param as_matrix A logical value indicating whether the result should 
#'                  be converted to a matrix. Default is \code{TRUE}.
#'
#' @return A data frame or matrix with the specified rows deleted.
#'
#' @details
#' This function uses \code{stm_transform_keep_delete()} to remove the rows 
#' specified in \code{indices}. The result can optionally be returned as a 
#' matrix based on the \code{as_matrix} argument.
#'
#' @seealso \code{\link{stm_delete_cols}}, \code{\link{stm_keep_rows}}, \code{\link{stm_keep_cols}}
#' 
#' @export
#' 
#' @examples
#' A <- matrix(1:15, ncol = 3, byrow = TRUE)
#' print(A)
#' stm_delete_rows(A, 2)
#' stm_delete_rows(A, c(1, 3))
#'
stm_delete_rows <- function(dat, indices, as_matrix = TRUE) {
  stm_transform_keep_delete(
    dat = dat, indices = indices, is_keep = FALSE, is_rows = TRUE,
    as_matrix = as_matrix
  )
}


#' Keep specified columns in a data frame
#'
#' \code{stm_keep_cols()} retains specified columns from a data frame.
#' It uses \code{stm_transform_keep_delete()} to perform the operation.
#'
#' @param dat A data frame or matrix from which columns will be retained.
#' @param indices A numeric vector or logical vector indicating the columns
#'                to retain.
#' @param as_matrix Optional. A logical value indicating whether the result 
#'                  should be returned as a matrix. Default is \code{TRUE}.
#'
#' @return A data frame or matrix containing only the specified columns.
#'
#' @details
#' This function uses \code{stm_transform_keep_delete()} to retain the specified
#' columns in the given data. If \code{as_matrix} is \code{TRUE}, the result
#' is returned as a matrix, otherwise a data frame is returned.
#'
#' @seealso \code{\link{stm_keep_rows}}, \code{\link{stm_delete_cols}}, \code{\link{stm_keep_rows}}
#'
#' @export
#'
#' @examples
#' A <- matrix(1:15, ncol = 5, byrow = TRUE)
#' print(A)
#' stm_keep_cols(A, 2)
#' stm_keep_cols(A, 2, as_matrix = FALSE)
#'
stm_keep_cols <- function(dat, indices, as_matrix = TRUE) {
  stm_transform_keep_delete(
    dat = dat, indices = indices, is_keep = TRUE, is_rows = FALSE,
    as_matrix = as_matrix
  )
}


#' Delete specified columns from a data frame
#'
#' \code{stm_delete_cols()} removes specified columns from a data frame.
#' It uses \code{stm_transform_keep_delete()} to perform the operation.
#'
#' @param dat A data frame or matrix from which columns will be removed.
#' @param indices A numeric vector or logical vector indicating the columns
#'                to delete.
#' @param as_matrix Optional. A logical value indicating whether the result 
#'                  should be returned as a matrix. Default is \code{TRUE}.
#'
#' @return A data frame or matrix with the specified columns removed.
#'
#' @details
#' This function uses \code{stm_transform_keep_delete()} to remove the specified
#' columns from the given data. If \code{as_matrix} is \code{TRUE}, the result
#' is returned as a matrix, otherwise a data frame is returned.
#'
#' @seealso \code{\link{stm_keep_cols}}, \code{\link{stm_delete_rows}}, \code{\link{stm_keep_rows}}
#'
#' @export
#'
#' @examples
#' A <- matrix(1:15, ncol = 5, byrow = TRUE)
#' print(A)
#' stm_delete_cols(A, 2)
#'
stm_delete_cols <- function(dat, indices, as_matrix = TRUE) {
  stm_transform_keep_delete(
    dat, indices = indices, is_keep = FALSE, is_rows = FALSE,
    as_matrix = as_matrix
  )
}
