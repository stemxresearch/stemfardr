#' Insert values into a matrix or vector
#'
#' \code{stm_insert()} and its variants insert value(s) into a matrix or
#' vector at a specified location. The function allows inserting data
#' into either rows or columns, or simply appending values to the end.
#' 
#' @rdname insert
#'
#' @param dat The matrix, vector, or array to which new value(s) will be added.
#'             \describe{
#'               \item{In \code{stm_insert}}{Can be a vector or matrix.}
#'               \item{In \code{stm_insert_vector}}{Specifically a vector.}
#'               \item{In \code{stm_insert_rows}}{Specifically a matrix.}
#'               \item{In \code{stm_insert_cols}}{Specifically a matrix.}
#'             }
#' @param values The value(s) to insert as new columns or rows.
#' @param insert_at Optional. The location where to insert the new value(s). 
#'                  If \code{length(dat)}, value(s) are inserted at the 
#'                  end of the matrix or vector.
#' @param axis Optional. The axis along which to insert the values: 
#'             \code{0} for rows, \code{1} for columns. Default is \code{NULL},
#'             meaning the values will be inserted into the vector.
#'
#' @return A matrix or vector with the new value(s) inserted at the specified
#'         location.
#' 
#' @export 
#'
#' @examples
#' v <- 11:22
#' print(v)
#' stm_insert(v, insert_at = 3, values = 101:103)
#'
#' A <- matrix(dat = v, nrow = 3, byrow = TRUE)
#' print(A)
#' stm_insert(A, insert_at = 1, values = 101:104, axis = 0)
#'
#' A <- matrix(dat = 11:22, nrow = 3, byrow = TRUE)
#' print(A)
#' stm_insert(A, insert_at = 3, values = 101:103, axis = 1)
#'
stm_insert <- function(dat, insert_at = length(dat), values, axis = NULL) {
  dat <- check_vector_or_matrix(vmat = dat, par_name = "vmat")
  values <- check_vector(
    vec = values, allow_scalar= TRUE, par_name = "values"
  )
  if ("row" %in% axis) {
    axis <- 0
  } else if ("col" %in% axis) {
    axis <- 1
  } else {
    check_numeric(
      num = axis, min = 0, max = 1, is_integer = TRUE, allow_null = TRUE,
      par_name = "axis"
    )
  }
  dat <- as.matrix(dat)
  nrows <- nrow(dat)
  ncols <- ncol(dat)
  values <- as.matrix(values)
  # must be here
  max <- ifelse(is.null(axis) || axis == 0, length(dat), nrow(dat)) + 1
  check_numeric(
    num = insert_at, min = 1, max = max, is_integer = TRUE, allow_null = TRUE,
    par_name = "insert_at"
  )
  insert_at <- if (is.null(insert_at)) names(dat) else insert_at
  if (is.null(axis)) {
    dat <- as.vector(dat)
    values <- as.vector(values)
    if (insert_at <= 1) {
      y <- c(values, dat)
    } else if (insert_at > length(dat)) {
      y <- c(dat, values)
    } else {
      y <- append(x = dat, values = values, after = insert_at - 1)
    }
  } else {
    if (axis == 0) {
      if (ncol(values) == 1 & ncols != 1) {
        values <- t(values)
      }
      mcols <- ncol(values)
      if (ncols != mcols) {
        stop("Expected 'values' to have ", ncols, " columns, got --> ", mcols,
             " columns")
      }
      if (insert_at == 1) {
        y <- rbind(values, dat)
      } else if (insert_at < nrows + 1) {
        y <- rbind(dat[1:(insert_at - 1), ], values, dat[insert_at:nrows, ])
      } else {
        y <- rbind(dat, values)
      }
    } else {
      if (nrow(values) == 1 & nrows != 1) {
        values <- t(values)
      }
      mrows <- nrow(values)
      if (nrows != mrows) {
        stop("Expected 'values' to have ", nrows, " rows, got --> ", mrows,
             " rows")
      }
      if (insert_at == 1) {
        y <- cbind(values, dat)
      } else if (insert_at < ncols + 1) {
        y <- cbind(dat[, 1:(insert_at - 1)], values, dat[, insert_at:ncols])
      } else {
        y <- cbind(dat, values)
      }
    }
  }
  return(y)
}


#' @rdname insert
#' @param vec A vector of values.
#' @examples
#' stm_insert_vector(11:22, insert_at = 1, values = 101:104)
#' @export
stm_insert_vector <- function(vec, insert_at = length(vec), values) {
  stm_insert(dat = vec, insert_at = insert_at, values = values, axis = NULL)
}


#' @rdname insert
#' @param  mat A matrix of values.
#' @examples
#' A <- matrix(dat = 11:22, nrow = 3, byrow = TRUE)
#' stm_insert_rows(A, insert_at = 1, values = 101:104)
#'
#' @export
stm_insert_rows <- function(mat, insert_at = length(mat), values) {
  stm_insert(dat = mat, insert_at = insert_at, values = values, axis = 0)
}


#' @rdname insert
#' @param  mat A matrix of values.
#' @examples
#' A <- matrix(dat = 11:22, nrow = 3, byrow = TRUE)
#' stm_insert_cols(A, insert_at = 3, values = 101:103)
#' @export
stm_insert_cols <- function(mat, insert_at = length(mat), values) {
  stm_insert(dat = mat, insert_at = insert_at, values = values, axis = 1)
}
