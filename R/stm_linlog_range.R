stm_linlog_range <- function() {
  return(invisible(NULL))
}

#' Generate linearly spaced vector
#'
#' \code{stm_linspace()} generates a vector of linearly spaced values from 
#' \code{from} to \code{to}, with a specified number of points \code{n}.
#'
#' @param from A numeric value representing the start of the sequence.
#' @param to A numeric value representing the end of the sequence.
#' @param n Optional. A positive integer specifying the number of points in 
#'          the sequence. Default is 50.
#' @param digits Optional. An integer specifying the number of decimal places 
#'               to round the result. Default is \code{NULL}.
#'
#' @return A numeric vector of length \code{n} with linearly spaced values 
#'         from \code{from} to \code{to}.
#'
#' @details This function generates a sequence of values with equal spacing 
#' between them, which is useful for creating plots or numerical grids.
#' If \code{digits} is provided, the resulting vector will be rounded to 
#' the specified number of decimal places.
#'
#' @seealso \code{\link{stm_logspace}}, \code{\link{stm_arange}}, \code{\link{seq}}, \code{\link{seq_along}}, \code{\link{seq_len}}
#' 
#' @export
#' 
#' @examples
#' stm_linspace(from = 0, to = 1, n = 7)
#' stm_linspace(from = 0, to = 1, n = 7, digits = 4)
#'
stm_linspace <- function(from, to, n = 50, digits = NULL) {
  check_numeric(num = from, par_name = "from")
  check_numeric(num = to, par_name = "to")
  check_numeric(num = n, min = 1, is_integer = TRUE, par_name = "n")
  check_limits(from, to, lower_par_name = "from", upper_par_name = "to")
  y = seq(from = from, to = to, length.out = n)
  stm_round(dat = y, digits = digits)
}


#' Generate log-spaced vector
#'
#' \code{stm_logspace()} generates a vector of logarithmically spaced values 
#' from \code{from} to \code{to}, using a logarithmic scale with base 10.
#'
#' @param from A numeric value representing the start of the sequence in 
#'             logarithmic space.
#' @param to A numeric value representing the end of the sequence in 
#'           logarithmic space.
#' @param n Optional. A positive integer specifying the number of points in 
#'          the sequence. Default is 50.
#' @param digits Optional. An integer specifying the number of decimal places 
#'               to round the result. Default is \code{NULL}.
#'
#' @return A numeric vector of length \code{n} with logarithmically spaced 
#'         values from \code{10^from} to \code{10^to}.
#'
#' @details This function creates a sequence of values with logarithmic 
#' spacing, which is useful for visualizing data with a wide range or 
#' for numerical methods involving logarithms. If \code{digits} is provided, 
#' the resulting vector will be rounded to the specified number of decimal 
#' places.
#'
#' @seealso \code{\link{stm_linspace}}, \code{\link{stm_arange}}, \code{\link{seq}}, \code{\link{seq_along}}, \code{\link{seq_len}}
#' 
#' @export
#' 
#' @examples
#' stm_logspace(from = 1, to = 5, n = 7)
#' stm_logspace(from = 0, to = 1, n = 7, digits = 2)
#'
stm_logspace <- function(from, to, n = 50, digits = NULL) {
  y <- 10^stm_linspace(from = from, to = to, n = n)
  stm_round(dat = y, digits = digits)
}


#' Generate a sequence of values
#'
#' \code{stm_arange()} generates a sequence of values from \code{from} to 
#' \code{to} with a specified step size (\code{by}), optionally including 
#' the last value \code{to} if it is not already in the sequence.
#'
#' @param from Numeric value. The starting value of the sequence. Defaults to 
#'             \code{1} if \code{to} is provided and \code{from} is NULL.
#' @param to Numeric value. The end value of the sequence.
#' @param by Numeric value. The step size between consecutive values. 
#'           Default is \code{1}.
#' @param include_last Logical value. If \code{TRUE}, the sequence will 
#'                     include the last value \code{to} if it's not already 
#'                     included. Default is \code{FALSE}.
#' @param digits Optional. Number of decimal places to round the sequence.
#'
#' @return A numeric vector of values from \code{from} to \code{to}, 
#'         spaced by \code{by}.
#'
#' @details This function generates a sequence of values similar to 
#'          \code{seq()}, with the ability to include the last value 
#'          (\code{to}) if it is not already present in the sequence.
#'
#' @seealso \code{\link{stm_linspace}}, \code{\link{stm_logspace}}, \code{\link{seq}}, \code{\link{seq_along}}, \code{\link{seq_len}}
#' 
#' @export
#' 
#' @examples
#' stm_arange(from = 1, to = 10, by = 2)
#' stm_arange(to = 10)
#' stm_arange(from = 10) # sequence will start from 1 to 10
#' stm_arange(from = 1, to = 5, by = 0.25)
#' stm_arange(from = 1, to = 8, by = 2, include_last = TRUE)
#'
stm_arange <- function(from = NULL, to = NULL, by = 1, include_last = FALSE, 
                       digits = NULL) {
  if (!is.null(from) && is.null(to)) {
    to <- from
    from <- 1
  } else if (is.null(from) && !is.null(to)) {
    from <- 1
  } else if (is.null(to) && is.null(from)) {
    stop("Both 'from' and 'to' cannot be NULL.")
  }
  
  from <- check_numeric(num = from, par_name = "from")
  to <- check_numeric(num = to, par_name = "to")
  by <- check_numeric(num = by, par_name = "by")
  
  check_limits(from, to, lower_par_name = "from", upper_par_name = "to")
  
  y <- seq(from, to, by = by)
  
  y <- if (include_last && y[length(y)] != to) c(y, to) else y
  
  stm_round(dat = y, digits = digits)
}