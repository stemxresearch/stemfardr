#' Create and evaluate a system of expressions with vector inputs
#'
#' \code{stm_fsystem()} converts a character vector of expressions into
#' a function that evaluates each expression using vectors of provided
#' arguments.
#'
#' @param fsystem A character vector where each element is a mathematical
#'                expression as a string, representing the equations.
#' @param vars Optional. A character vector specifying the variable names
#'             used in the expressions. Default is \code{NULL}, meaning
#'             that variables are automatically detected.
#'
#' @return A function that takes vectors of arguments and evaluates each
#'         expression in \code{fsystem} with the given values, returning
#'         a numeric vector of results.
#'
#' @examples
#' f <- c("2 * x + y", "x^2 + sqrt(y)")
#' f_list <- stm_fsystem(f, vars = c("x", "y"))
#' f_list(c(x = 4, y = 9), c(x = 12, y = 7))
#'
#' f <- c("a + b", "a^2 - b")
#' f_list <- stm_fsystem(f, vars = c("a", "b"))
#' f_list(c(a = 3, b = 2), c(a = 5, b = 1))
#'
#' @details
#' This function takes a system of mathematical expressions and converts
#' them into a callable R function. Each expression is evaluated with
#' corresponding input vectors. If the number of provided arguments does
#' not match the number of expressions, an error is raised.
#'
#' @seealso \code{\link{stm_function}}, \code{\link{stm_fexpr_vars}}
#' 
#' @export
stm_fsystem <- function(fsystem, vars = NULL) {
  check_vector(vec = fsystem, is_numeric = FALSE)
  if (!is.null(vars)) {
    check_vector(vec = vars, is_numeric = FALSE)
  }

  fsystem <- lapply(fsystem, stm_function, vars = vars)

  # Create a wrapper function to evaluate each function with corresponding
  # arguments
  return(function(...) {
    args_list <- list(...)
    nargs = length(args_list)
    nsystem = length(fsystem)
    if (nargs != nsystem) {
      stop("Expected the number of argument to match the number of ",
           "expressions in 'fsystem', got --> ", nargs, " and ", nsystem,
           " respectively")
    }

    # Convert each named vector into a list for compatibility with do.call
    args_list <- lapply(args_list, function(v) as.list(v))

    # Use mapply to evaluate each function with its corresponding argument set
    mapply(
      function(f, args) do.call(f, args), fsystem, args_list, SIMPLIFY = TRUE
    )
  })
}
