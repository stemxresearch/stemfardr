#' Extract variable names from a mathematical expression
#'
#' \code{stm_fexpr_vars()} identifies and extracts unique variable names from 
#' a string representation of a mathematical expression. Mathematical functions 
#' such as \code{sqrt}, \code{log}, and others are excluded from the output.
#'
#' @param fexpr A character string representing a mathematical expression. 
#'              It should be a valid expression with variables and functions.
#'
#' @return A character vector of unique variable names found in the expression.
#'         Mathematical functions are excluded from the result.
#' 
#' @seealso \code{\link{stm_function}}, \code{\link{stm_fexpr_vars}}
#' 
#' @export
#'
#' @examples
#' expr <- "x^2 + y * sqrt(z)"
#' stm_fexpr_vars(expr)
#'
#' expr <- "log(a) + sin(b) + 3 * pi"
#' stm_fexpr_vars(expr)
#'
stm_fexpr_vars <- function(fexpr) {
  math_functions <- c(
    "sqrt", "cos", "sin", "tan", "sec", "csc", "cot", "acos", "asin", "atan",
    "cosh", "sinh", "tanh", "acosh", "asinh", "atanh", "exp", "log", "log10",
    "pi"
  )
  variables <- unique(
    # extract variable names using regular expressions (these are strings)
    unlist(stringr::str_extract_all(fexpr, "\\b[a-zA-Z]+\\b"))
  )
  setdiff(variables, math_functions)
}
