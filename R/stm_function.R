#' Convert string expression to function
#'
#' \code{stm_function()} converts a function or string representation of an
#' expression into a valid R function. Checks variable compatibility and
#' provides warnings or errors as necessary.
#'
#' @param fexpr A function or string representing a mathematical expression. 
#'              Strings must represent valid mathematical expressions.
#' @param vars Optional. A character vector of expected variable names.
#'             Default is \code{NULL} meaning that variables will be assumed
#'             for multi-variable expressions.
#' @param nvars Optional. An integer specifying the expected number of 
#'              variables in the expression. Raises an error if the count
#'              differs. Default is \code{NULL}
#' @param par_name Optional. A string used in error and warning messages to 
#'                 refer to the parameter. Defaults is \code{"fexpr"}.
#'
#' @return If \code{fexpr} is a function, it is returned unchanged. If it is
#'         a string, a callable R function is constructed and returned.
#'
#' @details
#' The function checks whether \code{fexpr} is a valid function or expression.
#' If it is a string, variables are identified and a callable function is
#' constructed. If \code{nvars} is specified and does not match the variable
#' count, an error is raised. If \code{vars} is not specified and multiple
#' variables are found, a warning is issued and default names are assumed.
#'
#' @seealso \code{\link{stm_function}}, \code{\link{stm_fexpr_vars}}
#' 
#' @export
#' 
#' @examples
#' f <- stm_function(
#'   fexpr = "2 * x + sqrt(y) + cos(a) / pi", 
#'   vars = c("x", "y", "a")
#' )
#' f(3, 4, 5)
#' f(x = 3, y = 4, a = 5)
#' f(a = 3, x = 4, y = 5)
#'
stm_function <- function(fexpr, vars = NULL, nvars = NULL, par_name = 'fexpr') {
  if (is.function(fexpr)) {
    return(fexpr)
  } else {
    check_par_name(par_name)
    if (is.null(vars)) {
      fvars <- stm_fexpr_vars(fexpr)
    } else {
      fvars <- vars
    }
    nvars <- check_numeric(
      nvars, min = 0, is_integer = TRUE, allow_null = TRUE, par_name = "nvars"
    )
    m <- length(fvars)
    if (!is.null(vars) && !is.null(nvars) && !is.na(m) && nvars != m) {
      s <- check_singular_plural(n = nvars)
      stop("Expected '", par_name, "' to have ", nvars, " variable",s,
           " got --> ", m)
    }
    if (is.null(vars) && m != 1) {
      paste_vars <- paste0('"', paste(fvars, collapse = '", "'), '"')
      msg <- paste0("You have not specified the function variables with the ",
        "parameter 'vars'. The default variables c(", paste_vars, ") have been ",
        "assumed."
      )
      warning(msg, call. = FALSE)
    }
    ftn <- sprintf("function(%s) {%s}", paste(fvars, collapse = ", "), fexpr)
    eval(parse(text = ftn))
  }
}
