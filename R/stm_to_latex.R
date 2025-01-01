#' Convert matrix or string to LaTeX format
#'
#' \code{stm_to_latex()} converts a matrix or string into LaTeX code for
#' rendering mathematical expressions. Supports optional alignment and 
#' decimal rounding.
#'
#' @param x The matrix, vector, or string to convert to LaTeX format.
#' @param align Optional. The alignment for matrix columns. Choices are 
#'              \code{"right", "center",} or \code{"left"}. Default is
#'              \code{"right"}.
#' @param symbol Optional. The symbol to wrap around the LaTeX expression. 
#'               Default is \code{"$"}.
#' @param name Optional. The name for the equation. If provided, it will 
#'              appear before the LaTeX code.
#' @param digits Optional. The number of digits to round the matrix elements 
#'               to. Default is \code{NULL} (no rounding).
#'
#' @return A LaTeX formatted string.
#'
#' @details
#' The function supports both converting a matrix and rendering a string 
#' as a LaTeX equation. It also allows custom alignment of matrix columns 
#' and rounding of matrix values to the specified number of digits.
#'
#' @seealso \code{\link[Ryacas]{tex}}
#'
#' @export
#'
#' @examples
#' stm_to_latex("x^2 + y^2 - z^2")
#' stm_to_latex(matrix(1:4, nrow = 2), align = "center", digits = 2)
#' stm_to_latex(matrix(1:4, nrow = 2), name = "A")
#'
stm_to_latex <- function(
    x, align = c("right", "center", "left"), symbol = "$", name = NULL,
    digits = NULL) {
  symbol <- ifelse(symbol %in% c("(", "["), paste0("\\", symbol), symbol)
  if (is.character(x) && length(x) == 1) {# string equation
    latex <- paste0("\\displaystyle ", Ryacas::tex(Ryacas::yac_symbol(x)))
    latex <- ifelse(
      is.null(name) || !is.character(name) || length(name) != 1,
      latex, paste0(name, " = ", latex)
    )
    return(paste0(symbol, latex, symbol))
  }

  tryCatch({
    A <- as.matrix(x)
    A <- if (is.null(digits)) A else round(A, digits = digits)
    success = TRUE
  }, error = function(e) {
    success = FALSE
  })
  if (success) {
    align <- match.arg(align)
    align_char <- paste(rep(substr(align, 1, 1), ncol(A)), collapse = "")
    latex <- paste0("\\left[\\begin{array}{", align_char, "} ")
    for (i in seq_len(nrow(A))) {
      latex <- paste0(latex, paste(A[i, ], collapse = " & "))
      latex <- ifelse(i < nrow(A), paste0(latex, " \\\\ "), latex)
    }
    latex <- paste0(latex, " \\end{array}\\right]")
    latex <- ifelse(is.null(name), latex, paste0(name, " = ", latex))
    latex <- paste0(symbol, latex, symbol)
    return(latex)
  } else {
    stop("Unable to convert x to LaTex code")
  }
}
