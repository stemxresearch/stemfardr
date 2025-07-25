% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stm_charpoly.R
\name{stm_charpoly}
\alias{stm_charpoly}
\title{Compute the characteristic polynomial of a matrix}
\usage{
stm_charpoly(x, equation = FALSE, variable = "x", digits = NULL)
}
\arguments{
\item{x}{A square matrix for which the characteristic polynomial will be 
computed.}

\item{equation}{Logical. If \code{TRUE}, the polynomial is returned as a 
string equation. Default is \code{FALSE}.}

\item{variable}{A string specifying the variable to be used in the polynomial. 
Default is \code{"x"}.}

\item{digits}{Optional. An integer specifying the number of decimal places 
to round the result. Default is \code{NULL}, which means no 
rounding.}
}
\value{
A numeric vector of polynomial coefficients or a string representation 
        of the polynomial if \code{equation = TRUE}.
}
\description{
\code{stm_charpoly()} calculates the characteristic polynomial of a square 
matrix and optionally returns the polynomial as an equation.
}
\details{
The function calculates the characteristic polynomial of a square matrix 
using \code{pracma::charpoly()}. If \code{equation = TRUE}, the coefficients 
are formatted as a polynomial equation.
}
\examples{
A <- matrix(c(4, 1, 2, 3), nrow = 2)
stm_charpoly(A)
stm_charpoly(A, equation = TRUE)
stm_charpoly(A, equation = TRUE, variable = "t")

}
\seealso{
\code{\link[pracma]{charpoly}}
}
