% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stm_round.R
\name{stm_round}
\alias{stm_round}
\title{Round numeric data to specified digits}
\usage{
stm_round(dat, digits = NULL)
}
\arguments{
\item{dat}{A numeric vector, list, or matrix to round.}

\item{digits}{Optional. The number of decimal places to round to. If not 
provided or invalid, the original data is returned.}
}
\value{
The rounded data.
}
\description{
\code{stm_round()} rounds numeric data to a specified number of digits.
If the input is a list, the rounding is applied element-wise to each 
numeric element.
}
\details{
The function applies rounding to numeric data. If \code{dat} is a list, 
it will apply rounding element-wise to each numeric element in the list. 
If an error occurs during rounding, the original data is returned.
}
\examples{
stm_round(3.14159, digits = 2)
stm_round(3.14159, digits = "two")
a <- list(3.14159, "hello", 2.71828, list(1.61803, "world"))
stm_round(a, digits = 3)

}
