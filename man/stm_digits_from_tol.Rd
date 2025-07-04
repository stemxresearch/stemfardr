% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stm_digits_from_tol.R
\name{stm_digits_from_tol}
\alias{stm_digits_from_tol}
\title{Determine number of decimal digits from tolerance}
\usage{
stm_digits_from_tol(vec, tol = NULL)
}
\arguments{
\item{vec}{A numeric vector from which to calculate the number of decimal 
digits. This vector is analyzed to determine the maximum number
of decimal places if \code{tol} is \code{NULL} or invalid.}

\item{tol}{Numeric tolerance used to determine the number of decimal digits.
           Default is \code{1e-16}, which corresponds to 16 decimal points.
           If \code{tol} is invalid (e.g., negative, non-numeric, 
           or \code{NULL}), the function will analyze the vector directly 
           to get the maximum number of decimal points.

This function is particularly useful in numerical computations where 
precise control over decimal representation is needed, such as in rounding 
or formatting operations.

If a tolerance value is provided, the number of decimal digits is determined 
as the ceiling of the negative logarithm (base 10) of the tolerance. 
If the tolerance is \code{NULL} or invalid, the function computes the 
maximum decimal places directly from the numeric vector.}
}
\value{
An integer representing the number of decimal digits required. This 
        value is based on the tolerance if provided, or on the vector's 
        maximum decimal places if not.
}
\description{
\code{stm_digits_from_tol()} calculates the number of decimal digits 
required either based on a specified numeric tolerance or by analyzing 
the maximum number of decimal places in a given numeric vector.
}
\examples{
vec1 <- c(1.234, 2.56, 3.4859)
print(vec1)
stm_digits_from_tol(vec1)

vec2 <- c(1.01, 2.001, 3.0001)
print(vec2)
stm_digits_from_tol(vec2, tol = 1e-4)

vec3 <- c(10, 20, 30)
print(vec3)
stm_digits_from_tol(vec3)

vec4 <- c(0.123456789, 0.987654321)
print(vec4)
stm_digits_from_tol(vec4, tol = 1e-10)

vec5 <- c(100, 200.5867, 300.000000)
print(vec5)
stm_digits_from_tol(vec5)

}
