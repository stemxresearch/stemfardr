% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stm_hessenberg.R
\name{stm_hessenberg}
\alias{stm_hessenberg}
\title{Compute Hessenberg matrix}
\usage{
stm_hessenberg(mat, calc_q = FALSE, digits = NULL)
}
\arguments{
\item{mat}{A numeric square matrix to be transformed into its Hessenberg 
form.}

\item{calc_q}{Optional. A logical indicating whether to return the orthogonal
matrix \( Q \) in addition to the Hessenberg matrix. Default is 
\code{FALSE}, meaning only the Hessenberg matrix is returned.}

\item{digits}{Optional. The number of digits to round the result to. If 
\code{NULL}, no rounding is performed.}
}
\value{
If \code{calc_q = FALSE}, a matrix representing the Hessenberg form 
        of \code{mat}. If \code{calc_q = TRUE}, a list containing two 
        elements: \code{H} (the Hessenberg matrix) and \code{Q} (the 
        orthogonal matrix \( Q \)).
}
\description{
\code{stm_hessenberg()} computes the Hessenberg form of a square matrix, 
optionally also returning the orthogonal matrix \( Q \) used in the 
transformation. This is useful in numerical linear algebra, particularly 
for simplifying matrix operations such as eigenvalue calculations.
}
\details{
The function uses the \code{pracma::hessenberg()} function to compute the 
Hessenberg form of a square matrix. Optionally, it also returns the 
orthogonal matrix used in the transformation, which is useful for certain 
numerical methods.
}
\examples{
A <- matrix(
  c(4, 1, 2, 2, 3, 1, 1, 0, 2), nrow = 3, byrow = TRUE
)
print(A)
stm_hessenberg(A)
stm_hessenberg(A, calc_q = TRUE)
stm_hessenberg(A, calc_q = TRUE, digits = 3)

}
