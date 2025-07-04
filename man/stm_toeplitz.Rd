% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stm_toeplitz.R
\name{stm_toeplitz}
\alias{stm_toeplitz}
\title{Create a Toeplitz matrix}
\usage{
stm_toeplitz(vec1, vec2 = vec1)
}
\arguments{
\item{vec1}{A numeric vector to form the first column of the matrix.}

\item{vec2}{A numeric vector to form the first row of the matrix. If 
not specified, defaults to \code{vec1}.}
}
\value{
A matrix of size \code{length(vec1)} x \code{length(vec2)} 
        with the structure of a Toeplitz matrix.
}
\description{
\code{stm_toeplitz()} generates a Toeplitz matrix from two vectors. A 
Toeplitz matrix is a matrix where each descending diagonal from left to 
right is constant.
}
\details{
The function checks that the two vectors have the same length. 
         It then constructs a matrix where the first column is \code{vec1} 
         and the first row is \code{vec2}. Subsequent rows are filled 
         following the Toeplitz matrix structure.
}
\examples{
x <- c(1, 2, 3)
y <- c(1, 5, 6, 7)
stm_toeplitz(x, y)

}
