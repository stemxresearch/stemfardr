% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stm_tranform_stack.R
\name{stm_stack_rows}
\alias{stm_stack_rows}
\title{Stack rows together into a data frame or matrix}
\usage{
stm_stack_rows(...)
}
\arguments{
\item{...}{Data frames, matrices, or vectors to be combined as rows.}
}
\value{
A data frame or matrix resulting from row-wise binding of the 
        input data.
}
\description{
\code{stm_stack_rows()} combines multiple data frames or vectors by
row binding them together using \code{rbind()}.
}
\details{
This function stacks the provided data frames, matrices, or vectors as rows
using the \code{rbind()} function. The resulting object is returned as a data
frame or matrix.
}
\examples{
A <- matrix(1:4, ncol = 2, byrow = TRUE)
print(A)
B <- matrix(5:8, ncol = 2, byrow = TRUE)
print(B)
stm_stack_rows(A, B)

}
\seealso{
\code{\link{cbind}}, \code{\link{rbind}}, \code{\link{stm_stack_cols}}
}
