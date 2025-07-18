% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{size}
\alias{size}
\title{Size of matrix or vector}
\usage{
size(x, verbosity = 1L)
}
\arguments{
\item{x}{Vector or matrix input}

\item{verbosity}{Integer: Verbosity level. If > 0, print size to console}
}
\value{
Integer vector of length 2: c(Nrow, Ncols), invisibly
}
\description{
Return the size of a matrix or vector as (Nrows, Ncolumns)
Are you tired of getting NULL when you run dim() on a vector?
}
\examples{
\dontrun{
x <- rnorm(20)
size(x)
# 20  1
x <- matrix(rnorm(100), 20, 5)
size(x)
# 20  5
}
}
\author{
EDG
}
