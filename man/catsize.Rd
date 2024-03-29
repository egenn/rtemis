% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_ops.R
\name{catsize}
\alias{catsize}
\title{Print Size}
\usage{
catsize(x, name = NULL, verbose = TRUE, newline = TRUE)
}
\arguments{
\item{x}{R object (usually that inherits from matrix or data.frame)}

\item{name}{Character: Name of input object}

\item{verbose}{Logical: If TRUE, print NROW and NCOL to console.}

\item{newline}{Logical: If TRUE, end with new line character.}
}
\value{
vector of NROW, NCOL invisibly
}
\description{
Get \code{NCOL(x)} and \code{NROW{x}}
}
\examples{
catsize(iris)
}
\author{
E.D. Gennatas
}
