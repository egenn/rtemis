% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/super_ops.R
\name{prob2categorical}
\alias{prob2categorical}
\title{Convert probabilities to categorical (factor)}
\usage{
prob2categorical(x, levels, binclasspos = NULL)
}
\arguments{
\item{x}{Numeric vector: Probabilities}

\item{levels}{Character vector: Class labels}

\item{binclasspos}{Integer: Index of the positive class for binary classification}
}
\value{
Factor
}
\description{
Convert probabilities to categorical (factor)
}
\examples{
\dontrun{
# Binary classification where "A" is the positive class, so .1 => B, .5 & .9 => A
prob2categorical(c(.1, .5, .9), c("A", "B"), 1)
# Binary classification where "B" is the positive class, so .1 => A, .5 & .9 => B
prob2categorical(c(.1, .5, .9), c("A", "B"), 2)
# Multi-class classification
prob <- matrix(c(.1, .3, .6, .05, .6, .35, .4, .3, .3), nrow = 3, byrow = TRUE)
prob2categorical(prob, c("A", "B", "C"))
}
}
\author{
EDG
}
