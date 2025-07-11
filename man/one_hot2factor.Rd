% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess.R
\name{one_hot2factor}
\alias{one_hot2factor}
\title{Convert one-hot encoded matrix to factor}
\usage{
one_hot2factor(x, labels = colnames(x))
}
\arguments{
\item{x}{one-hot encoded matrix or data.frame.}

\item{labels}{Character vector of level names.}
}
\value{
A factor.
}
\description{
Convert one-hot encoded matrix to factor
}
\details{
If input has a single column, it will be converted to factor and
returned
}
\examples{
\dontrun{
x <- data.frame(matrix(F, 10, 3))
colnames(x) <- c("Dx1", "Dx2", "Dx3")
x$Dx1[1:3] <- x$Dx2[4:6] <- x$Dx3[7:10] <- T
one_hot2factor(x)
}

}
\author{
EDG
}
