% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.data.tree.rpart.R
\name{as.data.tree.rpart}
\alias{as.data.tree.rpart}
\title{Convert \code{rpart} rules to \code{data.tree} object}
\usage{
as.data.tree.rpart(object, verbose = FALSE)
}
\arguments{
\item{object}{\code{rpart} object}

\item{verbose}{Logical: If TRUE, print messages to console}
}
\value{
\code{data.tree} object
}
\description{
Convert an \code{rpart} object to a \code{data.tree} object, which can be plotted with
\link{dplot3_cart}
}
\author{
E.D. Gennatas
}
