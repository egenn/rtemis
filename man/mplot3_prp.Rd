% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplot3_prp.R
\name{mplot3_prp}
\alias{mplot3_prp}
\title{Plot CART Decision Tree}
\usage{
mplot3_prp(
  object,
  type = 0,
  extra = "auto",
  branch.lty = 1,
  under = FALSE,
  fallen.leaves = TRUE,
  palette = NULL,
  filename = NULL,
  pdf.width = 7,
  pdf.height = 5,
  ...
)
}
\arguments{
\item{object}{Output of \link{s_CART}}

\item{palette}{Color vector}
}
\description{
Plot output of a regression or classification tree created using \code{rpart}
A wrapper for \code{rpart.plot::rpart.plot}
}
