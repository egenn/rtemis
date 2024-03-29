% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_ops.R
\name{rnormmat}
\alias{rnormmat}
\title{Random Normal Matrix}
\usage{
rnormmat(
  nrow = 10,
  ncol = 10,
  mean = 0,
  sd = 1,
  return.df = FALSE,
  seed = NULL
)
}
\arguments{
\item{nrow}{Integer: Number of rows. Default = 10}

\item{ncol}{Integer: Number of columns. Default = 10}

\item{mean}{Float: Mean. Default = 0}

\item{sd}{Float: Standard deviation. Default = 1}

\item{return.df}{Logical: If TRUE, return data.frame, otherwise matrix. Default = TRUE}

\item{seed}{Integer: Set seed for \code{rnorm}. Default = NULL}
}
\description{
Create a matrix or data frame of defined dimensions, whose columns are random normal vectors
}
\author{
E.D. Gennatas
}
