% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtMod.R
\name{rtModCV-methods}
\alias{rtModCV-methods}
\alias{plot.rtModCV}
\alias{summary.rtModCV}
\alias{predict.rtModCV}
\alias{describe.rtModCV}
\title{S3 methods for \code{rtModCV} class that differ from those of the \code{rtMod} superclass}
\usage{
\method{plot}{rtModCV}(x, ...)

\method{summary}{rtModCV}(object, ...)

\method{predict}{rtModCV}(
  object,
  newdata,
  which.repeat = 1,
  classification.output = c("prob", "class"),
  output = c("array", "avg"),
  ...
)

\method{describe}{rtModCV}(object, ...)
}
\arguments{
\item{x}{\code{rtModCV} object}

\item{...}{Not used}

\item{object}{\code{rtModCV} object}

\item{newdata}{Set of predictors to use}

\item{which.repeat}{Integer: Which repeat to use for prediction}

\item{classification.output}{Character: "prob" or "class" for classification models
If "class" and \code{output} is "avg", the mode of the predictions is returned.}

\item{output}{Character: "matrix" or "avg". Produce either a matrix with predictions
of each model in different columns, or the mean/mode of the predictions across models}

\item{n.cores}{Integer: Number of cores to use}
}
\description{
S3 methods for \code{rtModCV} class that differ from those of the \code{rtMod} superclass

\code{plot.rtModCV}: \code{plot} method for \code{rtModCV} object

\code{summary.rtModCV}: \code{summary} method for \code{rtModCV} object

\code{predict.rtModCV}: \code{predict} method for \code{rtModCV} object

\code{describe} method for \code{rtModCV} object
}
