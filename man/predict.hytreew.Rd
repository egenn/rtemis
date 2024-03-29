% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hytreew.R
\name{predict.hytreew}
\alias{predict.hytreew}
\title{Predict method for \code{hytreew} object}
\usage{
\method{predict}{hytreew}(
  object,
  newdata,
  n.feat = NCOL(newdata),
  fixed.cxr = NULL,
  cxr.newdata = NULL,
  cxr = FALSE,
  cxrcoef = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{object}{\code{hytreew}}

\item{newdata}{Data frame of predictors}

\item{n.feat}{(internal use) Integer: Use first \code{n.feat} columns of newdata to predict.
Defaults to all}

\item{fixed.cxr}{(internal use) Matrix: Cases by rules to use instead of matching cases to rules using
\code{newdata}}

\item{cxr.newdata}{(internal use) Data frame: Use these values to match cases by rules}

\item{cxr}{Logical: If TRUE, return list which includes cases-by-rules matrix along with predicted values}

\item{cxrcoef}{Logical: If TRUE, return cases-by-rules * coefficients matrix along with predicted values}

\item{verbose}{Logical: If TRUE, print messages to console}

\item{...}{Not used}
}
\description{
Predict method for \code{hytreew} object
}
\author{
E.D. Gennatas
}
