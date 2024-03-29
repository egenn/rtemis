% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtROC.R
\name{rtROC}
\alias{rtROC}
\title{Build an ROC curve}
\usage{
rtROC(
  true.labels,
  predicted.probabilities,
  thresholds = NULL,
  plot = FALSE,
  theme = rtTheme,
  verbose = TRUE
)
}
\arguments{
\item{true.labels}{Factor with true labels}

\item{predicted.probabilities}{Numeric vector of predicted probabilities /
estimated score}

\item{thresholds}{Numeric vector of thresholds to consider}

\item{plot}{Logical: If TRUE, print plot}

\item{theme}{rtemis theme to use}

\item{verbose}{Logical: If TRUE, print messages to console}
}
\description{
Calculate the points of an ROC curve and the AUC
}
\details{
\code{true.labels} should be a factor (will be coerced to one) where the first level is the
"positive" case. \code{predicted.probabilities} should be a vector of floats {0, 1} where \verb{[0, .5)}
corresponds to the first level and \verb{[.5, 1]} corresponds to the second level.
predicted.probabilities
}
\author{
E.D. Gennatas
}
