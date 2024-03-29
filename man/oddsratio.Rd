% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oddsratio.R
\name{oddsratio}
\alias{oddsratio}
\title{Calculate odds ratio for a 2x2 contingency table}
\usage{
oddsratio(x, verbose = TRUE)
}
\arguments{
\item{x}{2x2 contingency table (created with \code{table(x, y)}, where \code{x} and \code{y}
are factors with the first level being the control / unaffected / negative)}

\item{verbose}{Logical: If TRUE, print messages to console}
}
\description{
Calculate odds ratio for a 2x2 contingency table
}
