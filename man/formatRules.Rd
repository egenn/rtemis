% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatRules.R
\name{formatRules}
\alias{formatRules}
\title{Format rules}
\usage{
formatRules(x, space.after.comma = FALSE, decimal.places = NULL)
}
\arguments{
\item{x}{Vector, string: Logical expressions}

\item{space.after.comma}{Logical: If TRUE, place spaces after commas. Default = false}

\item{decimal.places}{Integer: Limit all floats (numbers of the form 9.9) to this many
decimal places}
}
\description{
Converts R-executable logical expressions to a more human-friendly format
}
\author{
E.D. Gennatas
}
