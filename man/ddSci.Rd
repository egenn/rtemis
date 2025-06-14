% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddSci.R
\name{ddSci}
\alias{ddSci}
\title{Format Numbers for Printing}
\usage{
ddSci(x, decimal_places = 2, hi = 1e+06, as_numeric = FALSE)
}
\arguments{
\item{x}{Vector of numbers}

\item{decimal_places}{Integer: Return this many decimal places.}

\item{hi}{Float: Threshold at or above which scientific notation is used.}

\item{as_numeric}{Logical: If TRUE, convert to numeric before returning.
This will not force all numbers to print 2 decimal places. For example:
1.2035 becomes "1.20" if \code{as_numeric = FALSE}, but 1.2 otherwise
This can be helpful if you want to be able to use the output as numbers / not just for printing.}
}
\value{
Formatted number
}
\description{
2 Decimal places, otherwise scientific notation
}
\details{
Numbers will be formatted to 2 decimal places, unless this results in 0.00 (e.g. if input was .0032),
in which case they will be converted to scientific notation with 2 significant figures.
\code{ddSci} will return \code{0.00} if the input is exactly zero.
This function can be used to format numbers in plots, on the console, in logs, etc.
}
\examples{
\dontrun{
x <- .34876549
ddSci(x)
# "0.35"
x <- .00000000457823
ddSci(x)
# "4.6e-09"
}
}
\author{
EDG
}
