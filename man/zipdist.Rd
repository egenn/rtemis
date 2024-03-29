% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zip_ops.R
\name{zipdist}
\alias{zipdist}
\title{Get distance between pairs of zip codes}
\usage{
zipdist(x, y, zipdt)
}
\arguments{
\item{x}{Character vector}

\item{y}{Character vector, same length as \code{x}}

\item{zipdt}{data.table with columns \code{zip}, \code{lng}, \code{lat}}
}
\value{
\code{data.table} with distances in meters
}
\description{
Get distance between pairs of zip codes
}
\author{
E.D. Gennatas
}
