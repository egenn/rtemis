% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchcases.R
\name{matchcases}
\alias{matchcases}
\title{Match cases by covariates}
\usage{
matchcases(
  target,
  pool,
  n.matches = 1,
  target.id = NULL,
  pool.id = NULL,
  exactmatch.factors = TRUE,
  exactmatch.cols = NULL,
  distmatch.cols = NULL,
  norepeats = TRUE,
  ignore.na = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{target}{data.frame you are matching against}

\item{pool}{data.frame you are looking for matches from}

\item{target.id}{String: Column name in \code{target} that holds unique cases IDs. Default = NULL,
in which case integer case numbers will be used}

\item{pool.id}{String: Same as \code{target.id} for \code{pool}}

\item{exactmatch.factors}{Logical: If TRUE, selected cases will have to exactly match factors
available in \code{target}}

\item{exactmatch.cols}{String: Names of columns that should be matched exactly}

\item{distmatch.cols}{String: Names of columns that should be distance-matched}

\item{norepeats}{Logical: If TRUE, cases in \code{pool} can only be chosen once. Default = TRUE}

\item{ignore.na}{Logical: If TRUE, ignore NA values during exact matching. Default = FALSE.}

\item{verbose}{Logical: If TRUE, print messages to console. Default = TRUE}
}
\description{
Find one or more cases from a `pool` DataFrame that match cases in a target DataFrame.
Match exactly and/or by distance (sum of squared distance).
}
\examples{
set.seed(2021)
cases <- data.frame(PID = paste0("PID", seq(4)),
                    Sex = factor(c(1, 1, 0, 0)),
                    Handedness = factor(c(1, 1, 0, 1)),
                    Age = c(21, 27, 39, 24),
                    Var = c(.7, .8, .9, .6),
                    Varx = rnorm(4))
controls <- data.frame(CID = paste0("CID", seq(50)),
                       Sex = factor(sample(c(0, 1), 50, TRUE)),
                       Handedness = factor(sample(c(0, 1), 50, TRUE, c(.1, .9))),
                       Age = sample(16:42, 50, TRUE),
                       Var = rnorm(50),
                       Vary = rnorm(50))

mc <- matchcases(cases, controls, 2, "PID", "CID")
}
\author{
E.D. Gennatas
}
