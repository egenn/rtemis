% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{resample}
\alias{resample}
\title{Resampling methods}
\usage{
resample(
  y,
  n.resamples = 10,
  resampler = c("strat.sub", "strat.boot", "kfold", "bootstrap", "loocv"),
  index = NULL,
  group = NULL,
  stratify.var = y,
  train.p = 0.75,
  strat.n.bins = 4,
  target.length = NROW(y),
  id.strat = NULL,
  rtset = NULL,
  seed = NULL,
  verbosity = TRUE
)
}
\arguments{
\item{y}{Vector or data.frame: Usually the outcome; \code{NROW(y)} defines sample size}

\item{n.resamples}{Integer: Number of training/testing sets required}

\item{resampler}{Character: Type of resampling to perform: "bootstrap", "kfold",
"strat.boot", "strat.sub".}

\item{index}{List where each element is a vector of training set indices. Use this for
manual/pre-defined train/test splits}

\item{group}{Integer, vector, length = \code{length(y)}: Integer vector, where numbers
define fold membership. e.g. for 10-fold on a dataset with 1000 cases, you could use
\code{group = rep(1:10, each = 100)}}

\item{stratify.var}{Numeric vector (optional): Variable used for stratification.}

\item{train.p}{Float (0, 1): Fraction of cases to assign to traininig set for
\code{resampler = "strat.sub"}}

\item{strat.n.bins}{Integer: Number of groups to use for stratification for
\code{resampler = "strat.sub" / "strat.boot"}}

\item{target.length}{Integer: Number of cases for training set for
\code{resampler = "strat.boot"}.}

\item{id.strat}{Vector of IDs which may be replicated: resampling should
force replicates of each ID to only appear in the training or testing.}

\item{rtset}{List: Output of an \link{setup.resample} (or named list with same
structure). NOTE: Overrides all other arguments. Default = NULL}

\item{seed}{Integer: (Optional) Set seed for random number generator, in order to make
output reproducible. See \code{?base::set.seed}}

\item{verbosity}{Logical: If TRUE, print messages to console}
}
\description{
Create resamples of your data, e.g. for model building or validation.
"bootstrap" gives the standard bootstrap, i.e. random sampling with replacement, using
\link{bootstrap}, "strat.sub" creates stratified subsamples using \link{strat.sub},
while "strat.boot" uses \link{strat.boot} which runs \link{strat.sub} and then
randomly duplicates some of the training cases to reach original length of input
(default) or length defined by \code{target.length}.
}
\details{
\code{resample} is used by multiple \pkg{rtemis} learners, \code{gridSearchLearn}, and
\link{train_cv}. Note that option 'kfold', which uses \link{kfold} results in resamples
of slightly different length for y of small length, so avoid all operations which rely
on equal-length vectors. For example, you can't place resamples in a data.frame, but
must use a list instead.
}
\examples{
y <- rnorm(200)
# 10-fold (stratified)
res <- resample(y, 10, "kfold")
# 25 stratified subsamples
res <- resample(y, 25, "strat.sub")
# 100 stratified bootstraps
res <- resample(y, 100, "strat.boot")
}
\seealso{
\link{train_cv}
}
\author{
E.D. Gennatas
}
