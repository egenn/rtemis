% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_S7_Resampler.R
\name{setup_Resampler}
\alias{setup_Resampler}
\title{Setup Resampler}
\usage{
setup_Resampler(
  n_resamples = 10L,
  type = c("KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV"),
  stratify_var = NULL,
  train_p = 0.75,
  strat_n_bins = 4L,
  target_length = NULL,
  id_strat = NULL,
  seed = NULL,
  verbosity = 1L
)
}
\arguments{
\item{n_resamples}{Integer: Number of resamples to make.}

\item{type}{Character: Type of resampler: "KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV"}

\item{stratify_var}{Character: Variable to stratify by.}

\item{train_p}{Float: Training set percentage.}

\item{strat_n_bins}{Integer: Number of bins to stratify by.}

\item{target_length}{Integer: Target length for stratified bootstraps.}

\item{id_strat}{Integer: Vector of indices to stratify by. These may be, for example, case IDs
if your dataset contains repeated measurements. By specifying this vector, you can ensure that
each case can only be present in the training or test set, but not both.}

\item{seed}{Integer: Random seed.}

\item{verbosity}{Integer: Verbosity level.}
}
\value{
ResamplerParameters object.
}
\description{
Setup Resampler
}
\author{
EDG
}
