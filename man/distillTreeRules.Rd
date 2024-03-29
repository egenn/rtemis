% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distillTreeRules.R
\name{distillTreeRules}
\alias{distillTreeRules}
\title{Distill rules from trained RF and GBM learners}
\usage{
distillTreeRules(
  mod,
  x,
  y = NULL,
  n.trees = NULL,
  maxdepth = 100,
  maxDecay = 0.05,
  typeDecay = 2,
  verbose = TRUE
)
}
\arguments{
\item{mod}{A trained RF or GBM model}

\item{x}{The training set features}

\item{y}{The training set outcomes. If NULL, assumed to be last column of \code{x}}

\item{n.trees}{Integer: Number of trees to extract}

\item{maxdepth}{Integer: Max depth to consider}

\item{maxDecay}{Float: See \code{inTree=es::pruneRule}}

\item{typeDecay}{Integer: See \code{inTreees::pruneRule}}

\item{verbose}{Logical: If TRUE, print messages to output}
}
\description{
Extract rules from RF or GBM model, prune, and remove unnecessary rules using \code{inTrees}
}
\details{
Models must be trained with \link{s_RF} or \link{s_GBM}
}
\author{
E.D. Gennatas
}
