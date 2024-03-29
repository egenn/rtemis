% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s_LIHADBoost.R
\name{s_LIHADBoost}
\alias{s_LIHADBoost}
\title{Boosting of Linear Hard Additive Trees [R]}
\usage{
s_LIHADBoost(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  resid = NULL,
  boost.obj = NULL,
  learning.rate = 0.5,
  case.p = 1,
  max.depth = 5,
  gamma = 0.1,
  alpha = 0,
  lambda = 1,
  lambda.seq = NULL,
  minobsinnode = 2,
  minobsinnode.lin = 10,
  shrinkage = 1,
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  part.minbucket = 5,
  lin.type = c("glmnet", "cv.glmnet", "lm.ridge", "allSubsets", "forwardStepwise",
    "backwardStepwise", "glm", "sgd", "solve", "none"),
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = "lambda.min",
  max.iter = 10,
  tune.n.iter = TRUE,
  earlystop.params = setup.earlystop(),
  lookback = TRUE,
  init = NULL,
  .gs = FALSE,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  metric = NULL,
  maximize = NULL,
  cxrcoef = FALSE,
  print.progress.every = 5,
  print.error.plot = "final",
  x.name = NULL,
  y.name = NULL,
  question = NULL,
  base.verbose = FALSE,
  verbose = TRUE,
  grid.verbose = FALSE,
  trace = 0,
  prefix = NULL,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  print.plot = FALSE,
  print.base.plot = FALSE,
  print.tune.plot = TRUE,
  plot.type = "l",
  save.gridrun = FALSE,
  outdir = NULL,
  n.cores = rtCores,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
)
}
\arguments{
\item{x}{Numeric vector or matrix / data frame of features i.e. independent variables}

\item{y}{Numeric vector of outcome, i.e. dependent variable}

\item{x.test}{Numeric vector or matrix / data frame of testing set features
Columns must correspond to columns in \code{x}}

\item{y.test}{Numeric vector of testing set outcome}

\item{learning.rate}{Float (0, 1] Learning rate for the additive steps}

\item{max.iter}{Integer: Maximum number of iterations (additive steps) to perform. Default = 10}

\item{init}{Float: Initial value for prediction. Default = mean(y)}

\item{print.error.plot}{String or Integer: "final" plots a training and validation (if available) error curve at the
end of training. If integer, plot training and validation error curve every this many iterations
during training}

\item{x.name}{Character: Name for feature set}

\item{y.name}{Character: Name for outcome}

\item{question}{Character: the question you are attempting to answer with this model, in plain language.}

\item{base.verbose}{Logical: \code{verbose} argument passed to learner}

\item{verbose}{Logical: If TRUE, print summary to screen.}

\item{trace}{Integer: If > 0, print diagnostic info to console}

\item{plot.fitted}{Logical: if TRUE, plot True (y) vs Fitted}

\item{plot.predicted}{Logical: if TRUE, plot True (y.test) vs Predicted.
Requires \code{x.test} and \code{y.test}}

\item{plot.theme}{Character: "zero", "dark", "box", "darkbox"}

\item{print.plot}{Logical: if TRUE, produce plot using \code{mplot3}
Takes precedence over \code{plot.fitted} and \code{plot.predicted}.}

\item{print.base.plot}{Logical: Passed to \code{print.plot} argument of base learner, i.e. if TRUE, print error plot
for each base learner}

\item{outdir}{Path to output directory.
If defined, will save Predicted vs. True plot, if available,
as well as full model output, if \code{save.mod} is TRUE}

\item{save.mod}{Logical: If TRUE, save all output to an RDS file in \code{outdir}
\code{save.mod} is TRUE by default if an \code{outdir} is defined. If set to TRUE, and no \code{outdir}
is defined, outdir defaults to \code{paste0("./s.", mod.name)}}

\item{...}{Additional parameters to be passed to learner}
}
\description{
Boost a Linear Hard Additive Tree (i.e. LIHAD, i.e. LINAD with hard splits)
}
\details{
By default, early stopping works by checking training loss.
}
\author{
E.D. Gennatas
}
