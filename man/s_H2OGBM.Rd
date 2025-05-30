% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s_H2OGBM.R
\name{s_H2OGBM}
\alias{s_H2OGBM}
\title{Gradient Boosting Machine on H2O (C, R)}
\usage{
s_H2OGBM(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  ip = "localhost",
  port = 54321,
  h2o.init = TRUE,
  gs.h2o.init = FALSE,
  h2o.shutdown.at.end = TRUE,
  grid.resample.params = setup.resample("kfold", 5),
  metric = NULL,
  maximize = NULL,
  n.trees = 10000,
  force.n.trees = NULL,
  max.depth = 5,
  n.stopping.rounds = 50,
  stopping.metric = "AUTO",
  p.col.sample = 1,
  p.row.sample = 0.9,
  minobsinnode = 5,
  min.split.improvement = 1e-05,
  quantile.alpha = 0.5,
  learning.rate = 0.01,
  learning.rate.annealing = 1,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  na.action = na.fail,
  grid.n.cores = 1,
  n.cores = rtCores,
  imetrics = FALSE,
  .gs = FALSE,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  grid.verbose = verbose,
  save.mod = FALSE,
  outdir = NULL,
  ...
)
}
\arguments{
\item{x}{Numeric vector or matrix / data frame of features i.e. independent variables}

\item{y}{Numeric vector of outcome, i.e. dependent variable}

\item{x.test}{Numeric vector or matrix / data frame of testing set features
Columns must correspond to columns in \code{x}}

\item{y.test}{Numeric vector of testing set outcome}

\item{x.name}{Character: Name for feature set}

\item{y.name}{Character: Name for outcome}

\item{ip}{Character: IP address of H2O server. Default = "localhost"}

\item{port}{Integer: Port number for server. Default = 54321}

\item{h2o.shutdown.at.end}{Logical: If TRUE, run \code{h2o.shutdown(prompt = FALSE)} after
training is complete.}

\item{n.trees}{Integer: Number of trees to grow. Maximum number of trees if \code{n.stopping.rounds > 0}}

\item{max.depth}{[gS] Integer: Depth of trees to grow}

\item{n.stopping.rounds}{Integer: If > 0, stop training if \code{stopping.metric} does not improve for this
many rounds}

\item{stopping.metric}{Character: "AUTO" (Default), "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE",
"AUC", "lift_top_group", "misclassification", "mean_per_class_error"}

\item{p.col.sample}{[gS]}

\item{p.row.sample}{[gS]}

\item{minobsinnode}{[gS]}

\item{learning.rate}{[gS]}

\item{learning.rate.annealing}{[gS]}

\item{weights}{Numeric vector: Weights for cases. For classification, \code{weights} takes precedence
over \code{ifw}, therefore set \code{weights = NULL} if using \code{ifw}.
Note: If \code{weight} are provided, \code{ifw} is not used. Leave NULL if setting \code{ifw = TRUE}.}

\item{ifw}{Logical: If TRUE, apply inverse frequency weighting
(for Classification only).
Note: If \code{weights} are provided, \code{ifw} is not used.}

\item{ifw.type}{Integer {0, 1, 2}
1: class.weights as in 0, divided by min(class.weights)
2: class.weights as in 0, divided by max(class.weights)}

\item{upsample}{Logical: If TRUE, upsample cases to balance outcome classes (for Classification only)
Note: upsample will randomly sample with replacement if the length of the majority class is more than double
the length of the class you are upsampling, thereby introducing randomness}

\item{downsample}{Logical: If TRUE, downsample majority class to match size of minority class}

\item{resample.seed}{Integer: If provided, will be used to set the seed during upsampling.
Default = NULL (random seed)}

\item{na.action}{How to handle missing values. See \code{?na.fail}}

\item{n.cores}{Integer: Number of cores to use}

\item{.gs}{Internal use only}

\item{print.plot}{Logical: if TRUE, produce plot using \code{mplot3}
Takes precedence over \code{plot.fitted} and \code{plot.predicted}.}

\item{plot.fitted}{Logical: if TRUE, plot True (y) vs Fitted}

\item{plot.predicted}{Logical: if TRUE, plot True (y.test) vs Predicted.
Requires \code{x.test} and \code{y.test}}

\item{plot.theme}{Character: "zero", "dark", "box", "darkbox"}

\item{question}{Character: the question you are attempting to answer with this model, in plain language.}

\item{verbose}{Logical: If TRUE, print summary to screen.}

\item{trace}{Integer: If higher than 0, will print more information to the console.}

\item{save.mod}{Logical: If TRUE, save all output to an RDS file in \code{outdir}
\code{save.mod} is TRUE by default if an \code{outdir} is defined. If set to TRUE, and no \code{outdir}
is defined, outdir defaults to \code{paste0("./s.", mod.name)}}

\item{outdir}{Path to output directory.
If defined, will save Predicted vs. True plot, if available,
as well as full model output, if \code{save.mod} is TRUE}

\item{...}{Additional arguments}
}
\value{
\code{rtMod} object
}
\description{
Trains a Gradient Boosting Machine using H2O (http://www.h2o.ai)
}
\details{
[gS] denotes tunable hyperparameters
Warning: If you get an HTTP 500 error at random, use \code{h2o.shutdown()} to shutdown the server.
It will be restarted when \code{s_H2OGBM} is called
}
\seealso{
\link{train_cv} for external cross-validation

Other Supervised Learning: 
\code{\link{s_AdaBoost}()},
\code{\link{s_AddTree}()},
\code{\link{s_BART}()},
\code{\link{s_BRUTO}()},
\code{\link{s_BayesGLM}()},
\code{\link{s_C50}()},
\code{\link{s_CART}()},
\code{\link{s_CTree}()},
\code{\link{s_EVTree}()},
\code{\link{s_GAM}()},
\code{\link{s_GBM}()},
\code{\link{s_GLM}()},
\code{\link{s_GLMNET}()},
\code{\link{s_GLMTree}()},
\code{\link{s_GLS}()},
\code{\link{s_H2ODL}()},
\code{\link{s_H2ORF}()},
\code{\link{s_HAL}()},
\code{\link{s_Isotonic}()},
\code{\link{s_KNN}()},
\code{\link{s_LDA}()},
\code{\link{s_LM}()},
\code{\link{s_LMTree}()},
\code{\link{s_LightCART}()},
\code{\link{s_LightGBM}()},
\code{\link{s_MARS}()},
\code{\link{s_MLRF}()},
\code{\link{s_NBayes}()},
\code{\link{s_NLA}()},
\code{\link{s_NLS}()},
\code{\link{s_NW}()},
\code{\link{s_PPR}()},
\code{\link{s_PolyMARS}()},
\code{\link{s_QDA}()},
\code{\link{s_QRNN}()},
\code{\link{s_RF}()},
\code{\link{s_RFSRC}()},
\code{\link{s_Ranger}()},
\code{\link{s_SDA}()},
\code{\link{s_SGD}()},
\code{\link{s_SPLS}()},
\code{\link{s_SVM}()},
\code{\link{s_TFN}()},
\code{\link{s_XGBoost}()},
\code{\link{s_XRF}()}

Other Tree-based methods: 
\code{\link{s_AdaBoost}()},
\code{\link{s_AddTree}()},
\code{\link{s_BART}()},
\code{\link{s_C50}()},
\code{\link{s_CART}()},
\code{\link{s_CTree}()},
\code{\link{s_EVTree}()},
\code{\link{s_GBM}()},
\code{\link{s_GLMTree}()},
\code{\link{s_H2ORF}()},
\code{\link{s_LMTree}()},
\code{\link{s_LightCART}()},
\code{\link{s_LightGBM}()},
\code{\link{s_MLRF}()},
\code{\link{s_RF}()},
\code{\link{s_RFSRC}()},
\code{\link{s_Ranger}()},
\code{\link{s_XGBoost}()},
\code{\link{s_XRF}()}
}
\author{
E.D. Gennatas
}
\concept{Supervised Learning}
\concept{Tree-based methods}
