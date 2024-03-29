% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/massGLAM.R
\name{massGLAM}
\alias{massGLAM}
\title{Mass-univariate GLM Analysis}
\usage{
massGLAM(
  x,
  y,
  scale.x = FALSE,
  scale.y = FALSE,
  mod = c("glm", "gam"),
  type = NULL,
  xnames = NULL,
  ynames = NULL,
  spline.index = NULL,
  gam.k = 6,
  save.mods = TRUE,
  print.plot = FALSE,
  include_anova_pvals = NA,
  verbose = TRUE,
  trace = 0,
  n.cores = 1
)
}
\arguments{
\item{x}{Matrix / data frame of features}

\item{y}{Matrix / data frame of outcomes}

\item{scale.x}{Logical: If TRUE, scale and center \code{x}}

\item{scale.y}{Logical: If TRUE, scale and center \code{y}}

\item{mod}{Character: "glm" or "gam".}

\item{type}{Character: "massx" or "massy". Default = NULL,
where if (NCOL(x) > NCOL(y)) "massx" else "massy"}

\item{xnames}{Character vector: names of \code{x} feature(s)}

\item{ynames}{Character vector: names of \code{y} feature(s)}

\item{spline.index}{Integer vector: indices of features to fit splines for.}

\item{gam.k}{Integer: The dimension of the spline basis.}

\item{save.mods}{Logical: If TRUE, save models. Default = TRUE}

\item{print.plot}{Logical: If TRUE, print plot. Default = FALSE (best to
choose which p-values you want to plot directly)}

\item{include_anova_pvals}{Logical: If TRUE, include ANOVA p-values,
generated by \code{glm2table} (internal function)}

\item{verbose}{Logical: If TRUE, print messages during run}

\item{trace}{Integer: If > 0, print more verbose output to console.}

\item{n.cores}{Integer: Number of cores to use. (Testing only, do not
change from 1)}
}
\description{
Run a mass-univariate analysis with either:
a) single outome (y) and multiple predictors (x), one at a time, with an
optional common set of covariates in each model - "massx"
b) multiple different outcomes (y) with a fixed set of predictors (x) - "massy"
Therefore, the term mass-univariate refers to looking at one variable of
interest (with potential covariates of no interest) at a time
}
\examples{
\dontrun{
# Common usage is "reversed":
# x: outcome of interest as first column, optional covariates
# in the other columns
# y: features whose association with x we want to study
set.seed(2022)
features <- rnormmat(500, 40)
outcome <- features[, 3] - features[, 5] + features[, 14] + rnorm(500)
massmod <- massGLAM(outcome, features)
plot(massmod)
plot(massmod, what = "coef")
plot(massmod, what = "volcano")
}

}
\author{
E.D. Gennatas
}
