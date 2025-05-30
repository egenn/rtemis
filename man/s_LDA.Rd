% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s_LDA.R
\name{s_LDA}
\alias{s_LDA}
\title{Linear Discriminant Analysis}
\usage{
s_LDA(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  prior = NULL,
  method = "moment",
  nu = NULL,
  upsample = TRUE,
  downsample = FALSE,
  resample.seed = NULL,
  x.name = NULL,
  y.name = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  outdir = NULL,
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

\item{prior}{Numeric: Prior probabilities of class membership}

\item{method}{"moment" for standard estimators of the mean and variance, "mle" for
MLEs, "mve" to use cov.mve, or "t" for robust estimates based on a t distribution}

\item{nu}{Integer: Degrees of freedom for method = "t"}

\item{upsample}{Logical: If TRUE, upsample cases to balance outcome classes (for Classification only)
Note: upsample will randomly sample with replacement if the length of the majority class is more than double
the length of the class you are upsampling, thereby introducing randomness}

\item{downsample}{Logical: If TRUE, downsample majority class to match size of minority class}

\item{resample.seed}{Integer: If provided, will be used to set the seed during upsampling.
Default = NULL (random seed)}

\item{x.name}{Character: Name for feature set}

\item{y.name}{Character: Name for outcome}

\item{print.plot}{Logical: if TRUE, produce plot using \code{mplot3}
Takes precedence over \code{plot.fitted} and \code{plot.predicted}.}

\item{plot.fitted}{Logical: if TRUE, plot True (y) vs Fitted}

\item{plot.predicted}{Logical: if TRUE, plot True (y.test) vs Predicted.
Requires \code{x.test} and \code{y.test}}

\item{plot.theme}{Character: "zero", "dark", "box", "darkbox"}

\item{question}{Character: the question you are attempting to answer with this model, in plain language.}

\item{verbose}{Logical: If TRUE, print summary to screen.}

\item{outdir}{Path to output directory.
If defined, will save Predicted vs. True plot, if available,
as well as full model output, if \code{save.mod} is TRUE}

\item{save.mod}{Logical: If TRUE, save all output to an RDS file in \code{outdir}
\code{save.mod} is TRUE by default if an \code{outdir} is defined. If set to TRUE, and no \code{outdir}
is defined, outdir defaults to \code{paste0("./s.", mod.name)}}

\item{...}{Additional arguments passed to \code{MASS::lda}}
}
\value{
\code{rtMod} object
}
\description{
Train an LDA Classifier using \code{MASS::lda}
}
\details{
Note: LDA requires all predictors to be numeric.
The variable importance output ("varimp") is the vector of coefficients for LD1
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
\code{\link{s_H2OGBM}()},
\code{\link{s_H2ORF}()},
\code{\link{s_HAL}()},
\code{\link{s_Isotonic}()},
\code{\link{s_KNN}()},
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
}
\author{
E.D. Gennatas
}
\concept{Supervised Learning}
