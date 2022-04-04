# s_LM.R
# ::rtemis::
# 2015 E.D. Gennatas lambdamd.org

#' Linear model
#'
#' Fit a linear model and validate it. Options include base \code{lm()}, robust linear model using
#'   \code{MASS:rlm()}, generalized least squares using \code{nlme::gls}, or polynomial regression
#'   using \code{stats::poly} to transform features
#'
#' GLS can be useful in place of a standard linear model, when there is correlation among
#'   the residuals and/or they have unequal variances.
#'   Warning: \code{nlme}'s implementation is buggy, and \code{predict} will not work
#'   because of environment problems, which means it fails to get predicted values if
#'   \code{x.test} is provided.
#'   \code{robut = TRUE} trains a robust linear model using \code{MASS::rlm}.
#'   \code{gls = TRUE} trains a generalized least squares model using \code{nlme::gls}.
#'
#' @inheritParams s_GLM
#' @param robust Logical: if TRUE, use \code{MASS::rlm()} instead of base \code{lm()}
#' @param gls Logical: if TRUE, use \code{nlme::gls}
#' @param polynomial Logical: if TRUE, run lm on \code{poly(x, poly.d)} (creates orthogonal polynomials)
#' @param poly.d Integer: degree of polynomial
#' @param poly.raw Logical: if TRUE, use raw polynomials.
#'   Default, which should not really be changed is FALSE
#' @param plot.fitted Logical: if TRUE, plot True (y) vs Fitted
#' @param plot.predicted Logical: if TRUE, plot True (y.test) vs Predicted.
#'   Requires \code{x.test} and \code{y.test}
#' @param plot.theme Character: "zero", "dark", "box", "darkbox"
#' @param na.action How to handle missing values. See \code{?na.fail}
#' @param question Character: the question you are attempting to answer with this model, in plain language.
#' @param rtclass Character: Class type to use. "S3", "S4", "RC", "R6"
#' @param verbose Logical: If TRUE, print summary to screen.
#' @param outdir Path to output directory.
#'   If defined, will save Predicted vs. True plot, if available,
#'   as well as full model output, if \code{save.mod} is TRUE
#' @param save.mod Logical. If TRUE, save all output as RDS file in \code{outdir}
#'   \code{save.mod} is TRUE by default if an \code{outdir} is defined. If set to TRUE, and no \code{outdir}
#'   is defined, outdir defaults to \code{paste0("./s.", mod.name)}
#' @param ... Additional arguments to be passed to \code{MASS::rlm} if \code{robust = TRUE}
#'   or \code{MASS::lm.gls} if \code{gls = TRUE}
#' @return \link{rtMod}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @examples
#' x <- rnorm(100)
#' y <- .6 * x + 12 + rnorm(100)/2
#' mod <- s_LM(x, y)
#' @export

s_LM <- function(x, y = NULL,
                 x.test = NULL, y.test = NULL,
                 x.name = NULL, y.name = NULL,
                 weights = NULL,
                 ipw = TRUE,
                 ipw.type = 2,
                 upsample = FALSE,
                 downsample = FALSE,
                 resample.seed = NULL,
                 intercept = TRUE,
                 robust = FALSE,
                 gls = FALSE,
                 polynomial = FALSE,
                 poly.d = 3,
                 poly.raw = FALSE,
                 print.plot = TRUE,
                 plot.fitted = NULL,
                 plot.predicted = NULL,
                 plot.theme = getOption("rt.theme"),
                 na.action = na.exclude,
                 question = NULL,
                 rtclass = NULL,
                 verbose = TRUE,
                 trace = 0,
                 outdir = NULL,
                 save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ====
  if (missing(x)) { print(args(s_LM)); return(invisible(9)) }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  if (robust) {
    mod.name <- "RLM"
  } else if (gls) {
    mod.name <- "GLS"
  } else if (polynomial) {
    mod.name <- "POLY"
  } else {
    mod.name <- "LM"
  }

  # Dependencies ====
  if (robust) {
    dependency_check("MASS")
  }
  if (gls) {
    dependency_check("nlme")
  }

  # Arguments ====
  if (is.null(y) & NCOL(x) < 2) { print(args(s_LM)); stop("y is missing") }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (sum(c(robust, gls, polynomial)) > 1) {
    stop("Can only specify one of 'robust', 'gls', or 'polynomial'")
  }
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
                    ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  # .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Formula ====
  df.train <- data.frame(y = y, x)
  if (!polynomial) {
    features <- paste(xnames, collapse = " + ")
    formula.str <- paste0(y.name, " ~ ", features)
  } else {
    features <- paste0("poly(", paste0(xnames, ", degree = ", poly.d, ", raw = ", poly.raw, ")",
                                         collapse = " + poly("))
    formula.str <- paste0(y.name, " ~ ", features)
  }
  # Intercept
  if (!intercept) formula.str <- paste(formula.str, "- 1")
  myformula <- as.formula(formula.str)

  # LM & POLY ====
    if (!robust & !gls) {
      if (verbose) msg("Training linear model...", newline.pre = TRUE)
      mod <- lm(myformula, data = df.train,
                weights = weights,
                na.action = na.action, ...)
    }
    # RLM
    if (robust) {
      if (verbose) msg("Training robust linear model...", newline.pre = TRUE)
      mod <- MASS::rlm(myformula, data = df.train,
                       weights = weights,
                       na.action = na.action, ...)
    }
    # GLS
    if (gls) {
      if (verbose) msg("Training generalized least squares...", newline.pre = TRUE)
      mod <- nlme::gls(myformula, data = df.train,
                       weights = weights,
                       na.action = na.action, ...)
    }

    if (trace > 0) print(summary(mod))

  # Fitted ====
    if (!gls) {
      fitted <- predict(mod, x, se.fit = TRUE)
      se.fit <- as.numeric(fitted$se.fit)
      fitted <- as.numeric(fitted$fit)
    } else {
      se.fit <- NULL
      fitted <- as.numeric(predict(mod, x))
    }

  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
      if (gls) {
        assign('myformula', myformula) # why need this? nlme is buggy?
        predicted <- as.numeric(predict(mod, x.test))
      } else {
        predicted <- predict(mod, x.test, se.fit = TRUE)
        se.prediction <- predicted$se.fit
        predicted <- as.numeric(predicted$fit)
      }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ====
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 se.fit = se.fit,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = se.prediction,
                 varimp = mod$coefficients[-1],
                 error.test = error.test,
                 question = question)

  rtMod.out(rt,
            print.plot,
            plot.fitted,
            plot.predicted,
            y.test,
            mod.name,
            outdir,
            save.mod,
            verbose,
            plot.theme)

  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::s_LM


#' Robust linear model
#'
#' Convenience alias for \code{s_LM(robust = T)}. Uses \code{MASS::rlm}
#'
#' @inheritParams s_GLM
#' @param ... Additional parameters to be passed to \code{MASS::rlm}
#' @export

s_RLM <- function(x, y, x.test = NULL, y.test = NULL, ...) {

  s_LM(x, y, x.test = x.test, y.test = y.test, robust = TRUE, ...)

} # rtemis::s_RLM
