# s.GLM.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Generalized Linear Model [C, R]
#'
#' Train a Generalized Linear Model for Regression or Classification (i.e. Logistic Regression) using \code{stats::glm}.
#' If outcome \code{y} has more than two classes, Multinomial Logistic Regression is performed using
#' \code{nnet::multinom}
#'
#' A common problem with \code{glm} arises when the testing set containts a predictor with more
#' levels than those in the same predictor in the training set, resulting in error. This can happen
#' when training on resamples of a data set, especially after stratifying against a different
#' outcome, and results in error and no prediction. \code{s.GLM} automatically finds such cases
#' and substitutes levels present in \code{x.test} and not in \code{x} with NA.
#' Variable importance saved under \code{varImp} in the output R6 object is equal to the coefficients times the
#' variable standard deviation.
#'
#' @param x Numeric vector or matrix / data frame of features i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test Numeric vector or matrix / data frame of testing set features
#'   Columns must correspond to columns in \code{x}
#' @param y.test Numeric vector of testing set outcome
#' @param family Error distribution and link function. See \code{stats::family}
#' @param covariate Character: Name of column to be included as interaction term in formula, must be factor
#' @param x.name Character: Name for feature set
#' @param y.name Character: Name for outcome
#' @param interactions Logical: If TRUE, include all pairwise interactions. \code{formula = y ~.*.}
#' @param nway.interactions Integer: Include n-way interactions. This integer defined the n: \code{formula = y ~^n}
#' @param class.method Character: Define "logistic" or "multinom" for classification. The only purpose
#' of this is so you can try \code{nnet::multinom} instead of glm for binary classification
#' @param weights Numeric vector: Weights for cases. For classification, \code{weights} takes precedence
#' over \code{ipw}, therefore set \code{weights = NULL} if using \code{ipw}.
#' Note: If \code{weight} are provided, \code{ipw} is not used. Leave NULL if setting \code{ipw = TRUE}. Default = NULL
#' @param ipw Logical: If TRUE, apply inverse probability weighting (for Classification only).
#' Note: If \code{weights} are provided, \code{ipw} is not used. Default = TRUE
#' @param ipw.type Integer {0, 1, 2}
#" 0: class.weights = 1 / (class.frequencies/sum(class.frequencies))
#' 1: class.weights as in 0, divided by max(class.weights)
#' 2: class.weights as in 0, divided by min(class.weights)
#' Default = 2
#' @param upsample Logical: If TRUE, upsample cases to balance outcome classes (for Classification only)
#' Note: upsample will randomly sample with replacement if the length of the majority class is more than double
#' the length of the class you are upsampling, thereby introducing randomness
#' @param downsample Logical: If TRUE, downsample majority class to match size of minority class
#' @param resample.seed Integer: If provided, will be used to set the seed during upsampling.
#' Default = NULL (random seed)
#' @param intercept Logical: If TRUE, fit an intercept term. Default = TRUE
#' @param polynomial Logical: if TRUE, run lm on \code{poly(x, poly.d)} (creates orthogonal polynomials)
#' @param poly.d Integer: degree of polynomial. Default = 3
#' @param poly.raw Logical: if TRUE, use raw polynomials.
#'   Default, which should not really be changed is FALSE
#' @param print.plot Logical: if TRUE, produce plot using \code{mplot3}
#'   Takes precedence over \code{plot.fitted} and \code{plot.predicted}. Default = TRUE
#' @param plot.fitted Logical: if TRUE, plot True (y) vs Fitted
#' @param plot.predicted Logical: if TRUE, plot True (y.test) vs Predicted.
#'   Requires \code{x.test} and \code{y.test}
#' @param plot.theme Character: "zero", "dark", "box", "darkbox"
#' @param na.action How to handle missing values. See \code{?na.fail}
#' @param removeMissingLevels Logical: If TRUE, finds factors in \code{x.test} that contain levels
#' not present in \code{x} and substitutes with NA. This would result in error otherwise and no
#' predictions would be made, ending \code{s.GLM} prematurely
#' @param question Character: the question you are attempting to answer with this model, in plain language.
#' @param rtclass Character: Class type to use. "S3", "S4", "RC", "R6"
#' @param verbose Logical: If TRUE, print summary to screen.
#' @param trace Integer: If higher than 0, will print more information to the console. Default = 0
#' @param outdir Path to output directory.
#'   If defined, will save Predicted vs. True plot, if available,
#'   as well as full model output, if \code{save.mod} is TRUE
#' @param save.mod Logical: If TRUE, save all output to an RDS file in \code{outdir}
#'   \code{save.mod} is TRUE by default if an \code{outdir} is defined. If set to TRUE, and no \code{outdir}
#'   is defined, outdir defaults to \code{paste0("./s.", mod.name)}
#' @param ... Additional arguments
#' @return \link{rtMod}
#' @examples
#' x <- rnorm(100)
#' y <- .6 * x + 12 + rnorm(100)/2
#' mod <- s.GLM(x, y)
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Interpretable models
#' @export

s.GLM <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  family = NULL,
                  interactions = FALSE,
                  nway.interactions = 0,
                  covariate = NULL,
                  class.method = NULL,
                  weights = NULL,
                  ipw = TRUE,
                  ipw.type = 2,
                  upsample = FALSE,
                  downsample = FALSE,
                  resample.seed = NULL,
                  intercept = TRUE,
                  polynomial = FALSE,
                  poly.d = 3,
                  poly.raw = FALSE,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  na.action = na.exclude,
                  removeMissingLevels = TRUE,
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  trace = 0,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ====
  if (missing(x)) {
    print(args(s.GLM))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)

  # Arguments ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.GLM))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (trace > 0) verbose <- TRUE

  # Data ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
                    ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type

  if (!is.null(family) && family %in% c("binomial", "multinomial") && type != "Classification") {
    y  <- factor(y)
    if (!is.null(y.test)) y.test <- factor(y.test)
    type <- "Classification"
  }

  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (is.null(.weights)) .weights <- rep(1, NROW(y))
  if (verbose) dataSummary(x, y, x.test, y.test, type)

  if (type == "Regression") {
    mod.name <- if (polynomial) "POLY" else "GLM"
    if (is.null(family)) family <- gaussian()
  } else {
    if (is.null(class.method)) {
      mod.name <- if (length(levels(y)) > 2) "MULTINOM" else "LOGISTIC"
      if (is.null(family) & mod.name == "LOGISTIC") family <- binomial()
    } else {
      mod.name <- toupper(class.method)
    }
  }
  checkType(type, c("Classification", "Regression"), mod.name)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Formula ====
  # do not use data.frame() here; x already data.frame from dataPrepare.
  # If colnames was integers, data.frame() would add 'X' in front of those.
  # For example, splines produces output with integers as colnames.
  df.train <- cbind(x, y = if (mod.name == "LOGISTIC") reverseLevels(y) else y)
  if (!polynomial) {
    if (nway.interactions > 0) {
      .formula <- paste0(y.name, " ~ .^", nway.interactions)
    } else if (interactions) {
      .formula <- paste(y.name, "~ .*.")
    } else if (!is.null(covariate)) {
      features <- xnames[!grepl(covariate, xnames)]
      .formula <- paste(y.name, "~", paste(features, "*", covariate, collapse = " + "))
    } else {
      .formula <- paste(y.name, "~ .")
    }
  } else {
    # polynomial
    if (!is.null(covariate)) {
      warning("Covariate not supported with polynomial - will be ignored.
              You can pass squares and cubes manually")
    }
    features <- paste0("poly(", paste0(xnames, ", degree = ", poly.d, ", raw = ", poly.raw, ")",
                                       collapse = " + poly("))
    .formula <- paste0(y.name, " ~ ", features)
    }

  # Intercept
  if (!intercept) .formula <- paste(.formula, "- 1")
  .formula <- as.formula(.formula)

  # glm, nnet ====
  if (trace > 0) msg("Using formula", .formula)
  if (mod.name != "MULTINOM") {
      if (verbose) msg("Training GLM...", newline.pre = TRUE)
      mod <- glm(.formula, family = family, data = df.train,
                 weights = .weights, na.action = na.action, ...)
  } else {
    if (!depCheck("nnet", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    if (verbose) msg("Training multinomial logistic regression model...", newline.pre = TRUE)
    mod <- nnet::multinom(.formula, data = df.train,
                          weights = .weights, na.action = na.action, ...)
  }
  if (trace > 0) print(summary(mod))

  # Fitted ====
  fitted.prob <- se.fit <- NULL
  if (type == "Regression") {
      fitted <- predict(mod, x, se.fit = TRUE)
      se.fit <- as.numeric(fitted$se.fit)
      fitted <- as.numeric(fitted$fit)
  } else {
    if (mod.name == "LOGISTIC") {
      fitted.prob <- as.numeric(predict(mod, x, type = "response"))
      fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(fitted) <- levels(y)
    } else {
      fitted.prob <- predict(mod, x, type = "probs")
      fitted <- predict(mod, x, type = "class")
    }
  }

  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  predicted <- se.prediction <- error.test <- predicted.prob <- NULL
  if (!is.null(x.test)) {

    # Remove missing levels ====
    if (removeMissingLevels) {
      index.factor <- which(sapply(x, is.factor))
      if (length(index.factor) > 0) {
        levels.training <- lapply(x[, index.factor], function(x) levels(droplevels(x)))
        levels.testing <- lapply(x.test[, index.factor], levels)
        # Get index of levels present in test set and not in training
        index.missing <- lapply(seq_len(levels.training), function(i) levels.testing[[i]] %in% levels.training[[i]])
        # Set levels present in testing but missing in training to NA
        which.missing <- sapply(index.missing, all)
        if (any(!which.missing)) {
          if (verbose) msg("Levels present in testing and not in training replaced with NA")
          for (i in which(!which.missing)) {
            missing.level <- levels.testing[[i]][!index.missing[[i]]]
            index.extralevel <- x.test[, index.factor][, i] == missing.level
            x.test[, index.factor][, i][index.extralevel] <- NA
          }
        }
      }
    }

    se.prediction <- predicted.prob <- NULL
    if (type == "Regression") {
        predicted <- predict(mod, x.test, se.fit = TRUE)
        se.prediction <- predicted$se.fit
        predicted <- as.numeric(predicted$fit)
    } else {
      if (mod.name == "LOGISTIC") {
        predicted.prob <- predict(mod, x.test, type = "response")
        predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
        levels(predicted) <- levels(y)
      } else {
        predicted.prob <- as.numeric(predict(mod, x.test, type = "probs"))
        predicted <- predict(mod, x.test, type = "class")
      }
    }

    if (!is.null(y.test) && length(y.test) > 1) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ====
  extra <- list(formula = .formula,
                weights = .weights,
                polynomia = polynomial)
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
                 fitted.prob = fitted.prob,
                 se.fit = se.fit,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = se.prediction,
                 error.test = error.test,
                 # varimp = mod$coefficients[-1] * apply(x, 2, sd), #adjust for categorical with 3+ levels
                 varimp = mod$coefficients[-1],
                 question = question,
                 extra = extra)

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

} # rtemis::s.GLM


#' Logistic Regression
#'
#' Convenience alias for \code{s.GLM(family = binomial(link = "logit"))}.
#' @inheritParams s.GLM
#' @export
s.LOGISTIC <- function(x, y, x.test = NULL, y.test = NULL,
                       family = binomial(link = "logit"), ...) {

  s.GLM(x, y, x.test = x.test, y.test = y.test, family = family, ...)

} # rtemis::s.LOGISTIC


#' Multinomial Logistic Regression
#'
#' Convenience alias for \code{s.GLM(class.method = "multinom")}.
#' @inheritParams s.GLM
#' @export
s.MULTINOM <- function(x, y, x.test = NULL, y.test = NULL,
                       class.method = "multinom", ...) {

  s.GLM(x, y, x.test = x.test, y.test = y.test, class.method = class.method, ...)

} # rtemis::s.MULTINOM


#' Polynomial Regression
#'
#' Convenience alias for \code{s.LM(polynomial = T)}.
#'   Substitutes all features with \code{poly(x, poly.d)}
#' @inheritParams s.GLM
#' @param poly.d Integer: degree of polynomial(s) to use
#' @param poly.raw Logical: if TRUE, use raw polynomials. Defaults to FALSE, resulting in
#'   orthogonal polynomials. See \code{stats::poly}
#' @export

s.POLY <- function(x, y, x.test = NULL, y.test = NULL, poly.d = 3, poly.raw = FALSE, ...) {

  s.GLM(x, y, x.test = x.test, y.test = y.test,
        polynomial = TRUE, poly.d = poly.d, poly.raw = poly.raw, ...)

} # rtemis::s.POLY

#'
#' #' Generalized Least Squares
#' #'
#' #' Convenience alias for \code{s.GLM(gls = TRUE)}. Uses \code{nlme::gls}
#' #'
#' #' GLS can be useful in place of a standard linear model, when there is correlation among
#' #'   the residuals
#' #' @inheritParams s.GLM
#' #' @param ... Additional parameters to be passed to \code{nlme::gls}
#' #' @export
#'
#' s.GLS <- function(x, y = NULL, x.test = NULL, y.test = NULL, ...) {
#'
#'   s.GLM(x, y, x.test = x.test, y.test = y.test, gls = TRUE, ...)
#'
#' } # rtemis::s.GLS
