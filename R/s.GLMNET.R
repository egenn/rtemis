# s.GLMNET.R
# ::rtemis::
# 2016-7 E.D. Gennatas lambdamd.org
# TODO: Consider fixing foldid

#' GLM with Elastic Net Regularization [C, R, S]
#'
#' Train an elastic net model
#'
#' \code{s.GLMNET} runs \code{glmnet::cv.glmnet} for each value of alpha, for each resample in
#' \code{grid.resample.rtset}.
#' Mean values for \code{min.lambda} and MSE (Regression) or Accuracy (Classification) are aggregated for each
#' alpha and resample combination
#'
#' \code{[gS]} Indicates tunable hyperparameters: If more than a single value is provided, grid search will be
#' automatically performed
#' Variable importance saved under \code{varImp} in the output R6 object is equal to the coefficients times the
#' variable standard deviation.
#'
#' @inheritParams s.GLM
#' @inheritParams s.CART
#' @param alpha [gS] Float [0, 1]: The elasticnet mixing parameter:
#'   \code{a = 0} is the ridge penalty, \code{a = 1} is the lasso penalty
#' @param lambda [gS] Float vector: Best left to NULL, \code{cv.glmnet} will compute its own lambda sequence
#' @param intercept Logical: If TRUE, include intercept in the model. Default = TRUE
#' @param res.summary.fn Function: Used to average resample runs. Default = \code{mean}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Interpretable models
#' @export

s.GLMNET <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     x.name = NULL, y.name = NULL,
                     grid.resample.rtset = rtset.resample("kfold", 5),
                     grid.search.type = c("exhaustive", "randomized"),
                     grid.randomized.p = .1,
                     intercept = TRUE,
                     nway.interactions = 0,
                     family = NULL,
                     alpha = seq(0, 1, .2),
                     lambda = NULL,
                     nlambda = 100,
                     which.cv.lambda = c("lambda.1se", "lambda.min"),
                     penalty.factor = NULL,
                     weights = NULL,
                     ipw = TRUE,
                     ipw.type = 2,
                     upsample = FALSE,
                     downsample = FALSE,
                     resample.seed = NULL,
                     res.summary.fn = mean,
                     save.grid.run = FALSE,
                     metric = NULL,
                     maximize = NULL,
                     .gs = FALSE,
                     save.gs.mod = FALSE,
                     n.cores = rtCores,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.theme"),
                     question = NULL,
                     rtclass = NULL,
                     verbose = TRUE,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ====
  if (missing(x)) {
    print(args(s.GLMNET))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "GLMNET"

  # Dependencies ====
  if (!depCheck("glmnet", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  if (missing(x)) {
    print(args(s.GLMNET))
    stop("x is missing")
  }
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.GLMNET))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  which.cv.lambda <- match.arg(which.cv.lambda)
  grid.search.type <- match.arg(grid.search.type)

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
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (is.null(.weights)) .weights <- rep(1, NROW(y))
  if (is.null(family)) {
    if (type == "Regression") {
      family <- "gaussian"
    } else if (type == "Classification") {
      family <- if (length(levels(y)) == 2) "binomial" else "multinomial"
    } else if (type == "Survival") {
      family <- "cox"
    }
  } else {
    if (family %in% c("binomial", "multinomial") && type != "Classification") {
      y  <- factor(y)
      if (!is.null(y.test)) y.test <- factor(y.test)
      type <- "Classification"
    }
  }
  # Cox does not have an intercept (it is part of the baseline hazard)
  if (type == "Survival") intercept <- FALSE
  if (verbose) dataSummary(x, y, x.test, y.test, type)

  if (!is.null(family) && family %in% c("binomial", "multinomial") && !is.factor(y))

  if (type == "Survival") {
    colnames(y) <- c("time", "status")
    if (!is.null(y.test)) colnames(y.test) <- c("time", "status")
  }
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Model matrix ====
  dat <- data.frame(x, y = y)
  if (nway.interactions > 0) {
    .formula <- as.formula(paste0("y ~ .^", nway.interactions))
    x <- model.matrix(.formula, dat)[, -1]
  } else {
    .formula <- as.formula("y ~ .")
    x <- model.matrix(.formula, dat)[, -1]
  }

  if (is.null(penalty.factor)) penalty.factor <- rep(1, NCOL(x))

  if (!is.null(x.test)) {
    # for model.matrix to work, add y if not provided
    y.test1 <- if (is.null(y.test)) sample(y, NROW(x.test)) else y.test
    dat.test <- data.frame(x.test, y = y.test1)
    x.test <- model.matrix(.formula, dat.test)[, -1]
  }

  # Grid Search ====
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
    } else if (type == "Regression") {
      metric <- "MSE"
    }
  }
  if (is.null(maximize)) {
    maximize <- if (type == "Classification") TRUE else FALSE
  }
  cv.lambda <- is.null(lambda)
  do.gs <- is.null(lambda) | length(alpha) > 1
  if (!.gs && do.gs) {
    gs <- gridSearchLearn(x, y,
                          mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(alpha = alpha,
                                             lambda = lambda),
                          fixed.params = list(.gs = TRUE,          # We are in gridSearch
                                              which.cv.lambda = which.cv.lambda),
                          search.type = grid.search.type,
                          randomized.p = grid.randomized.p,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          save.mod = save.gs.mod,
                          verbose = verbose,
                          n.cores = n.cores)
    alpha <- gs$best.tune$alpha
    lambda <- gs$best.tune$lambda
  } else {
    gs <- NULL
  }
  if (verbose) parameterSummary(alpha, lambda,
                                newline.pre = TRUE)

  # glmnet::cv.glmnet/glmnet ====
  if (.gs && cv.lambda) {
    mod <- glmnet::cv.glmnet(x,
                             if (family == "binomial") reverseLevels(y) else y,
                             family = family,
                             alpha = alpha,
                             lambda = lambda,
                             nlambda = nlambda,
                             weights = .weights,
                             intercept = intercept,
                             penalty.factor = penalty.factor, ...)
  } else {
    if (verbose) msg("Training elastic net model...", newline.pre = TRUE)
    mod <- glmnet::glmnet(x,
                          if (family == "binomial") reverseLevels(y) else y,
                          family = family,
                          alpha = alpha,
                          lambda = lambda,
                          nlambda = nlambda,
                          weights = .weights,
                          intercept = intercept,
                          penalty.factor = penalty.factor, ...)
  }

  # Fitted ====
  if (type == "Regression" | type == "Survival") {
    fitted <- as.numeric(predict(mod, newx = x))
    fitted.prob <- NULL
  } else {
    if (family == "binomial") {
      fitted.prob <- predict(mod, x, type = "response")[, 1]
      fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(fitted) <- levels(y)
    } else {
      fitted.prob <- predict(mod, x, type = "response")
      fitted <- factor(colnames(fitted.prob)[apply(fitted.prob, 1, which.max)],
                       levels = levels(y))
    }

  }

  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression" | type == "Survival") {
      predicted <- as.numeric(predict(mod, newx = x.test))
      predicted.prob <- NULL
    } else {
      if (family == "binomial") {
        predicted.prob <- predict(mod, x.test, type = "response")[, 1]
        predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
        levels(predicted) <- levels(y)
      } else {
        predicted.prob <- predict(mod, x.test, type = "response")
        predicted <- factor(colnames(predicted.prob)[apply(predicted.prob, 1, which.max)],
                         levels = levels(y))
      }

    }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ====
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 gridsearch = gs,
                 parameters = list(lambda = lambda, alpha = alpha),
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 fitted.prob = fitted.prob,
                 se.fit.bag = NULL,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = NULL,
                 error.test = error.test,
                 varimp = as.matrix(coef(mod))[-1, 1],
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

} # rtemis::s.GLMNET
