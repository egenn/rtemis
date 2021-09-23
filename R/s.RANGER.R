# s.RANGER.R
# ::rtemis::
# 2016-8 E.D. Gennatas lambdamd.org
# TODO: Add Survival support
# TODO: Use inbag and resample for stratified bootstraps

#' Random Forest Classification and Regression [C, R]
#'
#' Train a Random Forest for regression or classification using \code{ranger}
#'
#' You should cconsider, or try, setting mtry to NCOL(x), especially for small number of features.
#' By default mtry is set to NCOL(x) for NCOL(x) <= 20.
#' For imbalanced datasets, setting stratify.on.y = TRUE should improve performance.
#' If \code{autotune = TRUE}, \code{randomForest::tuneRF} will be run to determine best \code{mtry}
#'   value.
#' [gS]: indicated parameter will be tuned by grid search if more than one value is passed
#'
#' See \href{https://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf}{Tech Report} comparing
#' balanced (ipw.case.weights = TRUE) and weighted (ipw.class.weights = TRUE) Random Forests.
#'
#'
#' @inheritParams s.RF
#' @inheritParams s.CART
#' @param n.trees Integer: Number of trees to grow. Default = 1000
#' @param mtry [gS] Integer: Number of features sampled randomly at each split. Defaults to square root of n of
#' features for classification, and a third of n of features for regression.
#' @param min.node.size [gS] Integer: Minimum node size
#' @param splitrule Character: For classification: "gini" (Default) or "extratrees";
#' For regression: "variance" (Default), "extratrees" or "maxstat".
#' For survival "logrank" (Default), "extratrees", "C" or "maxstat".
#' @param ipw.case.weights Logical: If TRUE, define ranger's \code{case.weights} using IPW. Default = TRUE
#' Note: Cannot use case.weights together with \code{stratify.on.y} or \code{inbag.resample}
#' @param ipw.class.weights Logical: If TRUE, define ranger's \code{class.weights} using IPW. Default = FALSE
#' @param probability Logical: If TRUE, grow a probability forest. See \code{ranger::ranger}. Default = FALSE
#' @param classwt Vector, Float: Priors of the classes for \code{randomForest::tuneRF} if \code{autotune = TRUE}.
#' For classification only; need not add up to 1
#' @param inbag.resample List, length \code{n.tree}: Output of \link{rtset.resample} to define resamples used for each
#' tree. Default = NULL
#' @param stratify.on.y Logical: If TRUE, overrides \code{inbag.resample} to use stratified bootstraps for each tree.
#' This can help improve test set performance in imbalanced datasets. Default = FALSE. Note: Cannot be used with
#' \code{ipw.case.weights}
#' @param ... Additional arguments to be passed to \code{ranger::ranger}
#' @return \link{rtMod} object
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Ensembles
#' @export

s.RANGER <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     x.name = NULL, y.name = NULL,
                     n.trees = 1000,
                     weights = NULL,
                     ipw = TRUE,
                     ipw.type = 2,
                     ipw.case.weights = TRUE,
                     ipw.class.weights = FALSE,
                     upsample = FALSE,
                     downsample = FALSE,
                     resample.seed = NULL,
                     autotune = FALSE,
                     classwt = NULL,
                     n.trees.try = 500,
                     stepFactor = 2,
                     mtry = NULL,
                     mtryStart = NULL,
                     inbag.resample = NULL,
                     stratify.on.y = FALSE,
                     grid.resample.rtset = rtset.resample("kfold", 5),
                     grid.search.type = c("exhaustive", "randomized"),
                     grid.randomized.p = .1,
                     metric = NULL,
                     maximize = NULL,
                     probability = FALSE,
                     importance = "impurity",
                     replace = TRUE,
                     min.node.size = NULL,
                     splitrule = NULL,
                     strata = NULL,
                     sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                     tune.do.trace = FALSE,
                     imetrics = FALSE,
                     n.cores = rtCores,
                     print.tune.plot = FALSE,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.fit.theme", "lightgrid"),
                     question = NULL,
                     grid.verbose = TRUE,
                     verbose = TRUE,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ====
  if (missing(x)) {
    print(args(s.RANGER))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "RANGER"

  # Dependencies ====
  if (!depCheck("ranger", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.RANGER))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
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
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry, newline.pre = TRUE)
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  .class.weights <- if (is.null(classwt) & ipw) dt$class.weights else classwt
  x0 <- if (upsample | downsample) dt$x0 else x
  y0 <- if (upsample | downsample) dt$y0 else y
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  n.features <- NCOL(x)
  if (is.null(mtry)) {
    if (n.features <= 20) mtry <- n.features
    mtry <- if (type == "Classification") floor(sqrt(n.features)) else max(floor(n.features/3), 1)
  }
  if (type == "Classification") nlevels <- length(levels(y))

  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Regression") {
      metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    }
  }

  if (is.null(maximize)) {
    maximize <- if (type == "Classification") TRUE else FALSE
  }

  # Formula ====
  df.train <- data.frame(x, y)
  colnames(df.train)[ncol(df.train)] <- y.name
  .formula <- as.formula(paste(y.name, "~ ."))

  # Grid Search ====
  if (gridCheck(mtry, min.node.size)) {
    gs <- gridSearchLearn(x0, y0,
                          mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(mtry = mtry,
                                             min.node.size = min.node.size),
                          fixed.params = list(n.trees = n.trees,
                                              replace = replace,
                                              importance = "none",
                                              ipw = ipw,
                                              ipw.type = ipw.type,
                                              upsample = upsample,
                                              resample.seed = resample.seed),
                          search.type = grid.search.type,
                          randomized.p = grid.randomized.p,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          verbose = grid.verbose,
                          n.cores = 1)
    mtry <- gs$best.tune$mtry
    min.node.size <- gs$best.tune$min.node.size
  } else {
    gs <- NULL
  }

  # In case tuning fails, use defaults
  if (length(mtry) == 0) {
    warning("Tuning failed; setting mtry to default")
    mtry <- if (type == "Classification") floor(sqrt(n.features)) else max(floor(n.features/3), 1)
  }

  if (length(min.node.size) == 0) {
    min.node.size <- if (type == "Classification") 1 else 5
  }

  # tuneRF ====
  if (is.null(mtryStart)) mtryStart <- sqrt(n.features)
  if (autotune) {
    if (verbose) msg("Tuning for mtry...")
    tuner <- try(randomForest::tuneRF(x = x, y = y,
                                      mtryStart = mtryStart,
                                      ntreeTry = n.trees.try,
                                      stepFactor = stepFactor,
                                      trace = verbose,
                                      plot = print.tune.plot,
                                      strata = strata,
                                      do.trace = tune.do.trace,
                                      classwt = .class.weights))
    if (!inherits(tuner, "try-error")) {
      mtry <- tuner[which.min(tuner[, 2]), 1]
      if (verbose) msg("Best mtry :", mtry)
    } else {
      msg("tuneRF failed; reverting to mtry =", mtry)
    }
  }
  parameters <- list(n.trees = n.trees,
                     mtry = mtry,
                     ipw = ipw,
                     ipw.type = ipw.type,
                     upsample = upsample,
                     resample.seed = resample.seed)

  # Ranger ====
  if (stratify.on.y) {
    inbag.resample <- rtset.resample("strat.boot", n.trees)
  }
  if (!is.null(inbag.resample)) {
    if (verbose) msg("Creating custom subsamples...")
    # Get integer index of inbag cases
    inbag.res <- resample(df.train$y, rtset = inbag.resample, verbose = verbose)
    # Convert to counts for each case
    inbag <- lapply(seq(n.trees), function(i) sapply(seq(df.train$y), function(j) sum(j == inbag.res[[i]])))
  } else {
    inbag <- NULL
  }
  if (verbose) msg("Training Random Forest (ranger)", type, "with", n.trees, "trees...", newline.pre = TRUE)
  mod <- ranger::ranger(formula = .formula,
                        data = df.train,
                        num.trees = n.trees,
                        case.weights = if (ipw.case.weights) .weights else NULL,
                        class.weights = if (ipw.case.weights) .class.weights else NULL,
                        mtry = mtry,
                        min.node.size = min.node.size,
                        splitrule = splitrule,
                        replace = replace,
                        probability = probability,
                        importance = importance,
                        write.forest = TRUE,
                        inbag = inbag,
                        num.threads = n.cores,
                        verbose = verbose, ...)

  # Fitted ====
  if (type == "Classification") {
    if (!probability) {
      fitted.all <- predict(mod, x, predict.all = TRUE)
      fitted.freq <- apply(fitted.all$predictions, 1, function(n) sum(n == 1))
      fitted.prob <- fitted.freq / n.trees
      fitted <- predict(mod, x)$predictions
    } else {
      fitted.prob <- predict(mod, x)$predictions
      fitted <- factor(apply(as.data.frame(fitted.prob), 1, which.max))
      fitted.prob <- fitted.prob[, 1]
      levels(fitted) <- levels(y)
    }
  } else {
    fitted.prob <- NULL
    fitted <- predict(mod, x)$predictions
  }
  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  predicted.prob <- NULL
  if (!is.null(x.test)) {
    if (type == "Classification") {
      if (!probability) {
        predicted.all <- predict(mod, x.test, predict.all = TRUE)
        predicted.freq <- apply(predicted.all$predictions, 1, function(n) sum(n == 1))
        predicted.prob <- predicted.freq / n.trees
        predicted <- predict(mod, x.test)$predictions
      } else {
        predicted.prob <- predict(mod, x.test)$predictions
        predicted <- factor(apply(as.data.frame(predicted.prob), 1, which.max))
        predicted.prob <- predicted.prob[, 1]
        levels(predicted) <- levels(y)
      }
    } else {
      predicted <- predict(mod, x.test)$predictions
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- error.test <- NULL
  }

  # Outro ====
  extra <- list(gridSearch = gs)

  if (imetrics) {
    n.nodes <- sum(plyr::ldply(mod$forest$child.nodeIDs,
                               function(t) plyr::ldply(t, function(n) length(unlist(n))))[, 1])
    extra$imetrics <- list(n.trees = n.trees, n.nodes = n.nodes)
  }
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 parameters = parameters,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 fitted.prob = fitted.prob,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = NULL,
                 error.test = error.test,
                 varimp = if (importance != "none") as.matrix(mod$variable.importance) else NULL,
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

} # rtemis::s.RANGER
