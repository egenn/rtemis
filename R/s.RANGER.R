# s.RANGER.R
# ::rtemis::
# 2016-8 Efstathios D. Gennatas egenn.github.io
# TODO: Add Survival support

#' Random Forest Classification and Regression [C, R]
#'
#' Train a Random Forest for regression or classification using \code{ranger}
#'
#' You should cconsider, or try, setting mtry to NCOL(x), especially for small number of features.
#' If \code{autotune = TRUE}, \code{randomForest::tuneRF} will be run to determine best \code{mtry}
#'   value.
#' [gS]: indicated parameter will be tuned by grid search if more than one value is passed
#'
#'
#' @inheritParams s.RF
#' @inheritParams s.CART
#' @param n.trees Integer: Number of trees to grow. Default = 1000
#' @param mtry [gS] Integer: Number of features sampled randomly at each split. Defaults to square root of n of
#' features for classification, and a third of n of features for regression.
#' @param min.node.size [gS] Integer: Minimum node size
#' @param splitrule String: For classification: "gini" (Default) or "extratrees";
#' For regression: "variance" (Default), "extratrees" or "maxstat".
#' For survival "logrank" (Default), "extratrees", "C" or "maxstat".
#' @param probability Logical: If TRUE, grow a probability forest. See \code{ranger::ranger}
#' @param classwt Vector, Float: Priors of the classes for \code{randomForest::tuneRF} if  \code{autotune = TRUE}.
#' For classification only; need not add up to 1
#' @param ... Additional arguments to be passed to \code{ranger::ranger}
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
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
                     upsample = FALSE,
                     upsample.seed = NULL,
                     autotune = FALSE,
                     classwt = NULL,
                     n.trees.try = 500,
                     stepFactor = 2,
                     mtry = NULL,
                     mtryStart = NULL,
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

  # [ INTRO ] ====
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

  # [ DEPENDENCIES ] ====
  if (!depCheck("ranger", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
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

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry)
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  .classwt <- if (is.null(classwt) & ipw) dt$class.weights else classwt
  x0 <- if (upsample) dt$x0 else x
  y0 <- if (upsample) dt$y0 else y
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(mtry)) {
    mtry <- if (type == "Classification") floor(sqrt(NCOL(x))) else max(floor(NCOL(x)/3), 1)
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

  # [ FORMULA ] ====
  df.train <- data.frame(x, y)
  colnames(df.train)[ncol(df.train)] <- y.name
  .formula <- as.formula(paste(y.name, "~ ."))

  # [ GRID SEARCH ] ====
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
                                              upsample.seed = upsample.seed),
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
    mtry <- if (type == "Classification") floor(sqrt(NCOL(x))) else max(floor(NCOL(x)/3), 1)
  }

  if (length(min.node.size) == 0) {
    min.node.size <- if (type == "Classification") 1 else 5
  }

  # [ tuneRF ] ====
  if (is.null(mtryStart)) mtryStart <- sqrt(NCOL(x))
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
                                      classwt = .classwt))
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
                     upsample.seed = upsample.seed)

  # [ RANGER ] ====
  if (verbose) msg("Training Random Forest (ranger)", type, "with", n.trees, "trees...", newline = TRUE)
  mod <- ranger::ranger(formula = .formula,
                        data = df.train,
                        num.trees = n.trees,
                        case.weights = .weights,
                        mtry = mtry,
                        min.node.size = min.node.size,
                        splitrule = splitrule,
                        replace = replace,
                        probability = probability,
                        importance = importance,
                        write.forest = TRUE,
                        num.threads = n.cores, ...)

  # [ FITTED ] ====
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
  # if (type == "Classification" && nlevels == 2) error.train$overall$AUC <- auc(fitted.prob, y)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
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
      # if (type == "Classification" && nlevels == 2) error.test$overall$AUC <- auc(predicted.prob, y.test)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- error.test <- NULL
  }

  # Discard forest to save memory
  # if (discard.forest) mod$forest <- NULL

  # [ OUTRO ] ====
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
