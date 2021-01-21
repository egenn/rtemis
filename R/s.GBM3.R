# s.GBM3.R
# ::rtemis::
# 2015-8 E.D. Gennatas lambdamd.org
#
# Notes: gbm.more currently fails with distribution "multinomial" due to a bug in gbm.


#' Gradient Boosting Machine [C, R, S]
#'
#' Train a GBM model using \code{gbm-developers/gbm3}
#'
#' Early stopping is implemented by fitting \code{n.trees} initially, checking the (smoothed) validation
#' error curve, and adding \code{n.new.trees} if needed, until error does not reduce or \code{max.trees} is
#' reached.
#' [gS] in the argument description indicates that multiple values can be passed, in
#' which case tuning will be performed using grid search. gS is supported for:
#' interaction.depth, shrinkage, bag.fraction, mFeatures, and n.minobsinnode
#' This function includes a workaround for when \code{gbm.fit} fails.
#' If an error is detected, \code{gbm.fit} is rerun until successful and the procedure continues normally
#' @inheritParams s.GLM
#' @inheritParams s.CART
#' @param n.trees Integer: Initial number of trees to fit. Default = 2000
#' @param max.trees Integer: Maximum number of trees to fit. Default = 5000
#' @param interaction.depth [gS] Integer: Interaction depth. Default = 3
#' @param shrinkage [gS] Float: Shrinkage (learning rate). Default = .01
#' @param n.minobsinnode [gS] Integer: Minimum number of observation allowed in node
#' @param bag.fraction [gS] Float (0, 1): Fraction of cases to use to train each tree.
#' Helps avoid overfitting. Default = .9
#' @param mFeatures [gS] Integer: Number of features to randomly choose from all available features to train at each
#' step. Default = NULL which results in using all features.
#' @param save.res.mod   Logical: If TRUE, save gbm model for each grid run. For diagnostic purposes only:
#'   Object size adds up quickly
#' @param stratify.var   If resampling is stratified, stratify against this variable. Defaults to outcome
#' @param outdir         Character: If defined, save log, 'plot.all' plots (see above) and RDS file of complete output
#' @param save.rds       Logical: If outdir is defined, should all data be saved in RDS file? s.SVDnetGBM will save
#'   mod.gbm, so no need to save again.
#' @param relInf         Logical: If TRUE (Default), estimate variables' relative influence.
#' @param varImp         Logical: If TRUE, estimate variable importance by permutation (as in random forests;
#'   noted as experimental in gbm). Takes longer than (default) relative influence.
#'   The two measures are highly correlated.
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Ensembles
#' @export

s.GBM3 <- function(x, y = NULL,
                   x.test = NULL, y.test = NULL,
                   weights = NULL,
                   ipw = TRUE,
                   ipw.type = 2,
                   upsample = FALSE,
                   downsample = FALSE,
                   resample.seed = NULL,
                   distribution = NULL,
                   interaction.depth = 3,
                   shrinkage = .01,
                   bag.fraction = 0.9,
                   mFeatures = NULL,
                   n.minobsinnode = 5,
                   n.trees = 2000,
                   max.trees = 5000,
                   force.n.trees = NULL,
                   n.tree.window = 0,
                   gbm.select.smooth = TRUE,
                   smoother = c("loess", "supsmu"),
                   n.new.trees = 500,
                   min.trees = 50,
                   failsafe.trees = 1000,
                   imetrics = FALSE,
                   .gs = FALSE,
                   grid.resample.rtset = rtset.resample("kfold", 5),
                   grid.search.type = c("exhaustive", "randomized"),
                   grid.randomized.p = .1,
                   metric = NULL,
                   maximize = NULL,
                   plot.tune.error = FALSE,
                   exclude.test.lt.train = FALSE,
                   exclude.lt.min.trees = FALSE,
                   res.fail.thres = .99,
                   n.extra.trees = 0,
                   n.cores = rtCores,
                   gbm.cores = 1,
                   relInf = TRUE,
                   varImp = FALSE,
                   offset = NULL,
                   var.monotone = NULL,
                   keep.data = TRUE,
                   var.names = NULL,
                   response.name = "y",
                   group = NULL,
                   plot.perf = FALSE,
                   plot.res = ifelse(!is.null(outdir), TRUE, FALSE),
                   plot.fitted = NULL,
                   plot.predicted = NULL,
                   plotRelInf = FALSE,
                   plotVarImp = FALSE,
                   print.plot = TRUE,
                   plot.theme = getOption("rt.fit.theme", "lightgrid"),
                   x.name = NULL, y.name = NULL,
                   question = NULL,
                   verbose = TRUE,
                   trace = 0,
                   grid.verbose = TRUE,
                   gbm.fit.verbose = FALSE,
                   outdir = NULL,
                   save.gridrun = FALSE,
                   save.error.diagnostics = FALSE,
                   save.rds = TRUE,
                   save.res = FALSE,
                   save.res.mod = FALSE,
                   save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.GBM3))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "GBM3"

  if (is.null(force.n.trees) && n.trees < min.trees) {
    warning("You requested n.trees = ", n.trees, ", but specified min.trees = ", min.trees,
            "\n  I'll go ahead and specify n.trees = ", min.trees)
    n.trees <- min.trees
  }

  # [ DEPENDENCIES ] ====
  if (!depCheck("gbm3", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (save.res.mod) save.res <- TRUE
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  # if (plot.res) {
  #   plot.res.outdir <- paste0(outdir, "/plotRes/")
  #   if (!dir.exists(plot.res.outdir)) dir.create(plot.res.outdir)
  # }
  if (n.trees > max.trees) {
    if (verbose) msg("n.trees specified is greater than max.trees, setting n.trees to", max.trees)
    n.trees <- max.trees
  }
  grid.search.type <- match.arg(grid.search.type)
  smoother <- match.arg(smoother)

  # [ DATA ] ====
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
  x0 <- if (upsample|downsample) dt$x0 else x
  y0 <- if (upsample|downsample) dt$y0 else y
  n.classes <- length(levels(y0))
  if (type == "Classificationn" && n.classes != 2) stop("GBM3 only supports binary classification")
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (type == "Classification") nlevels <- length(levels(y))

  if (is.null(distribution)) {
    if (type == "Classification") {
      distribution <- ifelse(length(levels(y)) > 2, "multinomial", "bernoulli")
    } else if (type == "Survival") {
      distribution <- "coxph"
    } else {
      distribution <- "gaussian"
    }
  }

  # Keep original inputs (after dataPrepare)
  .x <- x
  .y <- y
  .x.test <- x.test
  .y.test <- y.test

  # Name of loss function - in order to get the correct name for quantile regression
  loss <- ifelse(length(distribution) == 1, distribution, as.character(distribution$name))

  # For Bernoulli, convert to {0, 1}
  if (loss == "bernoulli") {
    .y <- as.integer(.y) - 1
    .y.test <- as.integer(.y.test) - 1
  }

  if (verbose) msg("Running Gradient Boosting", type, "with a", loss[[1]], "loss function...", newline.pre = TRUE)

  # [ GRID SEARCH ] ====
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Regression") {
      metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    }
  }

  .final <- FALSE
  gc <- gridCheck(interaction.depth, shrinkage, bag.fraction, mFeatures, n.minobsinnode)
  if (!.gs && (gc | is.null(force.n.trees))) {
    gs <- gridSearchLearn(x = x0, y = y0,
                          mod = mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(interaction.depth = interaction.depth,
                                             shrinkage = shrinkage,
                                             bag.fraction = bag.fraction,
                                             mFeatures = mFeatures,
                                             n.minobsinnode = n.minobsinnode),
                          fixed.params = list(n.trees = n.trees,
                                              max.trees = max.trees,
                                              n.tree.window = n.tree.window,
                                              gbm.select.smooth = gbm.select.smooth,
                                              n.new.trees = n.new.trees,
                                              min.trees = min.trees,
                                              failsafe.trees = failsafe.trees,
                                              ipw = ipw,
                                              ipw.type = ipw.type,
                                              upsample = upsample,
                                              downsample = downsample,
                                              resample.seed = resample.seed,
                                              relInf = FALSE,
                                              plot.tune.error = plot.tune.error,
                                              .gs = TRUE),
                          search.type = grid.search.type,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          save.mod = save.gridrun,
                          verbose = verbose,
                          grid.verbose = grid.verbose,
                          n.cores = n.cores)

    interaction.depth <- gs$best.tune$interaction.depth
    shrinkage <- gs$best.tune$shrinkage
    bag.fraction <- gs$best.tune$bag.fraction
    mFeatures <- gs$best.tune$mFeatures
    n.minobsinnode <- gs$best.tune$n.minobsinnode
    n.trees <- gs$best.tune$n.trees
    if (n.trees == -1) {
      warning("Tuning failed to find n.trees, defaulting to failsafe.trees = ", failsafe.trees)
      n.trees <- failsafe.trees
    }
    if (n.trees < min.trees) {
      warning("Tuning returned ", n.trees, " trees; using min.trees = ", min.trees, " instead")
      n.trees <- min.trees
    }

    # Now ready to train final full model
    .final <- TRUE
    .gs <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.n.trees)) n.trees <- force.n.trees
  parameters <- list(n.trees = n.trees,
                     interaction.depth = interaction.depth,
                     shrinkage = shrinkage,
                     bag.fraction = bag.fraction,
                     mFeatures = mFeatures,
                     n.minobsinnode = n.minobsinnode,
                     weights = .weights)
  if (verbose) {
    parameterSummary(n.trees,
                     interaction.depth,
                     shrinkage,
                     bag.fraction,
                     mFeatures,
                     n.minobsinnode,
                     weights,
                     newline.pre = TRUE)
  }

  # [ GBM3 ] ====
  if (!is.null(logFile)) sink() # pause writing to log
  # If we are in .gs, rbind train and test to get perf to tune n.trees
  # .xtrain and .ytrain to allow diff b/n .gs and full model
  if (.gs) {
    # This .x.test is the inner validation set, not the final test set
    .x.train <- rbind(.x, .x.test) # will be split to train/validation by nTrain
    .y.train <- c(.y, .y.test)
  } else {
    ### Fit the final model on the whole internal set using the optimal n of trees estimated above
    # incl. hack to check model is good: add single validation case to see if valid.error gets estimated
    .x.train <- rbind(.x, .x[1, , drop = FALSE])
    .y.train <- c(.y, .y[1])
    if (verbose) msg("Training GBM3 on full training set...", newline.pre = TRUE)
  }
  mod <- gbm3::gbm.fit(x = .x.train, y = .y.train,
                       offset = offset,
                       distribution = distribution,
                       w = .weights,
                       var.monotone = var.monotone,
                       n.trees = n.trees,
                       interaction.depth = interaction.depth,
                       n.minobsinnode = n.minobsinnode,
                       shrinkage = shrinkage,
                       bag.fraction = bag.fraction,
                       mFeatures = mFeatures,
                       nTrain = NROW(.x),
                       keep.data = keep.data,
                       verbose = gbm.fit.verbose,
                       var.names = var.names,
                       response.name = response.name,
                       group = group)
  if (!is.null(logFile)) sink(logFile, append = TRUE, split = verbose) # Resume writing to log

  while (all(is.na(mod$valid.error))) {
    msg("###   Caught gbm.fit error; retrying...   ###")
    if (save.error.diagnostics) {
      saveRDS(list(mod = mod,
                   x = .x.train, y = .y.train,
                   offset = offset,
                   distribution = distribution,
                   w = .weights,
                   var.monotone = var.monotone,
                   n.trees = n.trees,
                   interaction.depth = interaction.depth,
                   n.minobsinnode = n.minobsinnode,
                   shrinkage = shrinkage,
                   bag.fraction = bag.fraction,
                   mFeatures = mFeatures,
                   nTrain = NROW(.x),
                   keep.data = keep.data,
                   verbose = gbm.fit.verbose,
                   var.names = var.names,
                   response.name = response.name,
                   group = group),
              paste0("~/Desktop/s.GBM3.panic.rds"))
    }
    warning("Caught gbm.fit error: retraining last model and continuing")
    if (!is.null(logFile)) sink() # pause logging
    mod <- gbm3::gbm.fit(x = .x.train, y = .y.train,
                         offset = offset,
                         distribution = distribution,
                         w = .weights,
                         var.monotone = var.monotone,
                         n.trees = n.trees,
                         interaction.depth = interaction.depth,
                         n.minobsinnode = n.minobsinnode,
                         shrinkage = shrinkage,
                         bag.fraction = bag.fraction,
                         mFeatures = mFeatures,
                         nTrain = NROW(.x),
                         keep.data = keep.data,
                         verbose = gbm.fit.verbose,
                         var.names = var.names,
                         response.name = response.name,
                         group = group)
    if (!is.null(logFile)) sink(logFile, append = TRUE, split = verbose) # Resume writing to log
  }

  # If we are in .gs, use the best n.trees to get fitted and predicted values,
  # error.train, and error.test.
  if (.gs) {

    gst <- gbm3.select.trees(mod,
                             smooth = gbm.select.smooth,
                             plot = plot.tune.error,
                             verbose = verbose)
    n.trees <- gst$n.trees
    valid.error.smooth <- gst$valid.error.smooth
    if (plot.tune.error) mplot3.xy(seq(valid.error.smooth),
                                   list(Training = mod$train.error,
                                        Validation = mod$valid.error,
                                        `Smoothed Validation` = valid.error.smooth),
                                   type = 'l', group.adj = .95,
                                   line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
                                   vline = c(which.min(mod$valid.error), which.min(valid.error.smooth)),
                                   vline.col = c(ucsfCol$red, ucsfCol$purple),
                                   xlab = "N trees", ylab = "Loss")
    if (trace > 0) msg("### n.trees is", n.trees)
    while (n.trees >= (mod$params$num_trees - n.tree.window) & mod$params$num_trees < max.trees) {
      n.new.trees <- min(n.new.trees, max.trees - mod$params$num_trees)
      if (verbose) msg("Adding", n.new.trees, "more trees to trained GBM model...",
                       "\n    * current mod$params$num_trees =", mod$params$num_trees,
                       "\n    * best n.trees = ", n.trees,
                       "\n    * max.trees =", max.trees)
      mod <- gbm3::gbm_more(mod, num_new_trees = n.new.trees, is_verbose = gbm.fit.verbose)
      # CHECK: does this need to be checked the same way as mod above?
      gst <- gbm3.select.trees(mod,
                               smooth = gbm.select.smooth,
                               smoother = smoother,
                               plot = plot.tune.error,
                               verbose = verbose)
      n.trees <- gst$n.trees
      valid.error.smooth <- gst$valid.error.smooth
      if (plot.tune.error) mplot3.xy(seq(valid.error.smooth),
                                     list(Training = mod$train.error,
                                          Validation = mod$valid.error,
                                          `Smoothed Validation` = valid.error.smooth),
                                     type = 'l', group.adj = .95,
                                     line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
                                     vline = c(which.min(mod$valid.error), which.min(valid.error.smooth)),
                                     vline.col = c(ucsfCol$red, ucsfCol$purple),
                                     xlab = "N trees", ylab = "Loss")
    }
    if (n.trees == max.trees & verbose) msg("Reached max.trees =", max.trees)
  }

  # [ FITTED ] ====
  fitted.prob <- NULL
  if (type == "Regression" | type == "Survival") {
    if (distribution == "poisson") {
      fitted <- predict(mod, .x, n.trees = n.trees, type = "response")
    } else {
      fitted <- predict(mod, .x, n.trees = n.trees)
    }
  } else {
    if (distribution == "multinomial") {
      # Get probabilities per class
      fitted.prob <- fitted <- predict(mod, .x, n.trees = n.trees, type = "response")
      fitted <- apply(fitted, 1, function(x) levels(y)[which.max(x)])
    } else {
      # Bernoulli: convert {0, 1} back to factor
      fitted.prob <- 1 - predict(mod, .x, n.trees = n.trees, type = "response")
      fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(fitted) <- levels(y)
    }
  }

  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  ### Relative influence & variable importance
  # Estimating rel inf takes time, var imp even more so.
  # Do not estimate unless you need them.
  mod.summary.rel <- NULL
  if (relInf) {
    if (verbose) msg("Calculating relative influence of variables...")
    mod.summary.rel <- summary(mod, plot_it = plotRelInf,
                               order = FALSE, method = gbm3::relative_influence)
    if (plotRelInf) mtext(paste0(y.name, " ~ ", x.name, " GBM relative influence"), padj = -2)
  }

  mod.summary.perm <- NULL
  if (varImp) {
    if (verbose) msg("Calculating variable importance by permutation testing...")
    # similar to random forests (stated as experimental)
    mod.summary.perm <- summary(mod, plotit = plotVarImp,
                                order = FALSE, method = gbm3::permutation_relative_influence)
    if (plotVarImp) mtext(paste0(y.name, " ~ ", x.name,
                                 " GBM permutation-based variable importance"), padj = -2)
  }

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(.x.test)) {
    if (type == "Regression" | type == "Survival") {
      if (distribution == "poisson") {
        if (trace > 0) msg("Using predict for Poisson Regression with type = response")
        predicted <- predict(mod, x.test, n.trees = n.trees, type = "response")
      } else {
        if (verbose) msg("Using predict for", type, "with type = link")
        predicted <- predict(mod, x.test, n.trees = n.trees)
      }
    } else {
      if (distribution == "multinomial") {
        if (trace > 0) msg("Using predict for multinomial classification with type = response")
        # Get probabilities per class
        predicted.prob <- predicted <- predict(mod, x.test, n.trees = n.trees, type = "response")
        # Now get the predicted classes
        predicted <- apply(predicted, 1, function(x) levels(y.test)[which.max(x)])
      } else {
        # Bernoulli: convert {0, 1} back to factor
        predicted.prob <- 1 - predict(mod, x.test, n.trees = n.trees, type = "response")
        predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
        levels(predicted) <- levels(y)
      }
    }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(gridSearch = gs,
                mod.summary.rel = mod.summary.rel,
                mod.summary.perm = mod.summary.perm)
  if (imetrics) {
    extra$imetrics <- list(n.trees = n.trees,
                           depth = interaction.depth,
                           n.nodes = (2 ^ interaction.depth) * n.trees)
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
                 varimp = if (!is.null(mod.summary.rel)) mod.summary.rel[, 2, drop = FALSE] else NULL,
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

} # rtemis::s.GBM


gbm3.select.trees <- function(object,
                              smooth = TRUE,
                              smoother = "loess",
                              plot = FALSE,
                              verbose = FALSE) {

  n.trees <- object$params$num_trees

  valid.error.smooth <- if (smooth) {
    if (smoother == "loess") {
      valid.error.smooth <- loess(object$valid.error ~ seq(n.trees))$fitted
    } else {
      valid.error.smooth <- supsmu(seq(n.trees), object$valid.error)$y
    }
  } else {
    NULL
  }
  valid.error <- if (smooth) valid.error.smooth else object$valid.error

  if (plot) mplot3.xy(seq(n.trees), list(Training = object$train.error,
                                         Validation = object$valid.error,
                                         `Smoothed Validation` = valid.error.smooth),
                      type = 'l', group.adj = .95,
                      line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
                      vline = c(which.min(object$valid.error), which.min(valid.error.smooth)),
                      vline.col = c(ucsfCol$red, ucsfCol$purple),
                      xlab = "N trees", ylab = "Loss")

  list(n.trees = which.min(valid.error),
       valid.error.smooth = valid.error.smooth)

} # rtemis::gbm3.select.trees
