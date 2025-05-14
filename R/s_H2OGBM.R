# s_H2OGBM.R
# ::rtemis::
# 2017-8 E.D. Gennatas rtemis.org

#' Gradient Boosting Machine on H2O (C, R)
#'
#' Trains a Gradient Boosting Machine using H2O (http://www.h2o.ai)
#'
#' \[gS\] denotes tunable hyperparameters
#' Warning: If you get an HTTP 500 error at random, use `h2o.shutdown()` to shutdown the server.
#' It will be restarted when `s_H2OGBM` is called
#' @inheritParams s_GLM
#' @param ip Character: IP address of H2O server. Default = "localhost"
#' @param port Integer: Port number for server. Default = 54321
#' @param n.trees Integer: Number of trees to grow. Maximum number of trees if `n.stopping.rounds > 0`
#' @param max.depth \[gS\] Integer: Depth of trees to grow
#' @param learning.rate \[gS\]
#' @param learning.rate.annealing \[gS\]
#' @param p.col.sample \[gS\]
#' @param p.row.sample \[gS\]
#' @param minobsinnode \[gS\]
#' @param n.stopping.rounds Integer: If > 0, stop training if `stopping.metric` does not improve for this
#' many rounds
#' @param stopping.metric Character: "AUTO" (Default), "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE",
#' "AUC", "lift_top_group", "misclassification", "mean_per_class_error"
#' @param h2o.shutdown.at.end Logical: If TRUE, run `h2o.shutdown(prompt = FALSE)` after
#' training is complete.
#' @param n.cores Integer: Number of cores to use
#' @param .gs Internal use only
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_H2OGBM <- function(
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
  p.row.sample = .9,
  minobsinnode = 5,
  min.split.improvement = 1e-05,
  quantile.alpha = .5,
  learning.rate = .01,
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
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_H2OGBM))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "H2OGBM"

  # Dependencies ----
  dependency_check("h2o")

  # Arguments ----
  if (missing(x)) {
    print(args(s_H2OGBM))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_H2OGBM))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (!is.null(force.n.trees)) n.trees <- force.n.trees

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    ifw = ifw,
    ifw.type = ifw.type,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
  if (is.null(.weights)) .weights <- rep(1, NROW(y))
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # h2o Frames
  if (h2o.init) {
    if (verbose) msg2("Connecting to H2O server...")
    h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  }
  if (verbose) msg2("Creating H2O frames...")
  df.train <- h2o::as.h2o(data.frame(x, y = y, weights = .weights), "df_train")
  # if we are in gs, create df.valid, otherwise df.test
  if (.gs) {
    df.valid <- h2o::as.h2o(
      data.frame(x.test, y = y.test, weights = NA),
      "df_valid"
    )
  } else {
    df.valid <- NULL
    if (!is.null(x.test)) {
      df.test <- h2o::as.h2o(
        data.frame(x.test, y = y.test, weights = NA),
        "df_test"
      )
    } else {
      df.test <- NULL
    }
  }

  # Grid Search ----
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

  .final <- FALSE
  if (!.gs && is.null(force.n.trees)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        max.depth = max.depth,
        learning.rate = learning.rate,
        learning.rate.annealing = learning.rate.annealing,
        p.col.sample = p.col.sample,
        p.row.sample = p.row.sample,
        minobsinnode = minobsinnode
      ),
      fixed.params = list(
        n.trees = n.trees,
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        resample.seed = resample.seed,
        n.stopping.rounds = n.stopping.rounds,
        stopping.metric = stopping.metric,
        min.split.improvement = min.split.improvement,
        quantile.alpha = quantile.alpha,
        h2o.init = gs.h2o.init,
        .gs = TRUE
      ),
      weights = weights,
      metric = metric,
      maximize = maximize,
      verbose = verbose,
      grid.verbose = grid.verbose,
      n.cores = grid.n.cores
    )

    max.depth <- gs$best.tune$max.depth
    learning.rate <- gs$best.tune$learning.rate
    learning.rate.annealing <- gs$best.tune$learning.rate.annealing
    p.col.sample <- gs$best.tune$p.col.sample
    p.row.sample <- gs$best.tune$p.row.sample
    minobsinnode <- gs$best.tune$minobsinnode
    n.trees <- round(gs$best.tune$n.trees)

    # Reload original df.train and df.test
    df.train <- h2o::as.h2o(
      data.frame(x, y = y, weights = .weights),
      "df_train"
    )
    if (!is.null(x.test)) {
      df.test <- h2o::as.h2o(
        data.frame(x.test, y = y.test, weights = NA),
        "df_test"
      )
    }
    # Now ready to train full model
    .final <- TRUE
  } else {
    gs <- NULL
  }
  parameters <- list(
    n.trees = n.trees,
    max.depth = max.depth,
    learning.rate = learning.rate,
    learning.rate.annealing = learning.rate.annealing,
    p.col.sample = p.col.sample,
    p.row.sample = p.row.sample,
    minobsinnode = minobsinnode
  )

  # h2o::h2o.gbm ----
  if (.final) {
    # Use estimated n.trees from grid search. These will be at most n.trees defined originally
    n.stopping.rounds <- 0
    if (verbose) msg2("Training final H2O GBM model...", newline.pre = TRUE)
  } else {
    if (verbose)
      msg2("Training H2O Gradient Boosting Machine...", newline.pre = TRUE)
  }

  mod <- h2o::h2o.gbm(
    y = "y",
    training_frame = df.train,
    model_id = paste0("rtemis_H2OGBM.", format(Sys.time(), "%b%d.%H:%M:%S.%Y")),
    validation_frame = df.valid,
    weights_column = "weights",
    ntrees = n.trees,
    max_depth = max.depth,
    stopping_rounds = n.stopping.rounds,
    stopping_metric = stopping.metric,
    col_sample_rate = p.col.sample,
    sample_rate = p.row.sample,
    min_split_improvement = min.split.improvement,
    quantile_alpha = quantile.alpha,
    learn_rate = learning.rate,
    learn_rate_annealing = learning.rate.annealing
  )

  if (trace > 0) print(mod)

  # Fitted ----
  if (verbose) msg2("Getting fitted values...")
  fitted <- as.data.frame(predict(mod, df.train))[, 1]
  if (type == "Classification") {
    fitted <- factor(fitted, levels = levels(y))
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (verbose) msg2("Getting predicted values...")
    predicted <- if (.gs) {
      as.data.frame(predict(mod, df.valid))[, 1]
    } else {
      as.data.frame(predict(mod, df.test))[, 1]
    }
    if (type == "Classification") {
      predicted <- factor(predicted, levels = levels(y))
    }

    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list()
  if (imetrics) {
    extra$imetrics <- list(
      n.nodes = (2^max.depth) * n.trees,
      depth = max.depth,
      model_summary = as.data.frame(mod@model$model_summary)
    )
  }
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    gridsearch = gs,
    parameters = parameters,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
    question = question,
    extra = extra
  )

  rtMod.out(
    rt,
    print.plot,
    plot.fitted,
    plot.predicted,
    y.test,
    mod.name,
    outdir,
    save.mod,
    verbose,
    plot.theme
  )

  if (.final) if (h2o.shutdown.at.end) h2o::h2o.shutdown(prompt = FALSE)
  if (verbose)
    msg20("Access H2O Flow at http://", ip, ":", port, " in your browser")
  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_H2OGBM
