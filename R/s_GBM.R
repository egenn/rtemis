# s_GBM.R
# ::rtemis::
# 2015-8 E.D. Gennatas rtemis.org
#
# Notes: gbm.more currently fails with distribution "multinomial" due to a bug in gbm.

#' Gradient Boosting Machine \[C, R, S\]
#'
#' Train a GBM model using `gbm::gbm.fit`
#'
#' Early stopping is implemented by fitting `n.trees` initially, checking the
#' optionally smoothed validation error curve, and adding `n.new.trees` if
#' needed, until error does not reduce or `max.trees` is reached.
#' \[gS\] in the argument description indicates that a vector of values can be
#' passed, in which case grid search will be performed automatically using the
#' resampling scheme defined by `grid.resample.params`.
#'
#' This function includes a workaround for when `gbm.fit` fails.
#' If an error is detected, `gbm.fit` is rerun until successful and the
#' procedure continues normally
#'
#' @inheritParams s_CART
#' @param n.trees Integer: Initial number of trees to fit
#' @param max.trees Integer: Maximum number of trees to fit
#' @param force.n.trees Integer: If specified, use this number of trees instead of
#'  tuning number of trees
#' @param gbm.select.smooth Logical: If TRUE, smooth the validation error curve.
#' @param n.new.trees Integer: Number of new trees to train if stopping criteria have
#' not been met.
#' @param min.trees Integer: Minimum number of trees to fit.
#' @param failsafe.trees Integer: If tuning fails to find n.trees, use this number
#' instead.
#' @param imetrics Logical: If TRUE, save `extra$imetrics` with `n.trees`, `depth`,
#' and `n.nodes`.
#' @param plot.tune.error Logical: If TRUE, plot the tuning error curve.
#' @param distribution Character: Distribution of the response variable. See [gbm::gbm]
#' @param interaction.depth \[gS\] Integer: Interaction depth.
#' @param shrinkage \[gS\] Float: Shrinkage (learning rate).
#' @param n.minobsinnode \[gS\] Integer: Minimum number of observation allowed in node.
#' @param bag.fraction \[gS\] Float (0, 1): Fraction of cases to use to train each tree.
#' Helps avoid overfitting.
#' @param save.res.mod Logical: If TRUE, save gbm model for each grid run. For diagnostic purposes only:
#'   Object size adds up quickly
#' @param outdir Character: If defined, save log, 'plot.all' plots (see above) and RDS file of complete output
#' @param relInf Logical: If TRUE (Default), estimate variables' relative influence.
#' @param varImp Logical: If TRUE, estimate variable importance by permutation (as in random forests;
#'   noted as experimental in gbm). Takes longer than (default) relative influence.
#'   The two measures are highly correlated.
#' @param offset Numeric vector of offset values, passed to `gbm::gbm.fit`
#' @param var.monotone Integer vector with values {0, 1, -1} and length = N features.
#' Used to define monotonicity constraints. `0`: no constraint, `1`: increasing,
#' `-1`: decreasing.
#' @param .gs Internal use only
#'
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Ensembles
#' @export

s_GBM <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  distribution = NULL,
  interaction.depth = 2,
  shrinkage = .01,
  bag.fraction = 0.9,
  n.minobsinnode = 5,
  n.trees = 2000,
  max.trees = 5000,
  force.n.trees = NULL,
  gbm.select.smooth = FALSE,
  n.new.trees = 500,
  min.trees = 50,
  failsafe.trees = 500,
  imetrics = FALSE,
  .gs = FALSE,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  metric = NULL,
  maximize = NULL,
  plot.tune.error = FALSE,
  n.cores = rtCores,
  relInf = TRUE,
  varImp = FALSE,
  offset = NULL,
  # misc = NULL,
  var.monotone = NULL,
  keep.data = TRUE,
  var.names = NULL,
  response.name = "y",
  checkmods = FALSE,
  group = NULL,
  plot.perf = FALSE,
  plot.res = ifelse(!is.null(outdir), TRUE, FALSE),
  plot.fitted = NULL,
  plot.predicted = NULL,
  print.plot = FALSE,
  plot.theme = rtTheme,
  x.name = NULL,
  y.name = NULL,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  grid.verbose = verbose,
  gbm.fit.verbose = FALSE,
  outdir = NULL,
  save.gridrun = FALSE,
  save.res = FALSE,
  save.res.mod = FALSE,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE)
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_GBM))
    return(invisible(9))
  }
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
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
  mod.name <- "GBM"

  if (is.null(force.n.trees) && n.trees < min.trees) {
    warning(
      "You requested n.trees = ",
      n.trees,
      ", but specified min.trees = ",
      min.trees,
      "\n  I'll go ahead and specify n.trees = ",
      min.trees
    )
    n.trees <- min.trees
  }

  # Dependencies ----
  dependency_check("gbm")

  # Arguments ----
  if (save.res.mod) save.res <- TRUE
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (n.trees > max.trees) {
    if (verbose)
      msg2(
        "n.trees specified is greater than max.trees, setting n.trees to",
        max.trees
      )
    n.trees <- max.trees
  }

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
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
  n.classes <- length(levels(y0))
  if (type == "Classificationn" && n.classes != 2)
    stop("GBM only supports binary classification")
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
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
    if (verbose) msg2("Distribution set to", distribution)
  }

  # Keep original inputs (after prepare_data)
  .x <- x
  .y <- y
  .x.test <- x.test
  .y.test <- y.test

  # Name of loss function - in order to get the correct name for quantile regression
  loss <- ifelse(
    length(distribution) == 1,
    distribution,
    as.character(distribution$name)
  )

  # For Bernoulli, convert to {0, 1}
  if (loss == "bernoulli") {
    .y <- as.integer(.y) - 1
    .y.test <- as.integer(.y.test) - 1
  }

  if (verbose)
    msg2(
      "Running Gradient Boosting",
      type,
      "with a",
      loss[[1]],
      "loss function",
      newline.pre = TRUE
    )

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

  # .final <- FALSE
  gc <- gridCheck(interaction.depth, shrinkage, bag.fraction, n.minobsinnode)
  if (!.gs && (gc || is.null(force.n.trees))) {
    gs <- gridSearchLearn(
      x = x0,
      y = y0,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        interaction.depth = interaction.depth,
        shrinkage = shrinkage,
        bag.fraction = bag.fraction,
        n.minobsinnode = n.minobsinnode
      ),
      fixed.params = list(
        n.trees = n.trees,
        max.trees = max.trees,
        gbm.select.smooth = gbm.select.smooth,
        n.new.trees = n.new.trees,
        min.trees = min.trees,
        failsafe.trees = failsafe.trees,
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        downsample = downsample,
        resample.seed = resample.seed,
        relInf = FALSE,
        plot.tune.error = plot.tune.error,
        .gs = TRUE
      ),
      search.type = gridsearch.type,
      weights = weights,
      metric = metric,
      maximize = maximize,
      save.mod = save.gridrun,
      verbose = verbose,
      grid.verbose = grid.verbose,
      n.cores = n.cores
    )

    interaction.depth <- gs$best.tune$interaction.depth
    shrinkage <- gs$best.tune$shrinkage
    bag.fraction <- gs$best.tune$bag.fraction
    n.minobsinnode <- gs$best.tune$n.minobsinnode
    n.trees <- gs$best.tune$n.trees
    if (n.trees == -1) {
      warning(
        "Tuning failed to find n.trees, defaulting to failsafe.trees = ",
        failsafe.trees,
        "."
      )
      n.trees <- failsafe.trees
    }
    if (n.trees < min.trees) {
      warning(
        "Tuning returned ",
        n.trees,
        " trees; using min.trees = ",
        min.trees,
        " instead."
      )
      n.trees <- min.trees
    }

    # Now ready to train final full model
    # .final <- TRUE
    .gs <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.n.trees)) n.trees <- force.n.trees
  parameters <- list(
    n.trees = n.trees,
    interaction.depth = interaction.depth,
    shrinkage = shrinkage,
    bag.fraction = bag.fraction,
    n.minobsinnode = n.minobsinnode,
    weights = .weights
  )
  if (verbose) {
    parameterSummary(
      n.trees,
      interaction.depth,
      shrinkage,
      bag.fraction,
      n.minobsinnode,
      weights,
      newline.pre = TRUE
    )
  }

  # gbm::gbm.fit ----
  if (!is.null(logFile)) sink() # pause writing to log
  # If we are in .gs, rbind train and test to get perf to tune n.trees
  # .xtrain and .ytrain to allow diff b/n .gs and full model
  if (.gs) {
    .x.train <- rbind(.x, .x.test) # will be split to train/test by nTrain
    .y.train <- c(.y, .y.test)
  } else {
    ### Fit the final model on the whole internal set using the optimal n of trees estimated above
    # incl. hack to check model is good: add small valid set to see if valid.error gets estimated
    .x.train <- rbind(.x, .x[1, , drop = FALSE])
    .y.train <- c(.y, .y[1])
    if (verbose) msg2("Training GBM on full training set...")
  }
  mod <- gbm::gbm.fit(
    x = .x.train,
    y = .y.train,
    offset = offset,
    # misc = misc,
    distribution = distribution,
    w = .weights,
    var.monotone = var.monotone,
    n.trees = n.trees,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    shrinkage = shrinkage,
    bag.fraction = bag.fraction,
    nTrain = NROW(.x),
    keep.data = keep.data,
    verbose = gbm.fit.verbose,
    var.names = var.names,
    response.name = response.name,
    group = group
  )
  if (!is.null(logFile)) sink(logFile, append = TRUE, split = verbose) # Resume writing to log

  if (checkmods) {
    while (all(is.na(mod$valid.error))) {
      msg2("###   Caught gbm.fit error; retrying...   ###")
      warning("Caught gbm.fit error: retraining last model and continuing")
      if (!is.null(logFile)) sink() # pause logging
      mod <- gbm::gbm.fit(
        x = .x.train,
        y = .y.train,
        offset = offset,
        # misc = misc,
        distribution = distribution,
        w = .weights,
        var.monotone = var.monotone,
        n.trees = n.trees,
        interaction.depth = interaction.depth,
        n.minobsinnode = n.minobsinnode,
        shrinkage = shrinkage,
        bag.fraction = bag.fraction,
        nTrain = NROW(.x),
        keep.data = keep.data,
        verbose = gbm.fit.verbose,
        var.names = var.names,
        response.name = response.name,
        group = group
      )
      if (!is.null(logFile)) sink(logFile, append = TRUE, split = verbose) # Resume writing to log
    }
  }

  # If we are in .gs, use the best n.trees to get fitted and predicted values,
  # error.train, and error.test.
  if (.gs) {
    gst <- gbm.select.trees(
      mod,
      smooth = gbm.select.smooth,
      plot = plot.tune.error,
      verbose = verbose
    )
    n.trees <- gst$n.trees
    valid.error.smooth <- gst$valid.error.smooth
    if (plot.tune.error) {
      mplot3_xy(
        seq(valid.error.smooth),
        list(
          Training = mod$train.error,
          Validation = mod$valid.error,
          `Smoothed Validation` = valid.error.smooth
        ),
        type = "l",
        group.adj = .95,
        line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
        vline = c(which.min(mod$valid.error), which.min(valid.error.smooth)),
        vline.col = c(ucsfCol$red, ucsfCol$purple),
        xlab = "N trees",
        ylab = "Loss"
      )
    }
    if (trace > 0) msg2("### n.trees is", n.trees)
    while (n.trees >= mod$n.trees && mod$n.trees < max.trees) {
      n.new.trees <- min(n.new.trees, max.trees - mod$n.trees)
      if (verbose) {
        msg2(
          "Adding",
          n.new.trees,
          "more trees to trained GBM model...",
          "\n    * current mod$n.trees =",
          mod$n.trees,
          "\n    * best n.trees = ",
          n.trees,
          "\n    * max.trees =",
          max.trees
        )
      }
      mod <- gbm::gbm.more(
        mod,
        n.new.trees = n.new.trees,
        verbose = gbm.fit.verbose
      )
      # CHECK: does this need to be checked the same way as mod above?
      gst <- gbm.select.trees(
        mod,
        smooth = gbm.select.smooth,
        plot = plot.tune.error,
        verbose = verbose
      )
      n.trees <- gst$n.trees
      valid.error.smooth <- gst$valid.error.smooth
      if (plot.tune.error) {
        mplot3_xy(
          seq(valid.error.smooth),
          list(
            Training = mod$train.error,
            Validation = mod$valid.error,
            `Smoothed Validation` = valid.error.smooth
          ),
          type = "l",
          group.adj = .95,
          line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
          vline = c(which.min(mod$valid.error), which.min(valid.error.smooth)),
          vline.col = c(ucsfCol$red, ucsfCol$purple),
          xlab = "N trees",
          ylab = "Loss"
        )
      }
    }
    if (n.trees == max.trees && verbose) msg2("Reached max.trees =", max.trees)
  }

  # Fitted ----
  fitted.prob <- NULL
  if (type == "Regression" || type == "Survival") {
    if (distribution == "poisson") {
      fitted <- predict(mod, .x, n.trees = n.trees, type = "response")
    } else {
      fitted <- predict(mod, .x, n.trees = n.trees)
    }
  } else {
    if (distribution == "multinomial") {
      # Get probabilities per class
      fitted.prob <- fitted <- predict(
        mod,
        .x,
        n.trees = n.trees,
        type = "response"
      )
      fitted <- factor(
        apply(fitted, 1, function(x) levels(y)[which.max(x)]),
        levels = levels(y)
      )
    } else {
      # Bernoulli: convert {0, 1} back to factor
      fitted.prob <- 1 -
        predict(
          mod,
          .x,
          n.trees = n.trees,
          type = "response"
        )
      fitted <- factor(
        ifelse(fitted.prob >= .5, 1, 0),
        levels = c(1, 0),
        labels = levels(y)
      )
    }
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  ### Relative influence & variable importance
  # Estimating rel inf takes time, var imp even more so.
  # Do not estimate unless you need them.
  mod.summary.rel <- NULL
  if (relInf) {
    if (verbose) msg2("Calculating relative influence of variables...")
    mod.summary.rel <- gbm::summary.gbm(
      mod,
      plotit = FALSE,
      order = FALSE,
      method = gbm::relative.influence
    )
  }

  mod.summary.perm <- NULL
  if (varImp) {
    if (verbose)
      msg2("Calculating variable importance by permutation testing...")
    # similar to random forests (stated as experimental)
    mod.summary.perm <- gbm::summary.gbm(
      mod,
      plotit = FALSE,
      order = FALSE,
      method = gbm::permutation.test.gbm
    )
  }

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(.x.test)) {
    if (type == "Regression" || type == "Survival") {
      if (distribution == "poisson") {
        if (trace > 0)
          msg2("Using predict for Poisson Regression with type = response")
        predicted <- predict(mod, x.test, n.trees = n.trees, type = "response")
      } else {
        if (verbose) msg2("Using predict for", type, "with type = link")
        predicted <- predict(mod, x.test, n.trees = n.trees)
      }
    } else {
      if (distribution == "multinomial") {
        if (trace > 0)
          msg2(
            "Using predict for multinomial classification with type = response"
          )
        # Get per-class probabilities
        predicted.prob <- predicted <- predict(
          mod,
          x.test,
          n.trees = n.trees,
          type = "response"
        )
        # Now get the predicted classes
        predicted <- factor(
          apply(predicted, 1, function(x) levels(y.test)[which.max(x)]),
          levels = levels(y)
        )
      } else {
        # Bernoulli: convert {0, 1} back to factor
        predicted.prob <- 1 -
          predict(mod, x.test, n.trees = n.trees, type = "response")
        predicted <- factor(
          ifelse(predicted.prob >= .5, 1, 0),
          levels = c(1, 0)
        )
        levels(predicted) <- levels(y)
      }
    }

    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  if (imetrics) {
    extra$imetrics <- list(
      n.trees = n.trees,
      depth = interaction.depth,
      n.nodes = (2^interaction.depth) * n.trees
    )
  }

  if (!is.null(mod.summary.perm)) {
    varimp <- mod.summary.perm[, 2]
    names(varimp) <- mod.summary.perm[, 1]
    attr(varimp, "type") <- "permutation-based variable importance"
  } else if (!is.null(mod.summary.rel)) {
    varimp <- mod.summary.rel[, 2]
    names(varimp) <- mod.summary.rel[, 1]
    attr(varimp, "type") <- "relative influence"
  } else {
    varimp <- NULL
  }

  rt <- rtModSet(
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
    fitted.prob = fitted.prob,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    predicted.prob = predicted.prob,
    se.prediction = NULL,
    error.test = error.test,
    varimp = varimp,
    question = question,
    extra = list(
      mod.summary.rel = mod.summary.rel,
      mod.summary.perm = mod.summary.perm
    )
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

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_GBM


#' Select number of trees for GBM
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
gbm.select.trees <- function(
  object,
  smooth = TRUE,
  plot = FALSE,
  verbose = FALSE
) {
  n.trees <- object$n.trees

  valid.error.smooth <- if (smooth) {
    supsmu(seq(n.trees), object$valid.error)$y
  } else {
    NULL
  }
  valid.error <- if (smooth) valid.error.smooth else object$valid.error

  if (plot) {
    mplot3_xy(
      seq(n.trees),
      list(
        Training = object$train.error,
        Validation = object$valid.error,
        `Smoothed Validation` = valid.error.smooth
      ),
      type = "l",
      group.adj = .95,
      line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
      vline = c(which.min(object$valid.error), which.min(valid.error.smooth)),
      vline.col = c(ucsfCol$red, ucsfCol$purple),
      xlab = "N trees",
      ylab = "Loss"
    )
  }

  list(
    n.trees = which.min(valid.error),
    valid.error.smooth = valid.error.smooth
  )
} # rtemis::gbm.select.trees
