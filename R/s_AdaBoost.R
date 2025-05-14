# s_AdaBoost.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Adaboost Binary Classifier [C]
#'
#' Train an Adaboost Classifier using `ada::ada`
#'
#' `ada::ada` does not support case weights
#'
#' @inheritParams s_GLM
#' @param loss Character: "exponential" (Default), "logistic"
#' @param type Character: "discrete", "real", "gentle"
#' @param iter Integer: Number of boosting iterations to perform. Default = 50
#' @param nu Float: Shrinkage parameter for boosting. Default = .1
#' @param bag.frac Float (0, 1]: Sampling fraction for out-of-bag samples
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Ensembles
#' @export

s_AdaBoost <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  loss = "exponential",
  type = "discrete",
  iter = 50,
  nu = .1,
  bag.frac = .5,
  upsample = FALSE,
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
  trace = 0,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_AdaBoost))
    invisible(9)
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
  mod.name <- "AdaBoost"

  # Dependencies ----
  dependency_check("ada")

  # Arguments ----
  .type <- type
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  # prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
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
  if (type != "Classification" || length(levels(y)) > 2)
    stop("AdaBoost is for binary classification only")
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # AdaBoost ----
  if (verbose) msg2("Training AdaBoost Classifier...", newline.pre = TRUE)
  mod <- ada::ada(
    x,
    y,
    loss = loss,
    type = .type,
    iter = iter,
    nu = nu,
    bag.frac = bag.frac,
    verbose = verbose,
    ...
  )
  if (trace > 0) summary(mod)

  # Fitted ----
  fitted.raw <- predict(mod, x, "both")
  fitted.prob <- fitted.raw$probs
  fitted <- factor(levels(y)[as.numeric(fitted.raw$class)], levels = levels(y))
  error.train <- mod_error(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted.raw <- predict(mod, x.test, type = "both")
    predicted.prob <- predicted.raw$probs
    predicted <- factor(
      levels(y)[as.numeric(predicted.raw$class)],
      levels = levels(y)
    )
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(
    fitted.prob = fitted.prob,
    predicted.prob = predicted.prob
  )
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    xnames = xnames,
    type = "Classification",
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

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_AdaBoost
