# s_KNN.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org
# TODO: Consider replacing knn fn
# FNN's KNN does not have a predict function

#' k-Nearest Neighbors Classification and Regression (C, R)
#'
#' Train a k-Nearest Neighbors learner for regression or classification using `FNN`
#'
#' @inheritParams s_CART
#' @param k Integer: Number of neighbors considered
#' @param algorithm Character: Algorithm to use. Options: "kd_tree", "cover_tree", "brute"
#' @param outdir Optional. Path to directory to save output
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_KNN <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  k = 3,
  algorithm = "kd_tree",
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE)
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_KNN))
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
  mod.name <- "KNN"

  # Dependencies ----
  dependency_check("FNN")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_KNN))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(k, algorithm, newline.pre = TRUE)

  # FNN::knn/knn.reg ----
  if (verbose)
    msg2("Running k-Nearest Neighbors", type, "...", newline.pre = TRUE)
  .x.test <- if (is.null(x.test)) x else x.test
  if (type == "Classification") {
    mod <- FNN::knn(
      train = x,
      test = .x.test,
      cl = y,
      k = k,
      prob = FALSE,
      algorithm = algorithm
    )
  } else {
    mod <- FNN::knn.reg(
      train = x,
      test = .x.test,
      y = y,
      k = k,
      algorithm = algorithm
    )
  }

  # Fitted / Predicted ----
  # TODO: write & incorporate predict.knn / replace KNN fn
  if (type == "Classification") {
    if (is.null(x.test)) {
      fitted <- factor(mod)
      error.train <- mod_error(y, fitted)
      if (verbose) errorSummary(error.train, mod.name)
      predicted <- NULL
      error.test <- NULL
    } else {
      fitted <- NULL
      error.train <- NULL
      predicted <- factor(mod)
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  } else {
    if (is.null(x.test)) {
      fitted <- mod$pred
      error.train <- mod_error(y, fitted)
      if (verbose) errorSummary(error.train, mod.name)
      predicted <- NULL
      error.test <- NULL
    } else {
      fitted <- NULL
      error.train <- NULL
      predicted <- mod$pred
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list()
  rt <- rtModSet(
    mod = mod,
    mod.name = mod.name,
    type = type,
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

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_KNN
