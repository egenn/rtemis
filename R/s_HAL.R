# s_HAL.R
# ::rtemis::
# 2023 E.D. Gennatas rtemis.org
# Work in progress

#' Highly Adaptive LASSO \[C, R, S\]
#'
#' Train a HAL model
#'
#' `\[gS\]` Indicates tunable hyperparameters: If more than a single value is provided,
#' grid search will be automatically performed
#'
#' @inheritParams s_GLM
#' @inheritParams s_CART
#' @param lambda Float vector: [hal9001::fit_hal] lambda
#' @param max.degree Integer: The highest order of interaction terms to generate basis
#' functions for.
#' @param .gs Internal use only
#'
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_HAL <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  family = NULL,
  max.degree = ifelse(ncol(x) >= 20, 2, 3),
  lambda = NULL,
  x.name = NULL,
  y.name = NULL,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  #  weights = NULL,
  #  ifw = TRUE,
  #  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  metric = NULL,
  maximize = NULL,
  .gs = FALSE,
  n.cores = rtCores,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_HAL))
    return(invisible(9))
  }
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  }
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "HAL"

  # Dependencies ----
  dependency_check("hal9001")

  # Arguments ----
  if (missing(x)) {
    print(args(s_HAL))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_HAL))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose || !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  }
  gridsearch.type <- match.arg(gridsearch.type)

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    # ifw = ifw,
    # ifw.type = ifw.type,
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
  # .weights <- if (is.null(weights) && ifw) dt$weights else weights
  # if (is.null(.weights)) .weights <- rep(1, NROW(y))
  if (is.null(family)) {
    if (type == "Regression") {
      family <- "gaussian"
    } else if (type == "Classification") {
      # family <- if (length(levels(y)) == 2) "binomial" else "multinomial"
      family <- "binomial"
    } else if (type == "Survival") {
      family <- "cox"
    }
  } else {
    if (family %in% c("binomial", "multinomial") && type != "Classification") {
      y <- factor(y)
      if (!is.null(y.test)) y.test <- factor(y.test)
      type <- "Classification"
    }
  }
  # Cox does not have an intercept (it is part of the baseline hazard)
  # if (type == "Survival") intercept <- FALSE
  if (verbose) dataSummary(x, y, x.test, y.test, type)

  if (
    !is.null(family) &&
      family %in% c("binomial", "multinomial") &&
      !is.factor(y)
  ) {
    if (type == "Survival") {
      colnames(y) <- c("time", "status")
      if (!is.null(y.test)) colnames(y.test) <- c("time", "status")
    }
  }
  if (print.plot) {
    if (is.null(plot.fitted)) {
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    }
    if (is.null(plot.predicted)) {
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
    }
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Model matrix ----
  dat <- data.frame(x, y = y)
  .formula <- as.formula("y ~ .")
  x <- model.matrix(.formula, dat)[, -1]

  if (!is.null(x.test)) {
    # for model.matrix to work, add y if not provided
    y.test1 <- if (is.null(y.test)) sample(y, NROW(x.test)) else y.test
    dat.test <- data.frame(x.test, y = y.test1)
    x.test <- model.matrix(.formula, dat.test)[, -1]
  }

  # Grid Search ----
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
  # cv.lambda <- is.null(lambda)
  # do.gs <- is.null(lambda) | length(alpha) > 1 | length(lambda) > 1
  # do.gs <- FALSE
  # if (!.gs && do.gs) {
  if (gridCheck(max.degree)) {
    gs <- gridSearchLearn(
      x,
      y,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        max.degree = max.degree
      ),
      fixed.params = list(
        lambda = lambda,
        .gs = TRUE
        # which.cv.lambda = which.cv.lambda
      ),
      search.type = gridsearch.type,
      randomized.p = gridsearch.randomized.p,
      weights = weights,
      metric = metric,
      maximize = maximize,
      verbose = verbose,
      n.cores = n.cores
    )
    # lambda <- gs$best.tune$lambda
    max.degree <- gs$best.tune$max.degree
  } else {
    gs <- NULL
  }
  if (verbose) {
    parameterSummary(lambda, newline.pre = TRUE)
  }

  # fit_hal ----
  if (verbose) msg2("Training Highly Adaptive LASSO...", newline.pre = TRUE)
  mod <- hal9001::fit_hal(
    X = x,
    Y = as.numeric(y),
    family = family,
    max_degree = max.degree,
    lambda = lambda,
    ...
  )

  # Fitted ----
  if (type == "Regression" || type == "Survival") {
    fitted <- as.numeric(predict(mod, x))
    fitted.prob <- NULL
  } else {
    fitted.prob <- 1 - predict(mod, x, type = "response")
    fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
    levels(fitted) <- levels(y)
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression" || type == "Survival") {
      predicted <- as.numeric(predict(mod, x.test))
      predicted.prob <- NULL
    } else {
      predicted.prob <- 1 - predict(mod, x.test, type = "response")
      predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(predicted) <- levels(y)
    }

    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    gridsearch = gs,
    parameters = list(lambda = lambda),
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
    varimp = NULL,
    question = question
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
} # rtemis::s_HAL
