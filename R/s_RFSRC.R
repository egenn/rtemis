# s_RFSRC.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Random Forest for Classification, Regression, and Survival \[C, R, S\]
#'
#' Train a Random Forest for Regression, Classification, or Survival Regression
#' using `randomForestSRC`
#'
#' For Survival Regression, y must be an object of type `Surv`, created using
#' `survival::Surv(time, status)`
#' `mtry` is the only tunable parameter, but it usually only makes a small difference
#' and is often not tuned.
#'
#' @inheritParams s_CART
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as `x`
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param n.trees Integer: Number of trees to grow. The more the merrier.
#' @param bootstrap Character:
#' @param mtry Integer: Number of features sampled randomly at each split
#' @param importance Logical: If TRUE, calculate variable importance.
#' @param proximity Character or Logical: "inbag", "oob", "all", TRUE, or FALSE; passed
#' to `randomForestSRC::rfsrc`
#' @param nodesize Integer: Minimum size of terminal nodes.
#' @param nodedepth Integer: Maximum tree depth.
#' @param na.action Character: How to handle missing values.
#' @param trace Integer: Number of seconds between messages to the console.
#' @param outdir Optional. Path to directory to save output
#' @param ... Additional arguments to be passed to `randomForestSRC::rfsrc`
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_RFSRC <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  n.trees = 1000,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  bootstrap = "by.root",
  mtry = NULL,
  importance = TRUE,
  proximity = TRUE,
  nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
  nodedepth = NULL,
  na.action = "na.impute",
  trace = FALSE,
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
    print(args(s_RFSRC))
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
  mod.name <- "RFSRC"

  # Dependencies ----
  dependency_check("randomForestSRC")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_RFSRC))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (is.null(trace)) trace <- if (verbose) n.trees / 10 else FALSE

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
  if (is.null(weights) && ifw) weights <- dt$weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry, pad = 4, newline.pre = TRUE)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(nodesize)) nodesize <- if (type == "Classification") 1 else 5

  # Formula ----
  if (type != "Survival") {
    df.train <- data.frame(y, x)
    colnames(df.train)[1] <- y.name
    .formula <- as.formula(paste(y.name, "~ ."))
  } else {
    time <- y[, 1]
    status <- y[, 2]
    df.train <- data.frame(time, status, x)
    # .formula <- as.formula(paste(y.name, "~ ."))
    .formula <- as.formula(Surv(time, status) ~ .)
  }

  # randomForestSRC::rfsrc ----
  if (verbose) {
    msg2(
      "Training Random Forest SRC",
      type,
      "with",
      n.trees,
      "trees...",
      newline.pre = TRUE
    )
  }
  mod <- randomForestSRC::rfsrc(
    .formula,
    data = df.train,
    ntree = n.trees,
    bootstrap = bootstrap,
    mtry = mtry,
    case.wt = weights,
    nodesize = nodesize,
    nodedepth = nodedepth,
    importance = importance,
    proximity = proximity,
    na.action = na.action,
    do.trace = trace,
    ...
  )

  # Fitted ----
  if (proximity) {
    fit <- predict(mod, x, proximity = TRUE)
    fitted <- fit$predicted
    proximity.train <- fit$proximity
  } else {
    fit <- predict(mod, x)
    fitted <- fit$predicted
    proximity.train <- NULL
  }

  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- factor(apply(fitted.prob, 1, function(i) which.max(i)))
    levels(fitted) <- levels(y)
  } else {
    fitted <- as.numeric(fitted)
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  if (!is.null(x.test)) {
    if (proximity) {
      pred <- predict(mod, x.test, proximity = proximity)
      predicted <- pred$predicted
      proximity.test <- pred$proximity
    } else {
      pred <- predict(mod, x.test)
      predicted <- pred$predicted
      proximity.test <- NULL
    }

    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- factor(apply(predicted.prob, 1, function(i) which.max(i)))
      levels(predicted) <- levels(y)
    } else {
      predicted <- as.numeric(predicted)
    }

    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    pred <- predicted <- error.test <- proximity.test <- NULL
  }

  # Outro ----
  extra <- list(
    fit = fit,
    proximity.test = proximity.test,
    proximity.train = proximity.train
  )
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
} # rtemis::s_RFSRC
