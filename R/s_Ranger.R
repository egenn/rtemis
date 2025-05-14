# s_Ranger.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org
# fix multiclass prob with probability F, predict with probability T
# TODO: Add Survival support
# TODO: Use inbag and resample for stratified bootstraps

#' Random Forest Classification and Regression (C, R)
#'
#' Train a Random Forest for regression or classification using `ranger`
#'
#' You should cconsider, or try, setting mtry to NCOL(x), especially for small number of features.
#' By default mtry is set to NCOL(x) for NCOL(x) <= 20.
#' For imbalanced datasets, setting stratify.on.y = TRUE should improve performance.
#' If `autotune = TRUE`, `randomForest::tuneRF` will be run to determine best `mtry`
#'   value.
#' \[gS\]: indicated parameter will be tuned by grid search if more than one value is passed
#'
#' See [Tech Report](https://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf) comparing
#' balanced (ifw.case.weights = TRUE) and weighted (ifw.class.weights = TRUE) Random Forests.
#'
#' @inheritParams s_RF
#' @inheritParams s_CART
#' @param n.trees Integer: Number of trees to grow. Default = 1000
#' @param mtry \[gS\] Integer: Number of features sampled randomly at each split.
#'  Defaults to square root of n of
#' features for classification, and a third of n of features for regression.
#' @param min.node.size \[gS\] Integer: Minimum node size
#' @param splitrule Character: For classification: "gini" (Default) or
#' "extratrees";
#' For regression: "variance" (Default), "extratrees" or "maxstat".
#' For survival "logrank" (Default), "extratrees", "C" or "maxstat".
#' @param ifw.case.weights Logical: If TRUE, define ranger's
#' `case.weights` using IPW. Default = TRUE
#' Note: Cannot use case.weights together with `stratify.on.y` or
#' `inbag.resample`
#' @param ifw.class.weights Logical: If TRUE, define ranger's
#' `class.weights` using IPW. Default = FALSE
#' @param probability Logical: If TRUE, grow a probability forest.
#' See `ranger::ranger`. Default = FALSE
#' @param importance Character: "none", "impurity", "impurity_corrected", or
#' "permutation"
#' Default = "impurity"
#' @param local.importance Logical: If TRUE, return local importance values.
#' Only applicable if
#' `importance` is set to "permutation".
#' @param classwt Vector, Float: Priors of the classes for
#' `randomForest::tuneRF` if `autotune = TRUE`.
#' For classification only; need not add up to 1
#' @param inbag.resample List, length `n.tree`: Output of
#' [setup.resample] to define resamples used for each
#' tree. Default = NULL
#' @param stratify.on.y Logical: If TRUE, overrides `inbag.resample` to
#' use stratified bootstraps for each tree.
#' This can help improve test set performance in imbalanced datasets.
#' Default = FALSE. Note: Cannot be used with `ifw.case.weights`
#' @param ... Additional arguments to be passed to `ranger::ranger`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Ensembles
#' @export

s_Ranger <- function(
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
  ifw.case.weights = TRUE,
  ifw.class.weights = FALSE,
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
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  metric = NULL,
  maximize = NULL,
  probability = NULL,
  importance = "impurity",
  local.importance = FALSE,
  replace = TRUE,
  min.node.size = NULL,
  splitrule = NULL,
  strata = NULL,
  sampsize = if (replace) nrow(x) else ceiling(.632 * nrow(x)),
  tune.do.trace = FALSE,
  imetrics = FALSE,
  n.cores = rtCores,
  print.tune.plot = FALSE,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  grid.verbose = verbose,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_Ranger))
    return(invisible(9))
  }
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  }
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
  mod.name <- "Ranger"

  # Dependencies ----
  dependency_check("ranger")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_Ranger))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  gridsearch.type <- match.arg(gridsearch.type)

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
  # Default to probability trees in Classification
  if (is.null(probability) && type == "Classification") {
    probability <- TRUE
  } else {
    probability <- FALSE
  }
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry, newline.pre = TRUE)
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  .class.weights <- if (is.null(classwt) && ifw) dt$class.weights else classwt
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
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
  n.features <- NCOL(x)
  if (is.null(mtry)) {
    if (n.features <= 20) mtry <- n.features
    mtry <- if (type == "Classification") {
      floor(sqrt(n.features))
    } else {
      max(floor(n.features / 3), 1)
    }
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

  # Formula ----
  df.train <- data.frame(x, y)
  colnames(df.train)[ncol(df.train)] <- y.name
  .formula <- as.formula(paste(y.name, "~ ."))

  # Grid Search ----
  if (gridCheck(mtry, min.node.size)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        mtry = mtry,
        min.node.size = min.node.size
      ),
      fixed.params = list(
        n.trees = n.trees,
        replace = replace,
        importance = "none",
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        resample.seed = resample.seed
      ),
      search.type = gridsearch.type,
      randomized.p = gridsearch.randomized.p,
      weights = weights,
      metric = metric,
      maximize = maximize,
      verbose = grid.verbose,
      n.cores = 1
    )
    mtry <- gs$best.tune$mtry
    min.node.size <- gs$best.tune$min.node.size
  } else {
    gs <- NULL
  }

  # In case tuning fails, use defaults
  if (length(mtry) == 0) {
    warning("Tuning failed; setting mtry to default")
    mtry <- if (type == "Classification") {
      floor(sqrt(n.features))
    } else {
      max(floor(n.features / 3), 1)
    }
  }

  if (length(min.node.size) == 0) {
    min.node.size <- if (type == "Classification") 1 else 5
  }

  # tuneRF ----
  if (is.null(mtryStart)) mtryStart <- sqrt(n.features)
  if (autotune) {
    if (verbose) msg2("Tuning for mtry...")
    tuner <- try(randomForest::tuneRF(
      x = x,
      y = y,
      mtryStart = mtryStart,
      ntreeTry = n.trees.try,
      stepFactor = stepFactor,
      trace = verbose,
      plot = print.tune.plot,
      strata = strata,
      do.trace = tune.do.trace,
      classwt = .class.weights
    ))
    if (!inherits(tuner, "try-error")) {
      mtry <- tuner[which.min(tuner[, 2]), 1]
      if (verbose) msg2("Best mtry :", mtry)
    } else {
      msg2("tuneRF failed; reverting to mtry =", mtry)
    }
  }
  parameters <- list(
    n.trees = n.trees,
    mtry = mtry,
    ifw = ifw,
    ifw.type = ifw.type,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed,
    probability = probability
  )

  # Ranger ----
  if (stratify.on.y) {
    inbag.resample <- setup.resample("strat.boot", n.trees)
  }
  if (!is.null(inbag.resample)) {
    if (verbose) msg2("Creating custom subsamples...")
    # Get integer index of inbag cases
    inbag.res <- resample(
      df.train$y,
      rtset = inbag.resample,
      verbosity = as.integer(verbose)
    )
    # Convert to counts for each case
    inbag <- lapply(
      seq(n.trees),
      function(i) {
        sapply(
          seq(df.train$y),
          function(j) sum(j == inbag.res[[i]])
        )
      }
    )
  } else {
    inbag <- NULL
  }
  if (verbose) {
    msg2(
      "Training Random Forest (ranger)",
      type,
      "with",
      n.trees,
      "trees...",
      newline.pre = TRUE
    )
  }
  mod <- ranger::ranger(
    formula = .formula,
    data = df.train,
    num.trees = n.trees,
    case.weights = if (ifw.case.weights) .weights else NULL,
    class.weights = if (ifw.case.weights) .class.weights else NULL,
    mtry = mtry,
    min.node.size = min.node.size,
    splitrule = splitrule,
    replace = replace,
    probability = probability,
    importance = importance,
    local.importance = local.importance,
    write.forest = TRUE,
    inbag = inbag,
    num.threads = n.cores,
    verbose = verbose,
    ...
  )

  # Fitted ----
  if (type == "Classification") {
    if (!probability) {
      fitted.all <- predict(mod, x, predict.all = TRUE)
      # fitted.freq <- apply(
      #     fitted.all$predictions, 1,
      #     \(n) sum(n == 1)
      # )
      fitted.freq <- t(apply(
        fitted.all$predictions,
        1,
        \(i) {
          one <- factor(i, levels = seq_len(nlevels))
          as.numeric(table(one))
        }
      ))
      fitted.prob <- fitted.freq / n.trees
      if (nlevels == 2) fitted.prob <- fitted.prob[, 1]
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
  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- NULL
  if (!is.null(x.test)) {
    if (type == "Classification") {
      if (!probability) {
        predicted.all <- predict(mod, x.test, predict.all = TRUE)
        predicted.freq <- apply(
          predicted.all$predictions,
          1,
          function(n) sum(n == 1)
        )
        predicted.prob <- predicted.freq / n.trees
        predicted <- predict(mod, x.test)$predictions
      } else {
        predicted.prob <- predict(mod, x.test)$predictions
        predicted <- factor(apply(
          as.data.frame(predicted.prob),
          1,
          which.max
        ))
        predicted.prob <- predicted.prob[, 1]
        levels(predicted) <- levels(y)
      }
    } else {
      predicted <- predict(mod, x.test)$predictions
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- error.test <- NULL
  }

  # Outro ----
  extra <- list()

  if (imetrics) {
    n.nodes <- sum(plyr::ldply(
      mod$forest$child.nodeIDs,
      function(t) plyr::ldply(t, function(n) length(unlist(n)))
    )[, 1])
    extra$imetrics <- list(n.trees = n.trees, n.nodes = n.nodes)
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
    fitted.prob = fitted.prob,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    predicted.prob = predicted.prob,
    se.prediction = NULL,
    error.test = error.test,
    varimp = if (importance != "none") as.matrix(mod$variable.importance) else
      NULL,
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
} # rtemis::s_Ranger
