# s_LINOA.R
# ::rtemis::
# 2019-20 E.D. Gennatas rtemis.org
# Allow early stopping
# LINOA -> shyoptleaves -> splitlin_ -> splitline -> cutnsplit

#' Linear Optimized Additive Tree (C, R)
#'
#' Train a Linear Optimized Additive Tree
#'
#' The Linear Optimized Additive Tree grows a tree by finding splits that minimize loss after linear
#' models are fit on each child.
#' We specify an upper threshold of leaves using `max.leaves` instead of directly defining a number,
#' because depending on the other parameters and the datasets, splitting may stop early.
#'
#' @inheritParams s_GLM
#' @param max.leaves Integer: Maximum number of terminal nodes to grow
#' @param nvmax \[gS\] Integer: Number of max features to use for lin.type "allSubsets", "forwardStepwise", or
#' "backwardStepwise". If values greater than n of features in `x` are provided, they will be excluded
#' @param lookback Logical: If TRUE, check validation error to decide when to stop growing tree. Default = FALSE
#' @param init Initial value. Default = `mean(y)`
#' @param lambda Float: lambda parameter for `MASS::lm.ridge` Default = .01
#' @param minobsinnode Integer: Minimum N observations needed in node, before considering splitting
#' @param .gs internal use only
#' @param plot.tuning Logical: If TRUE, plot validation error during gridsearch
#' @author E.D. Gennatas
#' @export

s_LINOA <- function(
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
  max.leaves = 8,
  learning.rate = .5,
  select.leaves.smooth = TRUE,
  force.max.leaves = NULL,
  lookback = TRUE,
  # splitline
  gamma = 0,
  n.quantiles = 20,
  minobsinnode = NULL, # set based on y
  minbucket = NULL, # set based on y
  lin.type = c(
    "forwardStepwise",
    "glmnet",
    "cv.glmnet",
    "lm.ridge",
    "allSubsets",
    "backwardStepwise",
    "glm",
    "solve",
    "none"
  ),
  alpha = 1,
  lambda = .05,
  lambda.seq = NULL,
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = "lambda.min",
  nbest = 1,
  nvmax = 3,
  # /splitline
  .rho = TRUE,
  rho.max = 1000,
  init = NULL,
  metric = "auto",
  maximize = NULL,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  save.gridrun = FALSE,
  grid.verbose = verbose,
  keep.x = FALSE,
  simplify = TRUE,
  cxrcoef = FALSE,
  n.cores = rtCores, # for gridSearchLearn
  splitline.cores = 1, # for splitline, i.e. searching for cutpoints
  .preprocess = NULL,
  plot.tuning = TRUE,
  verbose.predict = FALSE,
  x.name = NULL,
  y.name = NULL,
  question = NULL,
  outdir = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  save.mod = FALSE,
  .gs = FALSE,
  verbose = TRUE,
  trace = 1
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_LINOA))
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
  mod.name <- "LINOA"
  # delta 02.06.2020
  # if (max.leaves <= 1) force.max.leaves <- 1

  # Dependencies ----
  # ENH: deps for lincoef
  dependency_check("glmnet", "rpart")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  lin.type <- match.arg(lin.type)
  # if (.gs && nvmax == 0) lin.type = "none"

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
    .preprocess = .preprocess,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  x0 <- if (upsample) dt$x0 else x
  y0 <- if (upsample) dt$y0 else y
  # .classwt <- if (is.null(classwt) & ifw) dt$class.weights else classwt
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose)
    parameterSummary(
      gamma,
      lambda,
      minobsinnode,
      learning.rate,
      max.leaves,
      nvmax,
      newline.pre = TRUE
    )
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(init)) {
    init <- if (type == "Classification") 0 else mean(y)
  }
  init <- if (type == "Classification") 0 else mean(y)
  if (type == "Classification" && length(levels(y)) != 2)
    stop("s_LINOA currently supports only binary classification")

  if (is.null(minobsinnode)) minobsinnode <- round(.1 * length(y))
  if (is.null(minbucket)) minbucket <- round(.05 * length(y))

  loss.fn <- if (type == "Classification") class.loss else mse

  if (!is.null(force.max.leaves)) lookback <- FALSE

  # Remove nvmax values that are greater than N features
  if (lin.type %in% c("allSubsets", "forwardStepwise", "backwardStepwise")) {
    nvmax <- Filter(function(z) z <= NCOL(x), nvmax)
  }

  # Grid Search ----
  if (metric == "auto") {
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
  if (is.null(force.max.leaves)) {
    if (lookback) {
      gc <- gridCheck(
        gamma,
        lambda,
        learning.rate,
        nvmax,
        minobsinnode,
        minbucket
      )
    } else {
      gc <- gridCheck(
        gamma,
        lambda,
        learning.rate,
        max.leaves,
        nvmax,
        minobsinnode,
        minbucket
      )
    }
  } else {
    gc <- FALSE
  }

  # Must turn off plotting during parallel grid search or else it may hang
  if (!.gs && n.cores > 1) plot.tuning <- FALSE

  if ((!.gs && gc) || (!.gs && lookback && max.leaves > 1)) {
    grid.params <- if (lookback) list() else list(max.leaves = max.leaves)
    grid.params <- c(
      grid.params,
      list(
        nvmax = nvmax,
        gamma = gamma,
        lambda = lambda,
        learning.rate = learning.rate,
        minobsinnode = minobsinnode,
        minbucket = minbucket
      )
    )
    fixed.params <- if (lookback) list(max.leaves = max.leaves) else list()
    fixed.params <- c(
      fixed.params,
      list(
        init = init,
        n.quantiles = n.quantiles,
        lin.type = lin.type,
        lambda.seq = lambda.seq,
        cv.glmnet.nfolds = cv.glmnet.nfolds,
        which.cv.glmnet.lambda = which.cv.glmnet.lambda,
        nbest = nbest,
        metric = metric,
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        resample.seed = resample.seed,
        plot.tuning = plot.tuning,
        n.cores = splitline.cores,
        .gs = TRUE
      )
    )

    gs <- gridSearchLearn(
      x = x0,
      y = y0,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = grid.params,
      fixed.params = fixed.params,
      search.type = gridsearch.type,
      weights = weights,
      metric = metric,
      maximize = maximize,
      save.mod = save.gridrun,
      verbose = verbose,
      grid.verbose = grid.verbose,
      n.cores = n.cores
    )

    # lambda, minobsinnode, learning.rate, part.cp
    gamma <- gs$best.tune$gamma
    lambda <- gs$best.tune$lambda
    learning.rate <- gs$best.tune$learning.rate
    nvmax <- gs$best.tune$nvmax
    # max.leaves is a return special from gridSearchLearn
    max.leaves <- gs$best.tune$n.leaves
    minobsinnode <- gs$best.tune$minobsinnode
    minbucket <- gs$best.tune$minbucket

    # if (n.trees == -1) {
    #   warning("Tuning failed to find n.trees, defaulting to failsafe.trees = ", failsafe.trees)
    #   n.trees <- failsafe.trees
    # }
    # if (n.trees < min.trees) {
    #   warning("Tuning returned ", n.trees, " trees; using min.trees = ", min.trees, " instead")
    #   n.trees <- min.trees
    # }

    # Now ready to train final full model
    # .final <- TRUE
    .gs <- FALSE
    lookback <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.max.leaves)) max.leaves <- force.max.leaves

  # shyoptleaves ----
  if (.gs) {
    if (lookback) {
      x.valid <- x.test
      y.valid <- y.test
    } else {
      x.valid <- y.valid <- NULL
    }
  } else {
    x.valid <- y.valid <- NULL
    msg2("Training LINOA on full training set...", newline = TRUE)
  }

  if (length(nvmax) == 1 && nvmax == 0) lin.type <- "none"
  mod <- shyoptleaves(
    x,
    y,
    x.valid = x.valid,
    y.valid = y.valid,
    lookback = lookback,
    weights = .weights,
    max.leaves = max.leaves,
    learning.rate = learning.rate,
    select.leaves.smooth = select.leaves.smooth,
    gamma = gamma,
    n.quantiles = n.quantiles,
    minobsinnode = minobsinnode,
    minbucket = minbucket,
    # --lincoef
    lin.type = lin.type,
    alpha = alpha,
    lambda = lambda,
    lambda.seq = lambda.seq,
    cv.glmnet.nfolds = cv.glmnet.nfolds,
    which.cv.glmnet.lambda = which.cv.glmnet.lambda,
    nbest = nbest,
    nvmax = nvmax,
    # /--lincoef
    .rho = .rho,
    rho.max = rho.max,
    loss.fn = loss.fn,
    verbose = verbose,
    plot.tuning = plot.tuning,
    trace = trace,
    n.cores = n.cores
  )

  parameters <- list(
    max.leaves = max.leaves,
    n.leaves = mod$n.leaves,
    alpha = alpha,
    lambda = lambda,
    lambda.seq = lambda.seq,
    learning.rate = learning.rate,
    minobsinnode = minobsinnode,
    minbucket = minbucket,
    weights = .weights,
    init = init,
    lin.type = lin.type,
    cv.glmnet.nfolds = cv.glmnet.nfolds,
    which.cv.glmnet.lambda = which.cv.glmnet.lambda
  )

  # Fitted ----
  if (type == "Classification") {
    .fitted <- predict.shyoptleaves(mod, x, type = "all")
    fitted <- .fitted$estimate
    fitted.prob <- .fitted$probability
  } else {
    fitted <- predict.shyoptleaves(mod, x)
    fitted.prob <- NULL
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train)

  # Predicted ----
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Classification") {
      .predicted <- predict.shyoptleaves(
        mod,
        x.test,
        type = "all",
        learning.rate = learning.rate,
        trace = trace,
        verbose = verbose.predict
      )
      predicted <- .predicted$estimate
      predicted.prob <- .predicted$probability
    } else {
      predicted <- predict.shyoptleaves(
        mod,
        x.test,
        learning.rate = learning.rate,
        trace = trace,
        verbose = verbose.predict
      )
      predicted.prob <- NULL
    }

    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # Outro ----
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
} # rtemis:: s_LINOA

# Unfinished
# shyoptree.optimal.leaves <- function(object,
#                                    smooth = TRUE,
#                                    plot = FALSE,
#                                    verbose = FALSE) {
#   n.leaves <- NROW(object$leaves$rules)
#
#   # if (smooth) {
#   # dat <- data.frame(n.trees = seq(n.trees), valid.error = object$valid.error)
#   # dat <- complete.cases(dat)
#   # }
#
#   valid.error.smooth <- if (smooth) {
#     valid.error.smooth <- supsmu(seq(n.leaves), object$valid.error)$y
#   } else {
#     NULL
#   }
#   valid.error <- if (smooth) valid.error.smooth else object$valid.error
#
#   if (plot) mplot3_xy(seq(n.trees), list(Training = object$train.error,
#                                          Validation = object$valid.error,
#                                          `Smoothed Validation` = valid.error.smooth),
#                       type = 'l', group.adj = .95,
#                       line.col = c(ucsfCol$teal, ucsfCol$red, ucsfCol$purple),
#                       vline = c(which.min(object$valid.error), which.min(valid.error.smooth)),
#                       vline.col = c(ucsfCol$red, ucsfCol$purple),
#                       xlab = "N trees", ylab = "Loss")
#
#   list(n.trees = which.min(valid.error),
#        valid.error.smooth = valid.error.smooth)
#
# } # rtemis::shyoptree.optimal.leaves
