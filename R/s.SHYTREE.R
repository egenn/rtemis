# s.SHYTREE.R
# ::rtemis::
# 2019-20 Efstathios D Gennatas egenn.github.io
# Allow early stopping

#' Stepwise Hybrid Tree [C, R]
#'
#' Train a Stepwise Hybrid Tree for Regression or Binary Classification
#'
#' The Stepwise Hybrid Tree grows a tree using a sequence of regularized linear models and tree stumps
#' We specify an upper threshold of leaves using \code{max.leaves} instead of directly defining a number,
#' because depending on the other parameters and the datasets, splitting may stop early.
#'
#' @inheritParams s.GLM
#' @param max.leaves Integer: Maximum number of terminal nodes to grow
#' @param nvmax [gS] Integer: Number of max features to use for lin.type "allSubsets", "forwardStepwise", or
#' "backwardStepwise". If values greater than n of features in \code{x} are provided, they will be excluded
#' @param early.stopping Logical: If TRUE, check validation error to decide when to stop growing tree. Default = FALSE
#' @param init Initial value. Default = \code{mean(y)}
#' @param lambda Float: lambda parameter for \code{MASS::lm.ridge} Default = .01
#' @param minobsinnode Integer: Minimum N observations needed in node, before considering splitting
#' @param part.max.depth Integer: Max depth for each tree model within the additive tree
#' @param .gs internal use only
#' @param plot.tune.error Logical: If TRUE, plot validation error during gridsearch
#' @author Efstathios D. Gennatas
#' @export

s.SHYTREE <- function(x, y = NULL,
                      x.test = NULL, y.test = NULL,
                      weights = NULL,
                      ipw = TRUE,
                      ipw.type = 2,
                      upsample = FALSE,
                      downsample = FALSE,
                      resample.seed = NULL,
                      max.leaves = 6,
                      nvmax = 2,
                      force.max.leaves = NULL,
                      early.stopping = TRUE,
                      gamma = .1,
                      alpha = 1,
                      lambda = .05,
                      lambda.seq = NULL,
                      minobsinnode = 2,
                      minobsinnode.lin = 10,
                      learning.rate = 1,
                      part.minsplit = 2,
                      part.xval = 0,
                      part.max.depth = 1,
                      part.cp = 0,
                      part.minbucket = 5,
                      .rho = TRUE,
                      rho.max = 1000,
                      init = NULL,
                      lin.type = c("glmnet", "cv.glmnet", "lm.ridge", "allSubsets",
                                   "forwardStepwise", "backwardStepwise", "glm",
                                   "solve", "none"),
                      cv.glmnet.nfolds = 5,
                      cv.glmnet.lambda = "lambda.min",
                      metric = "auto",
                      maximize = NULL,
                      grid.resample.rtset = rtset.grid.resample(),
                      grid.search.type = "exhaustive",
                      save.gridrun = FALSE,
                      grid.verbose = TRUE,
                      keep.x = FALSE,
                      simplify = TRUE,
                      cxrcoef = FALSE,
                      n.cores = rtCores,
                      .preprocess = NULL,
                      verbose = TRUE,
                      plot.tune.error = FALSE,
                      verbose.predict = FALSE,
                      trace = 0,
                      x.name = NULL,
                      y.name = NULL,
                      question = NULL,
                      outdir = NULL,
                      print.plot = TRUE,
                      plot.fitted = NULL,
                      plot.predicted = NULL,
                      plot.theme = getOption("rt.fit.theme", "lightgrid"),
                      save.mod = FALSE,
                      .gs = FALSE) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.SHYTREE))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "SHYTREE"
  # delta 02.06.2020
  # if (max.leaves <= 1) force.max.leaves <- 1

  # [ DEPENDENCIES ] ====
  # ENH: deps for lincoef
  if (!depCheck("glmnet", "rpart", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  lin.type <- match.arg(lin.type)
  # if (.gs && nvmax == 0) lin.type = "none"

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    .preprocess = .preprocess,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  x0 <- if (upsample) dt$x0 else x
  y0 <- if (upsample) dt$y0 else y
  # .classwt <- if (is.null(classwt) & ipw) dt$class.weights else classwt
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(gamma, lambda, minobsinnode, learning.rate, part.cp, max.leaves, nvmax)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(init)) {
    init <- if (type == "Classification") 0 else mean(y)
  }
  init <- if (type == "Classification") 0 else mean(y)
  if (type == "Classification" && length(levels(y)) != 2)
    stop("s.SHYTREE currently supports only binary classification")

  if (!is.null(force.max.leaves)) early.stopping <- FALSE

  # Remove nvmax values that are greater than N features
  if (lin.type %in% c("allSubsets", "forwardStepwise", "backwardStepwise")) {
    nvmax <- Filter(function(z) z <= NCOL(x), nvmax)
  }

  # [ GRID SEARCH ] ====
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
    if (early.stopping) {
      gc <- gridCheck(gamma, lambda, minobsinnode, learning.rate, part.cp, nvmax)
    } else {
      gc <- gridCheck(gamma, lambda, minobsinnode, learning.rate, part.cp, max.leaves, nvmax)
    }
  } else {
    gc <- FALSE
  }


  if (n.cores > 1) plot.tune.error <- FALSE
  # if (!.gs && (gc | is.null(force.max.leaves))) {
  if ((!.gs && gc) | (!.gs && early.stopping && max.leaves > 1)) {
    grid.params <- if (early.stopping) list() else list(max.leaves = max.leaves)
    grid.params <- c(grid.params, list(nvmax = nvmax,
                                       gamma = gamma,
                                       lambda = lambda,
                                       minobsinnode = minobsinnode,
                                       learning.rate = learning.rate,
                                       part.cp = part.cp))
    fixed.params <- if (early.stopping) list(max.leaves = max.leaves) else list()
    fixed.params <- c(fixed.params, list(init = init,
                                         lin.type = lin.type,
                                         cv.glmnet.nfolds = cv.glmnet.nfolds,
                                         cv.glmnet.lambda = cv.glmnet.lambda,
                                         metric = metric,
                                         ipw = ipw,
                                         ipw.type = ipw.type,
                                         upsample = upsample,
                                         resample.seed = resample.seed,
                                         plot.tune.error = plot.tune.error,
                                         .gs = TRUE))

    gs <- gridSearchLearn(x = x0, y = y0,
                          mod = mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = grid.params,
                          fixed.params = fixed.params,
                          search.type = grid.search.type,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          save.mod = save.gridrun,
                          verbose = verbose,
                          grid.verbose = grid.verbose,
                          n.cores = n.cores)

    # lambda, minobsinnode, learning.rate, part.cp
    gamma <- gs$best.tune$gamma
    lambda <- gs$best.tune$lambda
    minobsinnode <- gs$best.tune$minobsinnode
    learning.rate <- gs$best.tune$learning.rate
    part.cp <- gs$best.tune$part.cp
    nvmax <- gs$best.tune$nvmax
    # Return special from gridSearchLearn
    max.leaves <- gs$best.tune$n.leaves

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
    early.stopping <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.max.leaves)) max.leaves <- force.max.leaves

  # [ shytreeLeaves ] ====
  if (.gs) {
    if (early.stopping) {
      x.valid <- x.test
      y.valid <- y.test
    } else {
      x.valid <- y.valid <- NULL
    }
  } else {
    x.valid <- y.valid <- NULL
    msg("Training SHYTREE on full training set...", newline = TRUE)
  }

  if (length(nvmax) == 1 && nvmax == 0) lin.type <- "none"
  mod <- shytreeLeavesRC(x, y,
                         x.valid = x.valid, y.valid = y.valid,
                         early.stopping = early.stopping,
                         max.leaves = max.leaves,
                         nvmax = nvmax,
                         gamma = gamma,
                         alpha = alpha,
                         lambda = lambda,
                         lambda.seq = lambda.seq,
                         minobsinnode = minobsinnode,
                         minobsinnode.lin = minobsinnode.lin,
                         learning.rate = learning.rate,
                         part.minsplit = part.minsplit,
                         part.xval = part.xval,
                         part.max.depth = part.max.depth,
                         part.cp = part.cp,
                         part.minbucket = part.minbucket,
                         .rho = .rho,
                         rho.max = rho.max,
                         weights = .weights,
                         lin.type = lin.type,
                         cv.glmnet.nfolds = cv.glmnet.nfolds,
                         cv.glmnet.lambda = cv.glmnet.lambda,
                         verbose = verbose,
                         plot.tune.error = plot.tune.error,
                         trace = trace,
                         n.cores = n.cores)

  parameters <- list(max.leaves = max.leaves,
                     n.leaves = mod$n.leaves,
                     alpha = alpha,
                     lambda = lambda,
                     lambda.seq = lambda.seq,
                     minobsinnode = minobsinnode,
                     minobsinnode.lin = minobsinnode.lin,
                     learning.rate = learning.rate,
                     part.minsplit = part.minsplit,
                     part.xval = part.xval,
                     part.max.depth = part.max.depth,
                     part.cp = part.cp,
                     weights = .weights,
                     init = init,
                     lin.type = lin.type,
                     cv.glmnet.nfolds = cv.glmnet.nfolds,
                     cv.glmnet.lambda = cv.glmnet.lambda)

  # [ FITTED ] ====
  if (type == "Classification") {
    .fitted <- predict.shytreeLeavesRC(mod, x, type = "all")
    fitted <- .fitted$estimate
    fitted.prob <- .fitted$probability
  } else {
    fitted <- predict.shytreeLeavesRC(mod, x)
    fitted.prob <- NULL
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train)

  # [ PREDICTED ] ====
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Classification") {
      .predicted <- predict.shytreeLeavesRC(mod, x.test,
                                            type = "all",
                                            learning.rate = learning.rate,
                                            trace = trace,
                                            verbose = verbose.predict)
      predicted <- .predicted$estimate
      predicted.prob <- .predicted$probability
    } else {
      predicted <- predict.shytreeLeavesRC(mod, x.test,
                                           learning.rate = learning.rate,
                                           trace = trace,
                                           verbose = verbose.predict)
      predicted.prob <- NULL
    }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # [ OUTRO ] ====
  extra <- list(gridSearch = gs)
  rt <- rtModSet(mod = mod,
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
                 varimp = NULL,
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

} # rtemis:: s.SHYTREE
