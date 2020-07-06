# s.SHYTREE.R
# ::rtemis::
# 2019-20 Efstathios D Gennatas egenn.github.io
# Allow early stopping
# varimp: N cases-weighted mean of absolute coefficients

#' Stepwise Hybrid Tree [C, R]
#'
#' Train a Stepwise Hybrid Tree for Regression or Binary Classification
#'
#' The Stepwise Hybrid Tree grows a tree using a sequence of regularized linear models and stumps.
#' We specify an upper threshold of leaves using \code{max.leaves} instead of directly defining a number,
#' because depending on the other parameters and the datasets, splitting may stop early.
#'
#' @inheritParams s.GLM
#' @param max.leaves Integer: Maximum number of terminal nodes to grow
#' @param nvmax [gS] Integer: Number of max features to use for lin.type "allSubsets", "forwardStepwise", or
#' "backwardStepwise". If values greater than n of features in \code{x} are provided, they will be excluded
#' @param lookback Logical: If TRUE, check validation error to decide best number of leaves to use.
#' Default = TRUE
#' @param init Initial value. Default = \code{mean(y)}
#' @param lambda Float: lambda parameter for \code{MASS::lm.ridge} Default = .01
#' @param part.max.depth Integer: Max depth for each tree model within the additive tree
#' @param .gs internal use only
#' @param plot.tuning Logical: If TRUE, plot validation error during gridsearch
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
                      max.leaves = 8,
                      leaf.model = c("line", "spline"),
                      gamlearner = "gamsel",
                      gam.params = list(),  # use force.lambda to force gamsel over cv.gamsel
                      nvmax = 3,
                      force.max.leaves = NULL,
                      lookback = TRUE, # requires cross-validation with gridSearchLearn
                      gamma = .1,
                      gamma.on.lin = FALSE,
                      alpha = 1,
                      lambda = .05,
                      lambda.seq = NULL,
                      minobsinnode.lin = 10,
                      learning.rate =.5,
                      # rpart
                      part.minsplit = 5,
                      part.xval = 0,
                      part.max.depth = 1,
                      part.cp = 0,
                      part.minbucket = 3,
                      .rho = TRUE,
                      rho.max = 1000,
                      init = NULL,
                      lin.type = c("forwardStepwise", "glmnet", "cv.glmnet", "lm.ridge",
                                   "allSubsets", "backwardStepwise", "glm", "solve", "none"),
                      cv.glmnet.nfolds = 5,
                      which.cv.glmnet.lambda = "lambda.min",
                      metric = "auto",
                      maximize = NULL,
                      grid.resample.rtset = rtset.resample("kfold", 5),
                      grid.search.type = "exhaustive",
                      save.gridrun = FALSE,
                      grid.verbose = TRUE,
                      cluster = FALSE,
                      keep.x = FALSE,
                      simplify = TRUE,
                      cxrcoef = FALSE,
                      n.cores = rtCores,
                      .preprocess = NULL,
                      verbose = TRUE,
                      plot.tuning = TRUE,
                      verbose.predict = FALSE,
                      trace = 1,
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
  leaf.model <- match.arg(leaf.model)

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
  if (verbose) parameterSummary(max.leaves,
                                learning.rate,
                                gamma,
                                lin.type,
                                nvmax,
                                alpha,
                                lambda,
                                minobsinnode.lin,
                                part.minsplit,
                                part.minbucket,
                                part.cp,
                                newline.pre = TRUE)
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

  if (!is.null(force.max.leaves)) lookback <- FALSE

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

  if (is.null(force.max.leaves)) {
    if (lookback) {
      gc <- gridCheck(learning.rate, gamma, minobsinnode.lin, lambda, nvmax,
                      part.minsplit, part.minbucket, part.cp)
    } else {
      # not recommended to cv max.leaves - use lookback instead; option likely will be removed
      gc <- gridCheck(learning.rate, gamma, minobsinnode.lin, lambda, nvmax,
                      part.minsplit, part.minbucket, part.cp, max.leaves)
    }
  } else {
    gc <- FALSE
  }

  # Must turn off plotting during parallel grid search or else likely to hang
  if (!.gs & n.cores > 1) plot.tuning <- FALSE

  if ((!.gs && gc) | (!.gs && lookback && max.leaves > 1)) {
    grid.params <- if (lookback) list() else list(max.leaves = max.leaves)
    grid.params <- c(grid.params, list(learning.rate = learning.rate,
                                       gamma = gamma,
                                       minobsinnode.lin = minobsinnode.lin,
                                       lambda = lambda,
                                       nvmax = nvmax,
                                       part.minsplit = part.minsplit,
                                       part.minbucket = part.minbucket,
                                       part.cp = part.cp))
    fixed.params <- if (lookback) list(max.leaves = max.leaves) else list()
    fixed.params <- c(fixed.params, list(init = init,
                                         lin.type = lin.type,
                                         gamma.on.lin = gamma.on.lin,
                                         cv.glmnet.nfolds = cv.glmnet.nfolds,
                                         which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                                         metric = metric,
                                         ipw = ipw,
                                         ipw.type = ipw.type,
                                         upsample = upsample,
                                         resample.seed = resample.seed,
                                         plot.tuning = plot.tuning,
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

    gamma <- gs$best.tune$gamma
    lambda <- gs$best.tune$lambda
    minobsinnode.lin <- gs$best.tune$minobsinnode.lin
    learning.rate <- gs$best.tune$learning.rate
    part.minsplit <- gs$best.tune$part.minsplit
    part.minbucket <- gs$best.tune$part.minbucket
    part.cp <- gs$best.tune$part.cp
    nvmax <- gs$best.tune$nvmax
    # Return special from gridSearchLearn
    max.leaves <- gs$best.tune$n.leaves

    # Now ready to train final full model
    .gs <- FALSE
    lookback <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.max.leaves)) max.leaves <- force.max.leaves

  # [ shytreegamleaves ] ====
  if (.gs) {
    if (lookback) {
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

  mod <- shytreegamleaves(x, y,
                          x.valid = x.valid, y.valid = y.valid,
                          lookback = lookback,
                          max.leaves = max.leaves,
                          gamleaves = leaf.model == "spline",
                          gamlearner = gamlearner,
                          gam.params = gam.params,
                          nvmax = nvmax,
                          gamma = gamma,
                          gamma.on.lin = gamma.on.lin,
                          alpha = alpha,
                          lambda = lambda,
                          lambda.seq = lambda.seq,
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
                          which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                          verbose = verbose,
                          plot.tuning = plot.tuning,
                          trace = trace)

  parameters <- list(max.leaves = max.leaves,
                     learning.rate = learning.rate,
                     gamma = gamma,
                     n.leaves = mod$n.leaves,
                     lin.type = lin.type,
                     nvmax = nvmax,
                     alpha = alpha,
                     lambda = lambda,
                     lambda.seq = lambda.seq,
                     minobsinnode.lin = minobsinnode.lin,
                     part.minsplit = part.minsplit,
                     part.xval = part.xval,
                     part.max.depth = part.max.depth,
                     part.cp = part.cp,
                     weights = .weights,
                     init = init,
                     cv.glmnet.nfolds = cv.glmnet.nfolds,
                     which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                     metric = metric,
                     ipw = ipw,
                     ipw.type = ipw.type,
                     upsample = upsample,
                     resample.seed = resample.seed,
                     plot.tuning = plot.tuning)

  # [ FITTED ] ====
  if (type == "Classification") {
    .fitted <- predict(mod, x, type = "all")
    fitted <- .fitted$estimate
    fitted.prob <- .fitted$probability
  } else {
    fitted <- predict(mod, x)
    fitted.prob <- NULL
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train)

  # [ PREDICTED ] ====
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Classification") {
      .predicted <- predict(mod, x.test,
                            type = "all",
                            learning.rate = learning.rate,
                            trace = trace,
                            verbose = verbose.predict)
      predicted <- .predicted$estimate
      predicted.prob <- .predicted$probability
    } else {
      predicted <- predict(mod, x.test,
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
  varimp <- if (lin.type == "none") {
    numeric()
  } else {
    # This is probably a poor measure of variable importance.
    # In general, look at each leaf's coefficients instead
    apply(mod$leaves$coefs[, -1, drop = FALSE], 2, function(i) mean(abs(i))) * apply(x, 2, sd)
  }
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
                 varimp = varimp,
                 question = question,
                 extra = extra)

  if (cluster) {
    if (verbose) msg("Getting clusters...")
    mod$extra$clusters.train <- indexCasesByRules(x, mod$leaves$rules$rule)
    if (!is.null(x.test)) {
      mod$extra$clusters.test <- indexCasesByRules(x.test, mod$leaves$rules$rule)
    }
  }

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
