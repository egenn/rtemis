# s.IRF.R
# ::rtemis::
# 2018 E.D. Gennatas lambdamd.org

#' Iterative Random Forest [C, R]
#'
#' Train iterative Random Forests for regression or classification using \code{iRF}
#'
#' If \code{autotue = TRUE}, \code{iRF::tuneRF} will be run to determine best \code{mtry}
#'   value.
#'
#' @inheritParams s.RF
#' @param ... Additional arguments to be passed to \code{iRF::iRF}
#' @return \link{rtMod} object
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s.IRF <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  n.trees = 1000,
                  n.iter = 5,
                  n.bootstrap = 30,
                  interactions.return = NULL,
                  classwt = NULL,
                  ipw = TRUE,
                  upsample = FALSE,
                  downsample = FALSE,
                  resample.seed = NULL,
                  autotune = FALSE,
                  n.trees.try = 500,
                  stepFactor = 2,
                  mtry = NULL,
                  mtryStart = NULL,
                  mtry.select.prob = NULL,
                  proximity = FALSE,
                  importance = TRUE,
                  replace = TRUE,
                  min.node.size = 1,
                  strata = NULL,
                  sampsize = NULL,
                  tune.do.trace = FALSE,
                  print.tune.plot = FALSE,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  n.cores = rtCores,
                  question = NULL,
                  verbose = TRUE,
                  trace = 0,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ====
  if (missing(x)) {
    print(args(s.IRF))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "IRF"

  # Dependencies ====
  if (!depCheck("iRF", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.IRF))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)

  # Data ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .classwt <- if (is.null(classwt) & ipw) dt$class.weights else classwt
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry, pad = 4,
                                newline.pre = TRUE)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(mtry)) {
    mtry <- if (type == "Classification") floor(sqrt(NCOL(x))) else max(floor(NCOL(x)/3), 1)
  }
  if (is.null(mtryStart)) mtryStart <- mtry
  if (is.null(mtry.select.prob)) {
    mtry.select.prob <- rep(1/NCOL(x), NCOL(x))
  }
  if (is.null(sampsize)) {
    sampsize <- if (replace) nrow(x) else ceiling(.632*nrow(x))
  }

  # iRF::tuneRF/iRF ====
  if (autotune) {
    if (verbose) msg("Tuning for mtry...")
    tuner <- iRF::tuneRF(x = x, y = y,
                         mtryStart = mtryStart,
                         ntreeTry = n.trees.try,
                         stepFactor = stepFactor,
                         trace = verbose,
                         plot = print.tune.plot,
                         strata = strata,
                         do.trace = tune.do.trace,
                         classwt = classwt)
    mtry <- tuner[which.min(tuner[, 2]), 1]
    if (verbose) msg("Best mtry :", mtry)
  }
  if (verbose) msg("Running Iterative Random Forest (iRF)", type, "with", n.trees, "trees...", newline.pre = TRUE)
  mod <- iRF::iRF(x = data.matrix(x), y = y,
                  n.iter = n.iter,
                  ntree = n.trees,
                  n.core = n.cores,
                  n.bootstrap = n.bootstrap,
                  interactions.return = interactions.return,
                  classwt = classwt,
                  mtry = mtry,
                  mtry.select.prob = mtry.select.prob,
                  min.node.size = min.node.size,
                  replace = replace,
                  proximity = proximity,
                  importance = importance,
                  verbose = trace > 0, ...)
  parameters <- list(n.iter = n.iter,
                     n.bootstrap = n.bootstrap,
                     n.trees = n.trees,
                     mtry = mtry)

  # Fitted ====
  fitted.raw <- lapply(mod$rf.list, function(i) predict(i, x))
  if (type == "Classification") {
    fitted <- factor(apply(do.call(cbind, fitted.raw), 1, function(i) as.integer(mean(i))))
    levels(fitted) <- levels(y)
  } else {
    fitted <- apply(do.call(cbind, fitted.raw), 1, mean)
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  if (!is.null(x.test)) {
    predicted.raw <- lapply(mod$rf.list, function(i) predict(i, x.test))
    if (type == "Classification") {
      predicted <- factor(apply(do.call(cbind, predicted.raw), 1, function(i) as.integer(mean(i))))
      levels(predicted) <- levels(y)
    } else {
      predicted <- apply(do.call(cbind, predicted.raw), 1, mean)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted.raw <- predicted <- error.test <- NULL
  }

  # Variable importance ====
  if (importance) {
    metric <- if (type == "Classification") "MeanDecreaseAccuracy" else "%IncMSE"
    varimp.raw <- lapply(mod$rf.list, function(i) i$importance[, metric])
    varimp <- apply(do.call(cbind, varimp.raw), 1, mean)
  } else {
    varimp <- NULL
  }

  # Outro ====
  extra <- list(fitted.raw = fitted.raw,
                predicted.raw = predicted.raw)
  if (!is.null(importance)) extra$var.imp <- mod$variable.importance
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
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 varimp = varimp,
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
  return(rt)

} # rtemis::s.IRF
