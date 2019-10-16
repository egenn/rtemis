# s.POLYMARS.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io
# method = "cv" fails to find nk and penalty

#' Multivariate adaptive polynomial spline regression (POLYMARS) [C, R]
#'
#' Trains a POLYMARS model using \code{polspline::polymars} and validates it
#'
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as \code{x}
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param ... Additional parameters to pass to \code{polspline::polymars}
#' @return Object of class \link{rtMod}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.POLYMARS <- function(x, y = NULL,
                       x.test = NULL, y.test = NULL,
                       x.name = NULL, y.name = NULL,
                       grid.resample.rtset = rtset.grid.resample(),
                       bag.resample.rtset = NULL,
                       weights = NULL,
                       ipw = TRUE,
                       ipw.type = 2,
                       upsample = FALSE,
                       downsample = FALSE,
                       resample.seed = NULL,
                       maxsize = ceiling(min(6 * (nrow(x)^{1/3}), nrow(x)/4, 100)),
                       classify = NULL,
                       n.cores = rtCores,
                       print.plot = TRUE,
                       plot.fitted = NULL,
                       plot.predicted = NULL,
                       plot.theme = getOption("rt.fit.theme", "lightgrid"),
                       question = NULL,
                       verbose = TRUE,
                       trace = 0,
                       save.mod = FALSE,
                       outdir = NULL, ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.POLYMARS))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "POLYMARS"

  # [ DEPENDENCIES ] ====
  if (!depCheck("polspline", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(s.POLYMARS)); stop("x is missing")
  }
  if (is.null(y) & NCOL(x) < 2) { print(args(s.POLYMARS)); stop("y is missing") }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  bag <- if (is.null(bag.resample.rtset)) FALSE else bag.resample.rtset$n.resamples > 0

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
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
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (is.null(.weights)) .weights <- rep(1, nrow(x))
  x0 <- if (upsample|downsample) dt$x0 else x
  y0 <- if (upsample|downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (is.null(classify)) classify <- ifelse(type == "Classification", TRUE, FALSE)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # [ GRID SEARCH ] ====
  if (gridCheck(maxsize)) {
    gs <- gridSearchLearn(x0, y0, mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(maxsize = maxsize),
                          fixed.params = list(classify = classify,
                                              ipw = ipw,
                                              ipw.type = ipw.type),
                          weights = weights,
                          minimize = "MSE", verbose = verbose, n.cores = n.cores)
    maxsize <- gs$best.tune$maxsize
  } else {
    gs <- NULL
  }

  # [ POLYMARS ] ====
    if (verbose) msg("Training POLYMARS model...", newline.pre = TRUE)
      mod <- polspline::polymars(y, x,
                                 weights = .weights,
                                 maxsize = maxsize,
                                 verbose = verbose,
                                 classify = classify, ...)
    if (trace > 0) print(summary(mod))

  # [ FITTED ] ====
    fitted <- predict(mod, x)
    if (type == "Classification") {
      fitted <- apply(fitted, 1, which.max)
      fitted <- factor(levels(y)[fitted])
      levels(fitted) <- levels(y)
    }

  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
      predicted <- predict(mod, x.test)
    if (type == "Classification") {
      predicted <- apply(predicted, 1, which.max)
      predicted <- factor(levels(y)[predicted])
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(gridSearch = gs,
                grid.resample.rtset = grid.resample.rtset)
  rt <- rtModSet(rtclass = "rtMod",
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

} # rtemis::s.POLYMARS
