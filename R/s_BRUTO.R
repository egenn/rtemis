# s_BRUTO.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Projection Pursuit Regression (BRUTO) [R]
#'
#' Trains a BRUTO model and validates it
#'
#' @inheritParams s_GLM
#' @param ... Additional arguments to be passed to \code{mda::bruto}
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s_BRUTO <- function(x, y = NULL,
                    x.test = NULL, y.test = NULL,
                    x.name = NULL, y.name = NULL,
                    grid.resample.rtset = rtset.grid.resample(),
                    weights = NULL,
                    weights.col = NULL,
                    dfmax = 6,
                    cost = 2,
                    maxit.select = 20,
                    maxit.backfit = 20,
                    thresh = .0001,
                    start.linear = TRUE,
                    n.cores = rtCores,
                    print.plot = TRUE,
                    plot.fitted = NULL,
                    plot.predicted = NULL,
                    plot.theme = rtTheme,
                    question = NULL,
                    rtclass = NULL,
                    verbose = TRUE,
                    trace = 0,
                    outdir = NULL,
                    save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ----
  if (missing(x)) {
    print(args(s_BRUTO))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "BRUTO"

  # Dependencies ----
  dependency_check("mda")

  # Arguments ----
  if (missing(x)) { print(args(s_BRUTO)); stop("x is missing") }
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s_BRUTO))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    verbose = verbose)
  x <- data.matrix(dt$x)
  y <- data.matrix(dt$y)
  if (!is.null(dt$x.test)) x.test <- data.matrix(dt$x.test)
  if (!is.null(dt$y.test)) y.test <- data.matrix(dt$y.test)
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (is.null(weights)) weights <- rep(1, length(y))
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Grid Search ----
  if (gridCheck(dfmax, cost, maxit.select, maxit.backfit, thresh)) {
    gs <- gridSearchLearn(x, y, mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(dfmax = dfmax,
                                             cost = cost,
                                             maxit.select = maxit.select,
                                             maxit.backfit = maxit.backfit,
                                             thresh = thresh),
                          weights = weights,
                          metric = "MSE",
                          maximize = FALSE,
                          verbose = verbose,
                          n.cores = n.cores)
    dfmax <- gs$best.tune$dfmax
    cost <- gs$best.tune$cost
    maxit.select <- gs$best.tune$maxit.select
    maxit.backfit <- gs$best.tune$maxit.backfit
    thresh <- gs$best.tune$thresh
  }

  # BRUTO ----
  if (verbose) msg("Training BRUTO...", newline.pre = TRUE)
  mod <- mda::bruto(x, y,
                    w = weights,
                    wp = weights.col,
                    dfmax = dfmax,
                    cost = cost,
                    maxit.select = maxit.select,
                    maxit.backfit = maxit.backfit,
                    thresh = thresh,
                    start.linear = start.linear,
                    trace.bruto = trace > 0, ...)

  # Fitted ----
  fitted <- as.numeric(predict(mod))
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- as.numeric(predict(mod, x.test))
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(grid.resample.rtset = grid.resample.rtset)
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 call = call,
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

} # rtemis::s_BRUTO
