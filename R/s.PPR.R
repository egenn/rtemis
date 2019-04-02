# s.PPR.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Projection Pursuit Regression (PPR) [R]
#'
#' Train a Projection Pursuirt Regression model
#'
#' [gS]: If more than one value is passed, parameter tuning via grid search will be performed on resamples of the
#'   training set prior to training model on full training set
#'   Interactions: PPR automatically models interactions, no need to specify them
#'
#' @inheritParams s.GLM
#' @param nterms [gS] Integer: number of terms to include in the final model
#' @param optlevel [gS] Integer [0, 3]: optimization level (Default = 3). See Details in \code{stats::ppr}
#' @param sm.method [gS] String: "supsmu", "spline", "gcvspline". Smoothing method. Default = "gcvspline"
#' @param bass [gS] Numeric [0, 10]: for \code{sm.method} "supsmu": larger values result in greater smoother
#'   (Default = 5). See \code{stats::ppr}
#' @param span [gS] Numeric [0, 1]: for \code{sm.method} "supsmu": 0 (Default) results in automatic span selection by
#'   local crossvalidation. See \code{stats::ppr}
#' @param df [gS] Numeric: for \code{sm.method} "spline": Specify smoothness of each ridge term. See \code{stats::ppr}
#' @param gcvpen [gs] Numeric: for \code{sm.method} "gcvspline": Penalty used in the GCV selection for each
#'   degree of freedom used. Higher values -> greater smoothing. See \code{stats::ppr} Default = 5
#' @param ... Additional arguments to be passed to \code{ppr}
#' @return Object of class \pkg{rtemis}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.PPR <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  grid.resample.rtset = rtset.grid.resample(),
                  grid.search.type = c("exhaustive", "randomized"),
                  grid.search.randomized.p = .1,
                  weights = NULL,
                  nterms = NULL,
                  max.terms = nterms,
                  optlevel = 3,
                  sm.method = c("supsmu", "spline", "gcvspline"),
                  bass = 0,
                  span = 0,
                  df = 5,
                  gcvpen = 1,
                  metric = "MSE",
                  maximize = FALSE,
                  n.cores = rtCores,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  trace = 0,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.PPR))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "PPR"

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(s.PPR))
    stop("x is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  sm.method <- match.arg(sm.method)
  if (is.null(nterms)) nterms <- if (NCOL(x) < 4 ) NCOL(x) else 4
  grid.search.type <- match.arg(grid.search.type)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
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

  # [ GRID SEARCH ] ====
  if (gridCheck(nterms, optlevel, sm.method, bass, span, df, gcvpen)) {
    gs <- gridSearchLearn(x, y,
                          mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(nterms = nterms,
                                             optlevel = optlevel,
                                             sm.method = sm.method,
                                             bass = bass,
                                             span = span,
                                             df = df,
                                             gcvpen = gcvpen),
                          search.type = grid.search.type,
                          randomized.p = grid.search.randomized.p,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          verbose = verbose,
                          n.cores = n.cores)
    nterms <- gs$best.tune$nterms
    optlevel <- gs$best.tune$optlevel
    sm.method <- as.character(gs$best.tune$sm.method) # gridSearchLearn returns best.tune as df which convert characters to factors: change to list
    bass <- gs$best.tune$bass
    span <- gs$best.tune$span
    df <- gs$best.tune$df
    gcvpen <- gs$best.tune$gcvpen
  } else {
    gs <- NULL
  }
  if (verbose) parameterSummary(nterms, optlevel, sm.method, bass, span, df, gcvpen)

  # [ PPR ] ====
  if (verbose) msg("Running Projection Pursuit Regression...", newline = TRUE)
  mod <- ppr(x, y,
             weights = weights,
             nterms = nterms,
             optlevel = optlevel,
             sm.method = sm.method,
             bass = bass,
             span = span,
             df = df,
             gcvpen = gcvpen, ...)
  if (trace > 0) print(summary(mod))

  # [ FITTED ] ====
  fitted <- as.numeric(predict(mod))
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- as.numeric(predict(mod, x.test))
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  parameters <- list(weights = weights,
                     nterms = nterms,
                     max.terms = max.terms,
                     optlevel = optlevel,
                     sm.method = sm.method,
                     bass = bass,
                     span = span,
                     df = df,
                     gcvpen = gcvpen)
  extra <- list(grid.resample.rtset = grid.resample.rtset,
                gridSearch = gs)
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

} # rtemis::s.PPR
