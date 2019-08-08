# s.HYTBOOST.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

#' Boosting of Additive Trees [R]
#'
#' Boost an hybrid additive tree using \link{addtboost}
#'
#' @inheritParams addtboost
#' @inheritParams s.GLM
#' @param mod.params Named list of arguments for \code{mod}
#' @param learning.rate Float (0, 1] Learning rate for the additive steps
#' @param init Float: Initial value for prediction. Default = mean(y)
#' @param tolerance Float: If training error <= this value, training stops
#' @param tolerance.valid Float: If validation error <= this value, training stops
#' @param max.iter Integer: Maximum number of iterations (additive steps) to perform. Default = 10
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param base.verbose Logical: \code{verbose} argument passed to learner
#' @param print.error.plot String or Integer: "final" plots a training and validation (if available) error curve at the
#' end of training. If integer, plot training and validation error curve every this many iterations
#' during training
#' @param print.base.plot Logical: Passed to \code{print.plot} argument of base learner, i.e. if TRUE, print error plot
#' for each base learner
#' @param ... Additional parameters to be passed to learner
#' @author Efstathios D. Gennatas
#' @export


s.HYTBOOST <- function(x, y = NULL,
                        x.test = NULL, y.test = NULL,
                        x.valid = NULL, y.valid = NULL,
                        resid = NULL,
                        boost.obj = NULL,
                        mod.params = rtset.HYTREE(),
                        case.p = 1,
                        weights = NULL,
                        max.iter = 10,
                        learning.rate = .1,
                        init = mean(y),
                        cxrcoef = FALSE,
                        print.progress.every = 5,
                        print.error.plot = "final",
                        x.name = NULL,
                        y.name = NULL,
                        question = NULL,
                        base.verbose = FALSE,
                        verbose = TRUE,
                        trace = 0,
                        prefix = NULL,
                        plot.fitted = NULL,
                        plot.predicted = NULL,
                        plot.theme = getOption("rt.fit.theme", "lightgrid"),
                        print.plot = TRUE,
                        print.base.plot = FALSE,
                        plot.type = 'l',
                        outdir = NULL,
                        save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(boost))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "HYTBOOST"

  # [ DEPENDENCIES ] ====
  if (!depCheck("rpart", "glmnet", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  # if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  extra.args <- list(...)
  mod.params <- c(mod.params, extra.args)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    x.valid = x.valid, y.valid = y.valid,
                    # ipw = ipw, ipw.type = ipw.type,
                    # upsample = upsample, resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  x.valid <- dt$x.valid
  y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)

  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  # if (is.null(init)) init <- mean(y)

  # [ HYTBOOST ] ====
  if (verbose) parameterSummary(init,
                                max.iter,
                                learning.rate,
                                mod.params)
  if (trace > 0) msg("Initial MSE =", mse(y, init))
  if (verbose) msg("Training HYTBOOST...", newline.pre = TRUE)
  mod <- addtboost(x = x, y = y,
                   x.valid = x.valid, y.valid = y.valid,
                   resid = resid,
                   boost.obj = boost.obj,
                   mod.params = mod.params,
                   case.p = case.p,
                   learning.rate = learning.rate,
                   max.iter = max.iter,
                   init = init,
                   cxrcoef = cxrcoef,
                   print.error.plot = print.error.plot,
                   print.progress.every = print.progress.every,
                   base.verbose = base.verbose,
                   verbose = verbose,
                   trace = trace,
                   prefix = prefix,
                   print.plot = print.plot,
                   plot.type = 'l')

  # [ FITTED ] ====
  fitted <- mod$fitted
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train)

  # [ VALID ] ====
  error.valid <- if (!is.null(y.valid)) mod$error.valid else NULL

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # [ OUTRO ] ====
  parameters <- list(init = init,
                     max.iter = max.iter,
                     learning.rate = learning.rate,
                     mod.params = mod.params)
  extra <- list(error.valid = error.valid)
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 parameters = parameters,
                 call = NULL,
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

} # rtemis::s.HYTBOOST
