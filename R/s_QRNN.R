# s_QRNN.R
# ::rtemis::
# 2015 E.D. Gennatas www.lambdamd.org
#
# Description
#   Train a quantile regression neural network and validate it.
#
# Notes
#   Can replace qrnn.nlm to add resample support instead of regular bootstrap

#' Quantile Regression Neural Network [R]
#'
#' Train an ensemble of Neural Networks to perform Quantile Regression using `qrnn`
#'
#' @inheritParams s_GLM
#' @param n.hidden Integer. Number of hidden nodes
#' @param tau Float. tau-quantile. Defaults to .5
#' @param n.ensemble Integer. Number of NNs to train
#' @param iter.max Integer. Max N of iteration of the optimization algorithm
#' @param n.trials Integer. N of trials. Used to avoid local minima
#' @param bag Logical. Should bagging be used?
#' @param ... Additional arguments to be passed to `qrnn::qrnn.fit`
#' @author E.D. Gennatas
#' @seealso [train] for external cross-validation
#' @family Supervised Learning
#' @export

s_QRNN <- function(x, y = NULL,
                   x.test = NULL, y.test = NULL,
                   x.name = NULL, y.name = NULL,
                   n.hidden = 1,
                   tau = .5,
                   n.ensemble = 5,
                   iter.max = 5000,
                   n.trials = 5,
                   bag = TRUE,
                   lower = -Inf,
                   eps.seq = 2 ^ (-8 : -32),
                   Th = qrnn::sigmoid,
                   Th.prime = qrnn::sigmoid.prime,
                   penalty = 0,
                   trace = T,
                   print.plot = FALSE,
                   plot.fitted = NULL,
                   plot.predicted = NULL,
                   plot.theme = rtTheme,
                   question = NULL,
                   rtclass = NULL,
                   verbose = TRUE,
                   outdir = NULL,
                   save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ----
  if (missing(x)) {
    print(args(s_QRNN))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "QRNN"

  # Dependencies ----
  dependency_check("qrnn")

  # Arguments ----
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s_QRNN))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- dataPrepare(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)

  # QRNN ----
  if (verbose) msg2("Training QRNN mmodel...", newline.pre = TRUE)
  mod <- qrnn::qrnn.fit(x = as.matrix(x), y = as.matrix(y),
                        n.hidden = n.hidden,
                        tau = tau,
                        n.ensemble = n.ensemble,
                        iter.max = iter.max,
                        n.trials = n.trials,
                        bag = bag,
                        lower = lower,
                        eps.seq = eps.seq,
                        Th = Th,
                        Th.prime = Th.prime,
                        penalty = penalty,
                        trace = verbose, ...)

  # Fitted ----
  fitted <- rowMeans(qrnn::qrnn.predict(as.matrix(x), mod))
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- rowMeans(qrnn::qrnn.predict(as.matrix(x.test), mod))
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtModSet(rtclass = rtclass,
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
              error.test = error.test, list,
              question = question)

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

} # rtemis::s_QRNN
