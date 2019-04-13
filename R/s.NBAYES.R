# s.NBAYES.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Naive Bayes Classifier [C]
#'
#' Train a Naive Bayes Classifier using \code{e1071::naiveBayes}
#'
#' @inheritParams s.GLM
#' @param laplace Float (>0): Laplace smoothing. Default = 0 (no smoothing)
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @family Supervised Learning
#' @export

s.NBAYES <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     laplace = 0,
                     x.name = NULL,
                     y.name = NULL,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.fit.theme", "lightgrid"),
                     question = NULL,
                     rtclass = NULL,
                     verbose = TRUE,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.NBAYES))
    invisible(9)
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "NBAYES"

  # [ DEPENDENCIES ] ====
  if (!depCheck("e1071", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = F), "/")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Classification", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)

  # [ NBAYES ] ====
  if (verbose) msg("Training Naive Bayes Classifier...", newline = TRUE)
  mod <- e1071::naiveBayes(x, y,
                           laplace = laplace, ...)

  # [ FITTED ] ====
  fitted <- predict(mod, x)
  error.train <- modError(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  rt <- rtMod$new(mod.name = mod.name,
                  y.train = y,
                  y.test = y.test,
                  x.name = x.name,
                  xnames = xnames,
                  mod = mod,
                  type = type,
                  fitted = fitted,
                  se.fit = NULL,
                  error.train = error.train,
                  predicted = predicted,
                  se.prediction = NULL,
                  error.test = error.test,
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

} # rtemis::s.NBAYES
