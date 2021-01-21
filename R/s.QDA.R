# s.QDA.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Quadratic Discriminant Analysis [C]
#'
#' Train a QDA Classifier using \code{MASS::qda}
#'
#' @inheritParams s.GLM
#' @return \link{rtMod} object
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.QDA <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  prior = NULL,
                  method = "moment",
                  nu = NULL,
                  x.name = NULL,
                  y.name = NULL,
                  upsample = FALSE,
                  downsample = FALSE,
                  resample.seed = NULL,
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
    print(args(s.QDA))
    invisible(9)
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "QDA"

  # [ DEPENDENCIES ] ====
  if (!depCheck("MASS", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # [ DATA ] ====
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
  checkType(type, "Classification", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # [ QDA ] ====
  params <- c(list(x = x, grouping = y,
                   method = method,
                   nu = nu), list(...))
  if (!is.null(prior)) params$prior <- prior
  if (verbose) msg("Running Quadratic Discriminant Analysis...", newline.pre = TRUE)
  mod <- do.call(MASS::qda, args = params)

  # [ FITTED ] ====
  fitted.raw <- predict(mod, x)
  fitted <- fitted.raw$class
  fitted.prob <- fitted.raw$posterior
  train.projections <- fitted.raw$x
  error.train <- modError(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.raw <- predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted.raw <- predict(mod, x.test)
    predicted <- predicted.raw$class
    predicted.prob <- predicted.raw$posterior
    test.projections <- predicted.raw$x
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(fitted.prob = fitted.prob, predicted.prob = predicted.prob,
                train.projections = train.projections, test.projections = test.projections)
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

} # rtemis::s.QDA
