# s.DA.R
# ::rtemis::
# 2017-8 Efstathios D. Gennatas egenn.github.io

#' Linear and Quadratic Discriminant Analysis [C]
#'
#' Train a DA Classifier using \code{MASS::lda} or \code{MASS::qda}
#'
#' Note: Does not support case weights
#' @inheritParams s.GLM
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.DA <- function(x, y = NULL,
                 x.test = NULL, y.test = NULL,
                 da.type = "lin",
                 prior = NULL,
                 method = "moment",
                 nu = NULL,
                 upsample = FALSE,
                 downsample = FALSE,
                 resample.seed = NULL,
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

  # [ DEPENDENCIES ] ====
  if (!depCheck("MASS", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (da.type == "lin") {
    da <- MASS::lda
    mod.name <- "LDA"
  } else if (da.type == "quad") {
    da <- MASS::qda
    mod.name <- "QDA"
  } else {
    stop('Wrong da.type specified, must be "lin" or "quad"')
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    upsample = upsample,
                    downsample =  downsample,
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

  # [ L/QDA ] ====
  params <- c(list(x = x,
                   grouping = y,
                   method = method,
                   nu = nu), list(...))
  if (!is.null(prior)) params$prior <- prior

  if (verbose) msg("Running", ifelse(da.type == "lin", "Linear", "Quadratic"), "Discriminant Analysis...", newline.pre = TRUE)
  mod <- do.call(da, args = params)

  # [ FITTED ] ====
  fitted.raw <- predict(mod, x)
  fitted <- fitted.raw$class
  fitted.prob <- fitted.raw$posterior
  train.projections <- fitted.raw$x
  error.train <- modError(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.raw <- predicted <- predicted.prob <- error.test <- test.projections <- NULL
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
  extra <- list(train.projections = train.projections,
                test.projections = test.projections)
  rt <- rtModSet(mod.name = mod.name,
                 type = type,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 xnames = xnames,
                 mod = mod,
                 fitted = fitted,
                 fitted.prob = fitted.prob,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted.prob = predicted.prob,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 parameters = list(da.type = da.type,
                                   prior = prior,
                                   method = method,
                                   nu = nu,
                                   upsample = upsample,
                                   downsample = downsample,
                                   resample.seed = resample.seed),
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

} # rtemis::s.QDA


#' Linear Discriminant Analysis
#'
#' alias for \code{s.DA(da.type = "lin")}
#'
#' @inheritParams s.DA
#' @export
s.LDA <- function(x, y,
                  x.test = NULL, y.test = NULL,
                  prior = NULL,
                  method = "moment",
                  nu = NULL,
                  x.name = NULL,
                  y.name = NULL,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = "box",
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  s.DA(x = x, y = y,
       x.test = x.tes, y.test = y.test,
       da.type = "line",
       prior = prior,
       method = method,
       nu = nu,
       x.name = x.name,
       y.name = y.name,
       print.plot = print.plot,
       plot.fitted = plot.fitted,
       plot.predicted = plot.predicted,
       plot.theme = plot.theme,
       question = question,
       rtclass = rtclass,
       verbose = verbose,
       outdir = outdir,
       save.mod = save.mod)

} # rtemis::s.LDA


#' Quadratic Discriminant Analysis
#'
#' alias for \code{s.DA(da.type = "quad")}
#'
#' @inheritParams s.DA
#' @export
s.QDA <- function(x, y,
                  x.test = NULL, y.test = NULL,
                  prior = NULL,
                  method = "moment",
                  nu = NULL,
                  x.name = NULL,
                  y.name = NULL,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = "box",
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  s.DA(x = x, y = y,
       x.test = x.tes, y.test = y.test,
       da.type = "quad",
       prior = prior,
       method = method,
       nu = nu,
       x.name = x.name,
       y.name = y.name,
       print.plot = print.plot,
       plot.fitted = plot.fitted,
       plot.predicted = plot.predicted,
       plot.theme = plot.theme,
       question = question,
       rtclass = rtclass,
       verbose = verbose,
       outdir = outdir,
       save.mod = save.mod,
       plot.theme)

} # rtemis::s.QDA
