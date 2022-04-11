# s_LOESS.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Local Polynomial Regression (LOESS) [R]
#'
#' Fits a LOESS curve or surface
#'
#' A maximum of 4 features are allowed in this implementation (\code{stats::loess})
#' The main use for this algorithm would be fitting curves in bivariate plots,
#' where GAM or similar is preferable anyway. It is included in \pkg{rtemis} mainly for academic purposes -
#' not for building predictive models.
#'
#' @inheritParams s_GLM
#' @param ... Additional arguments to \code{loess}
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso \link{elevate}
#' @export

s_LOESS <- function(x, y = NULL,
                    x.test = NULL, y.test = NULL,
                    x.name = NULL, y.name = NULL,
                    print.plot = TRUE,
                    plot.fitted = NULL,
                    plot.predicted = NULL,
                    plot.theme = getOption("rt.theme"),
                    question = NULL,
                    rtclass = NULL,
                    verbose = TRUE,
                    trace = 0,
                    outdir = NULL,
                    save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ----
  if (missing(x)) {
    print(args(s_LOESS))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "LOESS"

  # Arguments ----
  if (missing(x)) { print(args(s_LOESS)); stop("x is missing") }
  if (is.null(y) & NCOL(x) < 2) { print(args(s_LOESS)); stop("y is missing") }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
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
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # loess ----
  df.train <- data.frame(y = y, x)
  features <- paste0(xnames, collapse = " + ")
  formula <- as.formula(paste0("y", " ~ ", features))
  if (verbose) msg("Training LOESS model...", newline.pre = TRUE)
  mod <- loess(formula, data = df.train, ...)
  if (trace > 0) print(summary(mod))

  # Fitted ----
  fitted <- predict(mod, se = TRUE)
  se.fit <- as.numeric(fitted$se.fit)
  fitted <- as.numeric(fitted$fit)
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  if (!is.null(x.test) & !is.null(y.test)) {
    predicted <- predict(mod, newdata = x.test, se = TRUE)
    se.prediction <- predicted$se.fit
    predicted <- as.numeric(predicted$fit)
    error.test <- modError(y.test, predicted)
    if (verbose) errorSummary(error.test, mod.name)
  } else {
    predicted <- se.prediction <- error.test <- NULL
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
              se.fit = se.fit,
              error.train = error.train,
              predicted = predicted,
              se.prediction = se.prediction,
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

} # rtemis::s_LOESS
