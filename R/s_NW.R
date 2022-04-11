# s_NW.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Nadaraya-Watson kernel regression [R]
#'
#' Computes a kernel regression estimate using \code{np::npreg()}
#'
#' \code{np::npreg} allows inputs with mixed data types.
#' NW automatically models interactions, like PPR, but the latter is a lot faster
#'
#' @inheritParams s_GLM
#' @param bw Bandwidth as calculate by \code{np::npregbw}. Default = NULL, in which case \code{np::npregbw} will be run
#' @param plot.bw Logical. Plot bandwidth selector results
#' @param ... Additional parameters to be passed to \code{npreg}
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' y <- .6 * x + 12 + rnorm(100)
#' mod <- s_NW(x, y)}
#' @export

s_NW <- function(x, y = NULL,
                 x.test = NULL, y.test = NULL,
                 x.name = NULL, y.name = NULL,
                 bw = NULL,
                 plot.bw = FALSE,
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
    print(args(s_NW))
    invisible(9)
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "NW"

  # Dependencies ----
  dependency_check("np")

  # Arguments ----
  if (missing(x)) {
    print(args(s_NW)); stop("x is missing")
  }
  if (is.null(y) & NCOL(x) < 2) { print(args(s_NW)); stop("y is missing") }
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
  if (type != "Regression") stop("s_NW only supports Regression")
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # NW ----
  # ! npregbw fails if 'np' is not loaded # used to fail?
  # suppressMessages(require(np, quietly = TRUE, warn.conflicts = FALSE))
  # requireNamespace("np", quietly = TRUE)
  if (is.null(bw)) {
    if (verbose) msg("Computing bandwidth...")
    bw <- np::npregbw(xdat = x, ydat = y)
    if (plot.bw) plot(bw, plot.errors.method = "asymptotic")
  }

  if (verbose) msg("Training Kernel Regression model...", newline.pre = TRUE)
  mod <- np::npreg(bw, txdat = x, tydat = y)
  if (trace > 0) summary(mod)

  # Fitted ----
  fitted <- predict(mod, x, se.fit = TRUE)
  se.fit <- as.numeric(fitted$se.fit)
  fitted <- as.numeric(fitted$fit)

  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test, se.fit = TRUE)
    se.prediction <- predicted$se.fit
    predicted <- predicted <- as.numeric(predicted$fit)
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(bw = bw)
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
                 se.fit = se.fit,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = se.prediction,
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

} # rtemis::s_NW
