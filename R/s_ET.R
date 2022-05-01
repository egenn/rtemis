# s_ET.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' ExtraTrees [C, R]
#'
#' Train an ExtraTrees model and validate
#'
#' @inheritParams s_GLM
#' @inheritParams s_RF
#' @param n.cores Integer. N of cores to use
#' @param verbose Logical. Print summary to screen
#' @param ... Additional parameters to be passed to \code{extraTrees}
#' 
#' @return Object of class \pkg{rtemis}
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' y <- .6 * x + 12 + rnorm(100)
#' mod <- s_ET(x, y)}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_ET <- function(x, y = NULL,
                 x.test = NULL, y.test = NULL,
                 x.name = NULL, y.name = NULL,
                 n.trees = 500,
                 mtry = if (!is.null(y) && !is.factor(y))
                   max(floor(NCOL(x)/3), 1) else floor(sqrt(NCOL(x))),
                 nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                 weights = NULL,
                 ipw = TRUE,
                 upsample = FALSE,
                 downsample = FALSE,
                 resample.seed = resample.seed,
                 n.cores = future::availableCores(),
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
    print(args(s_ET))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "ET"

  # Dependencies ----
  dependency_check("extraTrees")

  # Arguments ----
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s_ET))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
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
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # ET ----
  if (verbose) msg("Training extraTrees model...", newline.pre = TRUE)
  mod <- extraTrees::extraTrees(x = x, y = y,
                                ntree = n.trees,
                                mtry = mtry,
                                nodesize = nodesize,
                                weights = .weights,
                                numThreads = n.cores, ...)
  if (trace > 0) summary(mod)

  # Fitted ----
  fitted <- as.numeric(predict(mod, x))
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

} # rtemis::s_ET
