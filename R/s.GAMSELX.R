# s.GAMSELX.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org
# TODO: Update to use cv.gamsel similar to s.GLMNET
# TODO: Add special case in rtMod predict

#' GAMSEL with Interaction Discovery [R]
#'
#' Trains a GAMSEL model using \code{gamsel2::gamsel} after finding
#' pairwise interactions by mass GLM.
#'
#' @inheritParams s.GAMSEL
#' @inheritParams gamsel2::gamsel
#' @param gamsel.params1 List of parameters to pass to \link{s.GAMSEL} for first model
#' (linear and nonlinear main effects)
#' @param pairs.on.resid Logical: If TRUE, train second gamsel on residuals of first.
#' Default = TRUE. Should be kept TRUE, option available for experimentation/demonstration, etc.
#' @param p.adjust.method Character: Method to use for multiple comparison correction after mass GLM.
#' Default = "holm"
#' @param alpha Float: significance level. Default = .05
#' @param gamsel.params2 List of parameters to pass to \link{s.GAMSEL} for final model
#' @return \link{rtMod}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.GAMSELX <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     x.name = NULL, y.name = NULL,
                     data = NULL,
                     data.test = NULL,
                     # gamselx
                     gamsel.params1 = list(),
                     pairs.on.resid = TRUE,
                     p.adjust.method = "holm",
                     alpha = .05,
                     gamsel.params2 = gamsel.params1,
                     # /gamselx
                     verbose = TRUE,
                     trace = 0,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.fit.theme", "lightgrid"),
                     na.action = na.exclude,
                     question = NULL,
                     n.cores = 1,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ Intro ] ====
  if (missing(x)) {
    print(args(s.GAMSELX))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "GAMSELX"

  # [ Dependencies ] ====
  if (!depCheck("gamsel2", verbose = FALSE)) {
    cat("\n")
    stop("Please install dependency using remotes::install_github('egenn/gamsel2') and try again")
  }

  # [ Arguments ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.GAMSELX))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)

  # [ Data ] ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    # ipw = ipw,
                    # ipw.type = ipw.type,
                    # upsample = upsample,
                    # downsample = downsample,
                    # resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Regression"), mod.name)
  # if (is.null(weights) && type == "Classification" && ipw) weights <- dt$weights
  if (verbose) dataSummary(x, y, x.test, y.test, type = type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  if (is.null(family)) {
    family <- if (type == "Regression") "gaussian" else "binomial"
  }
  n.features <- NCOL(x)

  y0 <- y
  # if (type == "Classification") {
  #   y <- 2 - as.numeric(y)
  # }



  # [ GAMSELX ] ====
  if (verbose) msg("Training GAMSELX...", newline.pre = TRUE)
  mod <- gamselx(x, y,
                 gamsel.params1 = gamsel.params1,
                 pairs.on.resid = pairs.on.resid,
                 p.adjust.method = p.adjust.method,
                 gamsel.params2 = gamsel.params2,
                 alpha = alpha,
                 n.cores = n.cores,
                 verbose = verbose,
                 trace = trace)

  # [ Fitted ] ====
  # if (type == "Regression") {
    fitted <- mod$fitted
    error.train <- modError(y, fitted)
    fitted.prob <- NULL
  # } else {
  #   fitted.prob <- c(predict(mod, x, index = lambda.index, type = "response"))
  #   fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
  #   levels(fitted) <- levels(y0)
  #   error.train <- modError(y0, fitted, fitted.prob)
  # }

  if (verbose) errorSummary(error.train, mod.name)

  # [ Predicted ] ====
  predicted.prob <- predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    # if (type == "Regression") {
      predicted <- c(predict(mod, x.test, index = lambda.index, type = "response"))
    # } else {
    #   predicted.prob <- c(predict(mod, x.test, index = lambda.index, type = "response"))
    #   predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
    #   levels(predicted) <- levels(y0)
    # }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ Outro ] ====
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 y.train = y0,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted.prob = fitted.prob,
                 fitted = fitted,
                 error.train = error.train,
                 predicted.prob = predicted.prob,
                 predicted = predicted,
                 se.prediction = se.prediction,
                 error.test = error.test,
                 parameters = list(gamsel.params1 = gamsel.params1,
                                   pairs.on.resid = pairs.on.resid,
                                   p.adjust.method = p.adjust.method,
                                   gamsel.params2 = gamsel.params2,
                                   alpha = alpha,
                                   n.cores = n.cores),
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

} # rtemis::s.GAMSELX
