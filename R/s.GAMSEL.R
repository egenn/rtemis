# s.GAM.default.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Regularized Generalized Additive Model (GAMSEL) [C, R]
#'
#' Trains a GAMSEL using \code{gamsel::gamsel} and validates it.
#' Input will be used to create a formula of the form:
#' \deqn{y = s(x_{1}, k) + s(x_{2}, k) + ... + s(x_{n}, k)}
#'
#' @inheritParams s.GLM
#' @param k Integer. Number of bases for smoothing spline
#' @param ... Additional arguments to be passed to \code{mgcv::gam}
#' @return \link{rtMod}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.GAMSEL <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     x.name = NULL, y.name = NULL,
                     data = NULL,
                     data.test = NULL,
                     ipw = TRUE,
                     ipw.type = 2,
                     upsample = FALSE,
                     upsample.seed = NULL,
                     num.lambda = 50,
                     lambda = NULL,
                     family = NULL,
                     degrees = 10,
                     gamma = 0.4,
                     dfs = 5,
                     tol = 1e-04,
                     max.iter = 2000,
                     parallel = FALSE,
                     cleanup = TRUE,
                     verbose = TRUE,
                     trace = 0,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.fit.theme", "lightgrid"),
                     na.action = na.exclude,
                     question = NULL,
                     n.cores = rtCores,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.GAMSEL))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "GAMSEL"

  # [ DEPENDENCIES ] ====
  if (!depCheck("gamsel", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(s.GAM))
    stop("x is missing")
  }
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.GAMSEL))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (is.null(weights) && type == "Classification" && ipw) weights <- dt$weights
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
  if (type == "Classification") {
    y <- as.numeric(y) - 1
  }

  if (length(degrees) != n.features) degrees <- degrees[seql(degrees, seq(n.features))]
  if (length(dfs) != n.features) dfs <- dfs[seql(dfs, seq(n.features))]

  # [ GAMSEL ] ====
  bases <- gamsel::pseudo.bases(x, degrees, dfs, parallel = parallel, ...)
  if (verbose) msg("Training GAMSEL...", newline = TRUE)
  args <- list(x = x,
               y = y,
               num_lambda = num.lambda,
               lambda = lambda,
               family = family,
               degrees = degrees,
               gamma = gamma,
               dfs = dfs,
               bases = bases,
               tol = tol,
               max_iter = max.iter,
               traceit = trace > 0,
               parallel = parallel)
  mod <- do.call(gamsel::gamsel, args)
  if (cleanup) mod$call <- NULL

  # [ FITTED ] ====
  if (type == "Regression") {
    fitted <- c(predict(mod, x, index = num.lambda, type = "response"))
    error.train <- modError(y, fitted)
  } else {
    fitted.prob <- c(predict(mod, x, index = num.lambda, type = "response"))
    fitted <- ifelse(fitted.prob >= .5, 1, 0)
    fitted <- factor(levels(y0)[fitted + 1], levels = levels(y0))
    error.train <- modError(y0, fitted)
  }


  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression") {
      predicted <- c(predict(mod, x.test, index = num.lambda, type = "response"))
    } else {
      predicted.prob <- c(predict(mod, x.test, index = num.lambda, type = "response"))
      predicted <- ifelse(predicted.prob >= .5, 1, 0)
      predicted <- factor(levels(y0)[predicted + 1], levels = levels(y0))
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
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
                 parameters = list(num.lambda = num.lambda,
                                   lambda = lambda,
                                   family = family,
                                   degrees = degrees,
                                   gamma = gamma,
                                   dfs = dfs,
                                   bases = bases,
                                   tol = tol,
                                   max.iter = max.iter),
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

} # rtemis::s.GAMSEL
