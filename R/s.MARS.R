# s.MARS.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io
# method = "cv" fails to find nk and penalty

#' Multivariate adaptive regression splines (MARS) [C, R]
#'
#' Trains a MARS model using \code{earth::earth}.
#' For more info on algorithm hyperparameters, see \code{?earth::earth}
#'
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as \code{x}
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param degree [gS] Integer: Maximum degree of interaction. Default = 2
#' @param penalty [gS] Float: GCV penalty per knot. 0 penalizes only terms, not knots.
#' -1 means no penalty. Default = 3
#' @param pmethod [gS] String: Pruning method: "backward", "none", "exhaustive", "forward",
#' "seqrep", "cv". Default = "forward"
#' @param nprune [gS] Integer: Max N of terms (incl. intercept) in the pruned model
#' @param nk [gS] Integer: Maximum number of terms created by the forward pass.
#' See \code{earth::earth}
#' @param ... Additional parameters to pass to \code{earth::earth}
#' @return Object of class \link{rtMod}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.MARS <- function(x, y = NULL,
                   x.test = NULL, y.test = NULL,
                   x.name = NULL, y.name = NULL,
                   grid.resample.rtset = rtset.grid.resample(),
                   weights = NULL,
                   ipw = TRUE,
                   ipw.type = 2,
                   upsample = FALSE,
                   upsample.seed = NULL,
                   glm = NULL,
                   wp = NULL,
                   na.action = na.fail,
                   pmethod = "forward",
                   nprune = NULL,
                   degree = 2,
                   ncross = 1,
                   nfold = 4,
                   penalty = 3, # if (degree > 1) 3 else 2
                   thresh = 0,
                   newvar.penalty = 0,
                   fast.k = 2,
                   nk = min(200, max(20, 2 * NCOL(x))) + 1,
                   metric = NULL,
                   maximize = NULL,
                   n.cores = rtCores,
                   print.plot = TRUE,
                   plot.fitted = NULL,
                   plot.predicted = NULL,
                   plot.theme = getOption("rt.fit.theme", "lightgrid"),
                   question = NULL,
                   verbose = TRUE,
                   trace = 0,
                   save.mod = FALSE,
                   outdir = NULL, ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.MARS))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "MARS"

  # [ DEPENDENCIES ] ====
  if (!depCheck("earth", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(s.MARS)); stop("x is missing")
  }
  if (is.null(y) & NCOL(x) < 2) { print(args(s.MARS)); stop("y is missing") }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

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
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  x0 <- if (upsample) dt$x0 else x
  y0 <- if (upsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (type == "Classification" & is.null(glm)) {
    glm <- list(family = binomial)
  }
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # [ GRID SEARCH ] ====
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Regression") {
      metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    }
  }

  if (is.null(maximize)) {
    maximize <- if (type == "Classification") TRUE else FALSE
  }

  gs <- NULL
  if (gridCheck(pmethod, degree, nprune, penalty, nk)) {
    gs <- gridSearchLearn(x0, y0, mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(pmethod = pmethod,
                                             degree = degree,
                                             nprune = nprune,
                                             penalty = penalty,
                                             nk = nk),
                          fixed.params = list(glm = glm),
                          weights = weights,
                          minimize = "MSE", verbose = verbose, n.cores = n.cores)
    pmethod <- as.character(gs$best.tune$pmethod)
    degree <- gs$best.tune$degree
    nprune <- gs$best.tune$nprune
    penalty <- gs$best.tune$penalty
    nk <- gs$best.tune$nk
  }

  # [ EARTH ] ====
    if (verbose) msg("Training MARS model...", newline = TRUE)
    # parameterSummary(pmethod, degree, nprune, ncross, nfold, penalty, nk)
    # We do not pass penalty or nk if pmethod is "cv", because they are not handled correctly by update.earth or
    # related function and error out.
    if (pmethod != "cv") {
      mod <- earth::earth(x, y,
                          weights = .weights,
                          wp = wp,
                          na.action = na.action,
                          pmethod = pmethod,
                          degree = degree,
                          nprune = nprune,
                          ncross = ncross,
                          nfold = nfold,
                          penalty = penalty,
                          thresh = thresh,
                          newvar.penalty = newvar.penalty,
                          fast.k = fast.k,
                          nk = nk,
                          glm = glm,
                          trace = ifelse(verbose, 1, 0), ...)
    } else {
      mod <- earth::earth(x, y,
                          weights = .weights,
                          wp = wp,
                          na.action = na.action,
                          pmethod = pmethod,
                          degree = degree,
                          nprune = nprune,
                          ncross = ncross,
                          nfold = nfold,
                          # penalty = penalty,
                          # nk = nk,
                          thresh = thresh, # check if these cause problems like above
                          newvar.penalty = newvar.penalty, #
                          fast.k = fast.k, #
                          glm = glm,
                          trace = ifelse(verbose, 1, 0), ...)
    }
    if (trace > 0) print(summary(mod))

  # [ FITTED ] ====
  # pred.type <- if (type == "Classification") "response" else "link"
  fitted <- predict(mod)
  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- ifelse(fitted.prob >= .5, 1, 0)
    fitted <- factor(levels(y)[fitted + 1])
    levels(fitted) <- levels(y)
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
      predicted <- predict(mod, x.test)
    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- ifelse(predicted.prob >= .5, 1, 0)
      predicted <- factor(levels(y)[predicted + 1])
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(gridSearch = gs)
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
                 fitted.prob = fitted.prob,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = NULL,
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

} # rtemis::s.MARS
