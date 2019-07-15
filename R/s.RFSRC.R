# s.RFSRC.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Random Forest for Classification, Regression, and Survival [C, R, S]
#'
#' Train a Random Forest for Regression, Classification, or Survival Regression
#' using \code{randomForestSRC}
#'
#' For Survival Regression, y must be an object of type \code{Surv}, created using
#' \code{survival::Surv(time, status)}
#' \code{mtry} is the only tunable parameter, but it usually only makes a small difference
#' and is often not tuned.
#'
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as \code{x}
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param n.trees Integer: Number of trees to grow. The more the merrier.
#' @param bootstrap String:
#' @param mtry Integer: Number of features sampled randomly at each split
#' @param outdir Optional. Path to directory to save output
#' @param ... Additional arguments to be passed to \code{randomForestSRC::rfsrc}
#' @return Object of class \link{rtMod}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s.RFSRC <- function(x, y = NULL,
                    x.test = NULL, y.test = NULL,
                    x.name = NULL, y.name = NULL,
                    n.trees = 1000,
                    weights = NULL,
                    ipw = TRUE,
                    ipw.type = 2,
                    upsample = FALSE,
                    upsample.seed = NULL,
                    bootstrap = "by.root",
                    mtry = NULL,
                    importance = TRUE,
                    proximity = TRUE,
                    nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                    nodedepth = NULL,
                    na.action = "na.impute",
                    trace = FALSE,
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
  print(args(s.RFSRC))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "RFSRC"

  # [ DEPENDENCIES ] ====
  if (!depCheck("randomForestSRC", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.RFSRC))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (is.null(trace)) trace <- if (verbose) n.trees/10 else FALSE

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
  if (is.null(weights) & ipw) weights <- dt$weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry, pad = 4)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(nodesize)) nodesize <- if (type == "Classification") 1 else 5

  # [ FORMULA ] ====
  if (type != "Survival") {
    df.train <- data.frame(y, x)
    colnames(df.train)[1] <- y.name
    .formula <- as.formula(paste(y.name, "~ ."))
  } else {
    time <- y[, 1]
    status <- y[, 2]
    df.train <- data.frame(time, status, x)
    # .formula <- as.formula(paste(y.name, "~ ."))
    .formula <- as.formula(Surv(time, status) ~ .)
  }

  # [ RFSRC ] ====
  if (verbose) msg("Training Random Forest SRC", type, "with", n.trees, "trees...",
                   newline.pre = TRUE)
  mod <- randomForestSRC::rfsrc(.formula, data = df.train,
                                ntree = n.trees,
                                bootstrap = bootstrap,
                                mtry = mtry,
                                case.wt = weights,
                                nodesize = nodesize,
                                nodedepth = nodedepth,
                                importance = importance,
                                proximity = proximity,
                                na.action = na.action,
                                do.trace = trace, ...)

  # [ FITTED ] ====
  if (proximity) {
    fit <- predict(mod, x, proximity = TRUE)
    fitted <- fit$predicted
    proximity.train <- fit$proximity
  } else {
    fit <- predict(mod, x)
    fitted <- fit$predicted
    proximity.train <- NULL
  }

  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- factor(apply(fitted.prob, 1, function(i) which.max(i)))
    levels(fitted) <- levels(y)
  } else {
    fitted <- as.numeric(fitted)
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  if (!is.null(x.test)) {
    if (proximity) {
      pred <- predict(mod, x.test, proximity = proximity)
      predicted <- pred$predicted
      proximity.test <- pred$proximity
    } else {
      pred <- predict(mod, x.test)
      predicted <- pred$predicted
      proximity.test <- NULL
    }

    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- factor(apply(predicted.prob, 1, function(i) which.max(i)))
      levels(predicted) <- levels(y)
    } else {
      predicted <- as.numeric(predicted)
    }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    pred <- predicted <- error.test <- proximity.test <- NULL
  }

  # [ OUTRO ] ====
  extra <- list(fit = fit,
                proximity.test = proximity.test,
                proximity.train = proximity.train)
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

} # rtemis::s.RFSRC
