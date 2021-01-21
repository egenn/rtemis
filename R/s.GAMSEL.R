# s.GAMSEL.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org
# TODO: Update to use cv.gamsel similar to s.GLMNET
# TODO: Add special case in rtMod predict

#' Regularized Generalized Additive Model (GAMSEL) [C, R]
#'
#' Trains a GAMSEL model using \code{gamsel2::gamsel}.
#'
#' @inheritParams s.GLM
#' @inheritParams gamsel2::gamsel
#' @return \link{rtMod}
#' @author E.D. Gennatas
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
                     downsample = FALSE,
                     resample.seed = NULL,
                     lambda = NULL,
                     force.lambda = NULL, # force gamsel instead of cv.gamsel
                     min.unique.perfeat = 4,
                     num.lambda = 50,
                     family = NULL,
                     degrees = NULL,
                     min.degree = 1,
                     max.degree = 9,
                     gamma = 0.4,
                     dfs = NULL,
                     min.df = 1,
                     max.df = 5,
                     n.folds = 10,
                     which.lambda = c("lambda.min", "lambda.1se"),
                     failsafe = TRUE,
                     failsafe.lambda = .1,
                     tol = 1e-04,
                     max.iter = 2000,
                     parallel = FALSE,
                     # cleanup = TRUE, # call removed from gamsel
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
  if (!depCheck("gamsel2", verbose = FALSE)) {
    cat("\n")
    stop("Please install dependency using remotes::install_github('egenn/gamsel2') and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.GAMSEL))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  which.lambda <- match.arg(which.lambda)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)

  # [ DATA ] ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
                    ipw.type = ipw.type,
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
    y <- 2 - as.numeric(y)
  }

  unique_perfeat <- apply(x, 2, function(i) length(unique(i)))
  if (trace > 1) cat(".: Unique vals per feat:", unique_perfeat, "\n")
  if (any(unique_perfeat < min.unique.perfeat) && failsafe) {
    # Cannot run cv.gamsel, force lambda
    force.lambda <- failsafe.lambda
  }

  #
  # if (is.null(degrees)) {
  #   degrees <- sapply(seq_len(n.features), function(i)
  #     max(min.degree, min(unique_perfeat[i] - 1, max.degree)))
  # }
  #
  # if (length(degrees) < n.features) degrees <- rep(degrees, n.features)[seq_len(n.features)]
  # if (trace > 1) cat(".: 'degrees' set to:", degrees, "\n")
  #
  # if (is.null(dfs)) {
  #   # -2 is playing it safe to test: fix
  #   dfs <- sapply(seq_len(n.features), function(i) max(min.df, min(degrees[i] - 2, max.df)))
  # }
  # if (trace > 1) cat(".: 'dfs' set to:", dfs, "\n")
  # if (length(dfs) != n.features) dfs <- rep(dfs, n.features)[seq_len(n.features)]

  # [ GAMSEL ] ====
  # bases <- gamsel2::pseudo.bases(x, degrees, dfs, parallel = parallel, ...)
  if (verbose) msg("Training GAMSEL...", newline.pre = TRUE)
  args <- list(x = x,
               y = y,
               num_lambda = num.lambda,
               lambda = lambda,
               family = family,
               degrees = degrees,
               max.degree = max.degree,
               gamma = gamma,
               dfs = dfs,
               max.df = max.df,
               failsafe = failsafe,
               # nfolds = n.folds,
               # bases = gamsel2::pseudo.bases(x, degrees, dfs, parallel = parallel, ...),
               tol = tol,
               max_iter = max.iter,
               traceit = trace > 0,
               parallel = parallel)
  if (is.null(force.lambda)) {
    # use cv.gamsel
    args <- c(args, nfolds = n.folds)
    mod <- do.call(gamsel2::cv.gamsel, args)
  } else {
    args$lambda <- force.lambda
    mod <- do.call(gamsel2::gamsel, args)
  }
  # mod <- do.call(gamsel2::gamsel, args)

  # if (cleanup) mod$call <- head(mod$call)
  # nlambdas <- length(mod$lambdas)
  lambda.index <- if (which.lambda == "lambda.min") mod$index.min else mod$index.1se

  # [ FITTED ] ====
  # TODO: switch both fitted and predicted to predict.rtMod
  if (type == "Regression") {
    # in gamsel2, this works with both "gamsel" and "cv.gamsel" objects
    fitted <- c(predict(mod, x, index = lambda.index, type = "response"))
    error.train <- modError(y, fitted)
    fitted.prob <- NULL
  } else {
    fitted.prob <- c(predict(mod, x, index = lambda.index, type = "response"))
    fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
    levels(fitted) <- levels(y0)
    error.train <- modError(y0, fitted, fitted.prob)
  }

  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression") {
      predicted <- c(predict(mod, x.test, index = lambda.index, type = "response"))
    } else {
      predicted.prob <- c(predict(mod, x.test, index = lambda.index, type = "response"))
      predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(predicted) <- levels(y0)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  lambda.used <- if (which.lambda == "lambda.min") mod$lambda[mod$lambda.min] else mod$lambda[mod$index.1se]
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
                                   max.degree = max.degree,
                                   gamma = gamma,
                                   dfs = dfs,
                                   max.df = max.df,
                                   failsafe = failsafe,
                                   # bases = bases,
                                   which.lambda = which.lambda,
                                   lambda.used = lambda.used,
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
