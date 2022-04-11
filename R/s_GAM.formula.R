# s_GAM.formula.R
# ::rtemis::
# 2015 E.D. Gennatas lambdamd.org
# rtTODO: use s_GAM.default so that classification works

#' Generalized Additive Model (GAM) {C, R}
#'
#' Trains a GAM using \code{mgcv::gam} and validates it.
#' Input will be used to create a formula of the form:
#' \deqn{y = s(x_{1}, k = gam.k) + s(x_{2}, k = gam.k) + ... + s(x_{n}, k = gam.k)}
#'
#' \link{s_GAM.default} is the preferred way to train GAMs
#' @inheritParams s_GLM
#' @param covariates Factors to be included as covariates in model building
#' @param covariates.test Factors to be included as covariates in model validation
#' @param k Integer. Number of bases for smoothing spline
#' @param ... Additional arguments to be passed to \code{mgcv::gam}
#' @return \link{rtMod}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s_GAM.formula <- function(formula,
                          data,
                          data.test = NULL,
                          x.name = NULL, y.name = NULL,
                          k = 6,
                          family = gaussian(),
                          weights = NULL,
                          method = "REML",
                          select = FALSE,
                          verbose = TRUE,
                          print.plot = TRUE,
                          plot.fitted = NULL,
                          plot.predicted = NULL,
                          plot.theme = getOption("rt.theme"),
                          na.action = na.exclude,
                          question = NULL,
                          n.cores = rtCores,
                          outdir = NULL,
                          save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ----
  if (missing(formula) | missing(data)) {
    print(args(s_GAM.formula))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "GAM"

  # Dependencies ----
  dependency_check("mgcv")

  # Arguments ----
  .formula <- as.formula(formula)
  if (is.null(data)) stop("Please provide data")
  df.train <- data
  df.test <- data.test

  if (is.null(x.name)) x.name <- "x"
  y.name <- all.vars(.formula[[2]])

  y <- df.train[[y.name]]
  y.test <- df.test$y.test # NULL if df.test is null

  xnames <- all.vars(.formula[[3]])
  # if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) dataSummary(df.train[, -ncol(df.train)], y, df.test[, -max(ncol(df.test), 1, na.rm = T)], y.test)

  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (is.null(weights)) weights <- rep(1, NROW(data))

  # GAM ]
  if (verbose) msg("Training GAM...")
  args <- c(list(formula = .formula,
                 family = family,
                 data = df.train,
                 # weights = weights,
                 select = select,
                 method = method,
                 na.action = na.action),
            list(...))
  mod <- do.call(mgcv::gam, args)
  if (verbose) print(summary(mod))

  # Fitted ----

  fitted <- predict(mod, df.train, se.fit = TRUE)
  se.fit <- as.numeric(fitted$se.fit)
  fitted <- as.numeric(fitted$fit)
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(df.test)) {
    predicted <- predict(mod, data.frame(df.test), se.fit = TRUE)
    se.prediction <- predicted$se.fit
    predicted <- predicted <- as.numeric(predicted$fit)

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = "Regression",
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

} # rtemis::s_GAM.formula
