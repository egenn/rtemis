# s.GLS.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.lambdamd.org

#' Generalized Least Squares [R]
#'
#' Train a Generalized Least Squares regression model using \code{nlme::gls}
#'
#' @inheritParams s.GLM
#' @param ... Additional arguments
#' @return \link{rtMod}
#' @author Efstathios D. Gennatas
#' @family Supervised Learning
#' @export

s.GLS <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  interactions = FALSE,
                  nway.interactions = 0,
                  covariate = NULL,
                  weights = NULL,
                  intercept = TRUE,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  na.action = na.exclude,
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  trace = 0,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.GLS))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.GLS))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test, verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  mod.name <- "GLS"
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # [ FORMULA ] ====
  df.train <- cbind(x, y = y)

  if (nway.interactions > 0) {
    .formula <- paste0(y.name, " ~ .^", nway.interactions)
  } else if (interactions) {
    .formula <- paste(y.name, "~ .*.")
  } else if (!is.null(covariate)) {
    features <- xnames[!grepl(covariate, xnames)]
    .formula <- paste(y.name, "~", paste(features, "*", covariate, collapse = " + "))
  } else {
    .formula <- paste(y.name, "~ .")
  }

  # Intercept
  if (!intercept) .formula <- paste(.formula, "- 1")
  .formula <- as.formula(.formula)

  # [ GLS ] ====
  if (verbose) msg("Trainings GLS...", newline.pre = TRUE)
  args <- c(list(model = .formula, data = df.train, na.action = na.action),
            list(...))
  mod <- do.call(nlme::gls, args)
  if (trace > 0) print(summary(mod))

  # [ FITTED ] ====
  fitted <- as.numeric(mod$fitted)
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (!is.null(y.test) && length(y.test) > 1) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(formula = .formula)
  extra <- list()
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
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 varimp = mod$coefficients[-1],
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

} # rtemis::s.GLS
