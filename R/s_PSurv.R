# s_PSurv.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org
# TODO: add strata() support

#' Parametric Survival Regression \[S\]
#'
#' Fit a parametric survival regression model using `survival::survreg`
#'
#' @inheritParams s_CART
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Object of class "Surv" created using `survival::Surv`
#' @param x.test (Optional) Numeric vector or matrix of testing set features
#'   must have set of columns as `x`
#' @param y.test (Optional) Object of class "Surv" created using `survival::Surv`
#' @param weights Float: Vector of case weights
#' @param ... Additional parameters to pass to `survival::survreg`
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Survival Regression
#' @export

s_PSurv <- function(
  x,
  y,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  weights = NULL,
  dist = "weibull",
  control = survival::survreg.control(),
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  save.mod = FALSE,
  outdir = NULL,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_PSurv))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "PSurv"

  # Dependencies ----
  dependency_check("survival")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_PSurv))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  if (!is.null(x.test)) x.test <- dt$x.test
  if (!is.null(y.test)) y.test <- dt$y.test
  type <- dt$type
  checkType(type, "Survival", mod.name)
  xnames <- dt$xnames
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (type != "Survival") {
    stop("Please ensure 'y' is a survival object created by survival::Surv")
  }

  # Formula ----
  .formula <- y ~ .

  # SURVREG ----
  if (verbose)
    msg2("Training Parametric Survival Regression model...", newline.pre = TRUE)
  mod <- survival::survreg(
    .formula,
    data = x,
    weights = weights,
    dist = dist,
    control = control,
    ...
  )
  if (trace > 0) print(summary(mod))

  # Fitted ----
  fitted <- predict(mod, newdata = x, type = "response")
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, newdata = x.test, type = "response")
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list()
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    bag.resample.params = NULL,
    fitted.bag = NULL,
    fitted = fitted,
    se.fit.bag = NULL,
    se.fit = NULL,
    error.train = error.train,
    predicted.bag = NULL,
    predicted = predicted,
    se.predicted.bag = NULL,
    se.prediction = NULL,
    error.test = error.test,
    question = question,
    extra = extra
  )

  rtMod.out(
    rt,
    print.plot,
    plot.fitted,
    plot.predicted,
    y.test,
    mod.name,
    outdir,
    save.mod,
    verbose,
    plot.theme
  )

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_PSurv
