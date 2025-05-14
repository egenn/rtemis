# s_SGD.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Stochastic Gradient Descent (SGD) (C, R)
#'
#' Train a model by Stochastic Gradient Descent using `sgd::sgd`
#'
#' From `sgd::sgd`:
#' "Models: The Cox model assumes that the survival data is ordered when passed in, i.e.,
#' such that the risk set of an observation i is all data points after it."
#'
#' @inheritParams s_GLM
#' @inheritParams sgd::sgd
#' @param ... Additional arguments to be passed to `sgd.control`
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_SGD <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  model = NULL,
  model.control = list(lambda1 = 0, lambda2 = 0),
  sgd.control = list(method = "ai-sgd"),
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_SGD))
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
  mod.name <- "SGD"

  # Dependencies ----
  dependency_check("sgd")

  # Arguments ----
  if (missing(x)) {
    print(args(s_SGD))
    stop("x is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (is.null(weights)) weights <- rep(1, length(y))
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (type == "Classification") {
    nlevels <- length(levels(y))
    if (nlevels > 2) stop("Only binary classification is supported")
    if (is.null(model)) model <- "glm"
    if (is.null(model.control$family)) {
      model.control$family <- binomial(link = "logit")
    }
    y0 <- y
    y <- as.numeric(y) - 1

    # defaults from logistic example
    if (is.null(sgd.control$reltol)) sgd.control$reltol <- 1e-5
    if (is.null(sgd.control$npasses)) sgd.control$npasses <- 200
  } else if (type == "Regression") {
    if (is.null(model)) model <- "glm"
    if (is.null(model.control$family)) {
      model.control$family <- gaussian(link = "identity")
    }
  } else {
    if (is.null(model)) model <- "cox"
  }

  x <- data.matrix(cbind(Intercept = 1, x))
  if (!is.null(x.test)) {
    x.test <- data.matrix(cbind(Intercept = 1, x.test))
  }

  # sgd::sgd ----
  if (verbose) msg2("Training SGD model...", newline.pre = TRUE)
  mod <- sgd::sgd(
    x = x,
    y = y,
    model = model,
    model.control = model.control,
    sgd.control = sgd.control,
    ...
  )

  # Fitted ----
  fitted <- mod$fitted.values[, 1]
  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- factor(levels(y0)[as.numeric(fitted >= .5) + 1])
    levels(fitted) <- levels(y0)
  } else {
    fitted.prob <- NULL
  }
  if (type == "Classification") {
    error.train <- mod_error(y0, fitted)
  } else {
    error.train <- mod_error(y, fitted)
  }

  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- as.numeric(predict(mod, x.test))
    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- factor(levels(y0)[as.numeric(predicted >= .5) + 1])
      levels(predicted) <- levels(y0)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    y.train = if (type == "Classification") y0 else y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    fitted = fitted,
    fitted.prob = fitted.prob,
    error.train = error.train,
    predicted = predicted,
    predicted.prob = predicted.prob,
    parameters = list(
      model = model,
      model.control = model.control,
      sgd.control = sgd.control
    ),
    error.test = error.test,
    varimp = mod$coefficients,
    question = question
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
} # rtemis::s_SGD
