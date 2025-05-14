# s_BayesGLM.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Bayesian GLM
#'
#' Train a bayesian GLM using `arm::bayesglm`
#'
#' @inheritParams s_CART
#' @param family Character or function for the error distribution and link function to
#' be used. See `arm::bayesglm` for details.
#' @param prior.mean Numeric, vector: Prior mean for the coefficients. If scalar,
#' it will be replicated to length N features.
#' @param prior.scale Numeric, vector: Prior scale for the coefficients. Default = NULL,
#' which results in 2.5 for logit, 2.5*1.6 for probit. If scalar,
#' it will be replicated to length N features.
#' @param prior.df Numeric: Prior degrees of freedom for the coefficients. Set to 1 for
#' t distribution; set to Inf for normal prior distribution. If scalar,
#' it will be replicated to length N features.
#' @param prior.mean.for.intercept Numeric: Prior mean for the intercept.
#' @param prior.scale.for.intercept Numeric: Default = NULL, which results in 10 for a
#' logit model, and 10*1.6 for probit model.
#' @param prior.df.for.intercept Numeric: Prior df for the intercept.
#' @param min.prior.scale Numeric: Minimum prior scale for the coefficients.
#' @param scaled Logical: If TRUE, the scale for the prior distributions are:
#' For feature with single value, use `prior.scale`, for predictor with two values,
#' use `prior.scale/range(x)`, for more than two values, use `prior.scale/(2*sd(x))`.
#' If response is gaussian, `prior.scale` is multiplied by `2 * sd(y)`.
#' Default = TRUE
#' @param keep.order Logical: If TRUE, the feature positions are maintained, otherwise they are
#' reordered: main effects, interactions, second-order, third-order, etc.
#' @param drop.baseline Logical: If TRUE, drop the base level of factor features.
#' @param maxit Integer: Maximum number of iterations
#' @param ... Additional parameters to pass to `arm::bayesglm`
#'
#' @family Bayesian
#' @family Supervised Learning
#' @export
#' @author E.D. Gennatas

s_BayesGLM <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  family = NULL,
  prior.mean = 0,
  prior.scale = NULL,
  prior.df = 1,
  prior.mean.for.intercept = 0,
  prior.scale.for.intercept = NULL,
  prior.df.for.intercept = 1,
  min.prior.scale = 1e-12,
  scaled = TRUE,
  keep.order = TRUE,
  drop.baseline = TRUE,
  maxit = 100,
  x.name = NULL,
  y.name = NULL,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  metric = NULL,
  maximize = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  grid.verbose = verbose,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_BayesGLM))
    return(invisible(9))
  }
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
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
  mod.name <- "BayesGLM"

  # Dependencies ----
  dependency_check("arm")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_BayesGLM))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    ifw = ifw,
    ifw.type = ifw.type,
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
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (type == "Classification") nlevels <- length(levels(y))

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

  if (is.null(family)) {
    family <- if (type == "Regression") gaussian else binomial
  }

  # Formula ----
  df.train <- data.frame(x, y)
  colnames(df.train)[ncol(df.train)] <- y.name
  .formula <- as.formula(paste(y.name, "~ ."))

  extra.args <- list(...)
  parameters <- c(
    list(
      ifw = ifw,
      ifw.type = ifw.type,
      upsample = upsample,
      resample.seed = resample.seed,
      prior.mean = prior.mean,
      prior.scale = prior.scale,
      prior.df = prior.df,
      prior.mean.for.intercept = prior.mean.for.intercept,
      prior.scale.for.intercept = prior.scale.for.intercept,
      prior.df.for.intercept = prior.df.for.intercept,
      min.prior.scale = min.prior.scale,
      scaled = scaled,
      keep.order = keep.order,
      drop.baseline = drop.baseline,
      maxit = maxit
    ),
    extra.args
  )

  # BayesGLM ----
  if (verbose) msg2("Training Bayesian GLM...", newline.pre = TRUE)
  args <- c(
    list(
      formula = .formula,
      data = df.train,
      family = family,
      prior.mean = prior.mean,
      prior.scale = prior.scale,
      prior.df = prior.df,
      prior.mean.for.intercept = prior.mean.for.intercept,
      prior.scale.for.intercept = prior.scale.for.intercept,
      prior.df.for.intercept = prior.df.for.intercept,
      min.prior.scale = min.prior.scale,
      scaled = scaled,
      keep.order = keep.order,
      drop.baseline = drop.baseline,
      maxit = maxit
    ),
    extra.args
  )
  mod <- do.call(arm::bayesglm, args)

  # Fitted ----
  if (type == "Classification") {
    fitted.prob <- 1 - predict(mod, type = "response")
    fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
    levels(fitted) <- levels(y)
  } else {
    fitted.prob <- NULL
    fitted <- predict(mod, x)
  }
  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- NULL
  if (!is.null(x.test)) {
    if (type == "Classification") {
      predicted.prob <- 1 - predict(mod, x.test, type = "response")
      predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(predicted) <- levels(y)
    } else {
      predicted <- predict(mod, x.test)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- error.test <- NULL
  }

  # Outro ----
  extra <- list()
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    parameters = parameters,
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
    varimp = mod$coefficients[-1],
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
} # rtemis::s_BayesGLM
