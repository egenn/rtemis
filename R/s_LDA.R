# s_LDA.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Linear Discriminant Analysis
#'
#' Train an LDA Classifier using `MASS::lda`
#'
#' Note: LDA requires all predictors to be numeric.
#' The variable importance output ("varimp") is the vector of coefficients for LD1
#' @inheritParams s_CART
#' @param prior Numeric: Prior probabilities of class membership
#' @param method "moment" for standard estimators of the mean and variance, "mle" for
#' MLEs, "mve" to use cov.mve, or "t" for robust estimates based on a t distribution
#' @param nu Integer: Degrees of freedom for method = "t"
#' @param ... Additional arguments passed to `MASS::lda`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_LDA <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  prior = NULL,
  method = "moment",
  nu = NULL,
  upsample = TRUE,
  downsample = FALSE,
  resample.seed = NULL,
  x.name = NULL,
  y.name = NULL,
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
    print(args(s_LDA))
    invisible(9)
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
  mod.name <- "LDA"

  # Dependencies ----
  dependency_check("MASS")

  # Arguments ----
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
  checkType(type, "Classification", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)

  # Check all predictors are numeric
  if (any(!sapply(x, is.numeric))) {
    stop("All predictors need to be numeric")
  }

  # MASS::lda ----
  params <- c(
    list(
      x = x,
      grouping = y,
      method = method,
      nu = nu
    ),
    list(...)
  )
  if (!is.null(prior)) params$prior <- prior
  if (verbose)
    msg2("Running Linear Discriminant Analysis...", newline.pre = TRUE)
  mod <- do.call(MASS::lda, args = params)

  # Fitted ----
  fitted.raw <- predict(mod, x)
  fitted <- fitted.raw$class
  fitted.prob <- fitted.raw$posterior
  train.projections <- fitted.raw$x
  error.train <- mod_error(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.raw <- predicted <- predicted.prob <- test.projections <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted.raw <- predict(mod, x.test)
    predicted <- predicted.raw$class
    predicted.prob <- predicted.raw$posterior
    test.projections <- predicted.raw$x
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(
    fitted.prob = fitted.prob,
    predicted.prob = predicted.prob,
    train.projections = train.projections,
    test.projections = test.projections,
    params = params
  )
  rt <- rtMod$new(
    mod.name = mod.name,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    xnames = xnames,
    mod = mod,
    type = type,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
    varimp = coef(mod)[, 1],
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
} # rtemis::s_LDA
