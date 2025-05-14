# s_PolyMARS.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org
# method = "cv" fails to find nk and penalty

#' Multivariate adaptive polynomial spline regression (POLYMARS) (C, R)
#'
#' Trains a POLYMARS model using `polspline::polymars` and validates it
#'
#' @inheritParams s_CART
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as `x`
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param maxsize Integer: Maximum number of basis functions to use
#' @param trace Integer: If `> 0`, print summary of model
#' @param ... Additional parameters to pass to `polspline::polymars`
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_PolyMARS <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  grid.resample.params = setup.grid.resample(),
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  maxsize = ceiling(min(
    6 *
      (nrow(x)^{
        1 / 3
      }),
    nrow(x) / 4,
    100
  )),
  #  classify = NULL,
  n.cores = rtCores,
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
    print(args(s_PolyMARS))
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
  mod.name <- "POLYMARS"

  # Dependencies ----
  dependency_check("polspline")

  # Arguments ----
  if (missing(x)) {
    print(args(s_PolyMARS))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_PolyMARS))
    stop("y is missing")
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
  checkType(type, c("Classification", "Regression"), mod.name)
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  if (is.null(.weights)) .weights <- rep(1, nrow(x))
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  # if (is.null(classify))
  classify <- ifelse(type == "Classification", TRUE, FALSE)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Grid Search ----
  if (gridCheck(maxsize)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(maxsize = maxsize),
      fixed.params = list(
        classify = classify,
        ifw = ifw,
        ifw.type = ifw.type
      ),
      weights = weights,
      metric = "MSE",
      maximize = FALSE,
      verbose = verbose,
      n.cores = n.cores
    )
    maxsize <- gs$best.tune$maxsize
  } else {
    gs <- NULL
  }

  # polspline::polymars ----
  if (verbose) msg2("Training POLYMARS model...", newline.pre = TRUE)
  mod <- polspline::polymars(
    y,
    x,
    weights = .weights,
    maxsize = maxsize,
    verbose = verbose,
    classify = classify,
    ...
  )
  if (trace > 0) print(summary(mod))

  # Fitted ----
  fitted <- predict(mod, x)
  if (type == "Classification") {
    fitted <- apply(fitted, 1, which.max)
    fitted <- factor(levels(y)[fitted])
    levels(fitted) <- levels(y)
  }

  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (type == "Classification") {
      predicted <- apply(predicted, 1, which.max)
      predicted <- factor(levels(y)[predicted])
      levels(predicted) <- levels(y)
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
    gridsearch = gs,
    parameters = list(maxsize = maxsize),
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
} # rtemis::s_PolyMARS
