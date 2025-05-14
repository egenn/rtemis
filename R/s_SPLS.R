# s_SPLS.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org
# TODO: Add spgls option for Classification

#' Sparse Partial Least Squares Regression (C, R)
#'
#' Train an SPLS model using `spls::spls` (Regression) and `spls::splsda` (Classification)
#'
#' \[gS\] denotes argument can be passed as a vector of values, which will trigger
#' a grid search using `gridSearchLearn`
#' `np::npreg` allows inputs
#' with mixed data types.
#'
#' @inheritParams s_CART
#' @param k \[gS\] Integer: Number of components to estimate.
#' @param eta \[gS\] Float [0, 1): Thresholding parameter.
#' @param kappa \[gS\] Float \[0, .5\]: Only relevant for multivariate responses:
#' controls effect of concavity of objective
#'   function.
#' @param select \[gS\] Character: "pls2", "simpls". PLS algorithm for variable
#' selection.
#' @param fit \[gS\] Character: "kernelpls", "widekernelpls", "simpls",
#' "oscorespls". Algorithm for model fitting.
#' @param scale.x Logical: if TRUE, scale features by dividing each column by
#' its sample standard deviation
#' @param scale.y Logical: if TRUE, scale outcomes by dividing each column by
#' its sample standard deviation
#' @param maxstep \[gS\] Integer: Maximum number of iteration when fitting
#' direction vectors.
#' @param classifier Character: Classifier used by `spls::splsda` "lda"
#' or "logistic":
#' @param n.cores Integer: Number of cores to be used by
#' `gridSearchLearn`
#' @param trace If > 0 print diagnostic messages
#' @param ... Additional parameters to be passed to `npreg`
#'
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' y <- .6 * x + 12 + rnorm(100)
#' mod <- s_SPLS(x, y)
#' }
#' @export

s_SPLS <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  upsample = TRUE,
  downsample = FALSE,
  resample.seed = NULL,
  k = 2,
  eta = .5,
  kappa = .5,
  select = "pls2",
  fit = "simpls",
  scale.x = TRUE,
  scale.y = TRUE,
  maxstep = 100,
  classifier = c("lda", "logistic"),
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  metric = NULL,
  maximize = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  grid.verbose = verbose,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  n.cores = rtCores,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_SPLS))
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
  mod.name <- "SPLS"

  # Dependencies ----
  dependency_check("spls")

  # Arguments ----
  if (missing(x)) {
    print(args(s_SPLS))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_SPLS))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (k > NCOL(x)) {
    warning(
      "k cannot exceed number of features. Setting k to NCOL(x) = ",
      NCOL(x)
    )
    k <- NCOL(x)
  }

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
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (type == "Classification") {
    y1 <- as.numeric(y) - 1
    classifier <- match.arg(classifier)
  }

  # Grid Search ----
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

  if (gridCheck(k, eta, kappa, select, fit, maxstep)) {
    gs <- gridSearchLearn(
      x,
      y,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        k = k,
        eta = eta,
        kappa = kappa,
        select = select,
        fit = fit,
        maxstep = maxstep
      ),
      search.type = gridsearch.type,
      randomized.p = gridsearch.randomized.p,
      metric = metric,
      maximize = maximize,
      verbose = grid.verbose,
      n.cores = n.cores
    )
    k <- gs$best.tune$k
    eta <- gs$best.tune$eta
    kappa <- gs$best.tune$kappa
    select <- gs$best.tune$select
    fit <- gs$best.tune$fit
    maxstep <- gs$best.tune$maxstep
  } else {
    gs <- NULL
  }

  # spls::splsda/spls ----
  if (verbose) {
    msg20(
      "Training Sparse Partial Least Squares ",
      type,
      "...",
      newline.pre = TRUE
    )
  }
  if (type == "Classification") {
    # Cannot include select, scale.y, or trace options; see source
    mod <- spls::splsda(
      data.matrix(x),
      y1,
      K = k,
      eta = eta,
      kappa = kappa,
      classifier = classifier,
      # select = select,
      fit = fit,
      scale.x = scale.x,
      # scale.y = scale.y,
      maxstep = maxstep
    )
  } else {
    mod <- spls::spls(
      x,
      y,
      K = k,
      eta = eta,
      kappa = kappa,
      select = select,
      fit = fit,
      scale.x = scale.x,
      scale.y = scale.y,
      maxstep = maxstep,
      trace = verbose
    )
  }

  if (trace > 0) mod

  # Fitted ----
  fitted <- predict(mod, x, type = "fit")
  if (type == "Classification") {
    fitted <- factor(fitted)
    levels(fitted) <- levels(y)
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Coefficients ----
  coeffs <- spls::coef.spls(mod)

  # Predicted ----
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test, type = "fit")
    if (type == "Classification") {
      predicted <- factor(predicted)
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(coeffs = coeffs)
  rt <- rtModSet(
    mod = mod,
    mod.name = mod.name,
    type = type,
    gridsearch = gs,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = se.prediction,
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
} # rtemis::s_SPLS
