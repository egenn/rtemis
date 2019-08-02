# s.SPLS.R
# ::rtemis::
# 2016-8 Efstathios D. Gennatas egenn.github.io
# TODO: Add spgls option for Classification

#' Sparse Partial Least Squares Regression [C, R]
#'
#' Train an SPLS model using \code{spls::spls} (Regression) and \code{spls::splsda} (Classification)
#'
#' [gS] denotes argument can be passed as a vector of values, which will trigger a grid search using \link{gridSearchLearn}
#' \code{np::npreg} allows inputs with mixed data types.
#'
#' @inheritParams s.CART
#' @param k [gS] Integer: Number of components to estimate. Default = 2
#' @param eta [gS] Float [0, 1): Thresholding parameter. Default = .5
#' @param kappa [gS] Float [0, .5]: Only relevant for multivariate responses: controls effect of concavity of objective
#'   function. Default = .5
#' @param select [gS] String: "pls2", "simpls". PLS algorithm for variable selection. Default = "pls2"
#' @param fit [gS] String: "kernelpls", "widekernelpls", "simpls", "oscorespls". Algorithm for model fitting.
#'   Default = "simpls"
#' @param scale.x Logical: if TRUE, scale features by dividing each column by its sample standard deviation
#' @param scale.y Logical: if TRUE, scale outcomes by dividing each column by its sample standard deviation
#' @param maxstep [gS] Integer: Maximum number of iteration when fitting direction vectors. Default = 100
#' @param classifier String: Classifier used by \code{spls::splsda} "lda" or "logistic": Default = "lda"
#' @param n.cores Integer: Number of cores to be used by \link{gridSearchLearn}, if applicable
#' @param ... Additional parameters to be passed to \code{npreg}
#' @return Object of class \pkg{rtemis}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' y <- .6 * x + 12 + rnorm(100)
#' mod <- s.SPLS(x, y)}
#' @export

s.SPLS <- function(x, y = NULL,
                   x.test = NULL, y.test = NULL,
                   x.name = NULL, y.name = NULL,
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
                   grid.resample.rtset = rtset.resample("kfold", 5),
                   grid.search.type = c("exhaustive", "randomized"),
                   grid.randomized.p = .1,
                   metric = NULL,
                   maximize = NULL,
                   print.plot = TRUE,
                   plot.fitted = NULL,
                   plot.predicted = NULL,
                   plot.theme = getOption("rt.fit.theme", "lightgrid"),
                   question = NULL,
                   rtclass = NULL,
                   verbose = TRUE,
                   trace = 0,
                   grid.verbose = TRUE,
                   outdir = NULL,
                   save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
                   n.cores = rtCores, ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.SPLS))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "SPLS"

  # [ DEPENDENCIES ] ====
  if (!depCheck("spls", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(s.SPLS))
    stop("x is missing")
  }
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.SPLS))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (k > NCOL(x)) {
    warning("k cannot exceed number of features. Setting k to NCOL(x) = ", NCOL(x))
    k <- NCOL(x)
  }

  # [ DATA ] ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (type == "Classification") {
    y1 <- as.numeric(y) - 1
    classifier <- match.arg(classifier)
  }

  # [ GRID SEARCH ] ====
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
    gs <- gridSearchLearn(x, y,
                          mod = mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(k = k, eta = eta, kappa = kappa,
                                             select = select, fit = fit, maxstep = maxstep),
                          search.type = grid.search.type,
                          randomized.p = grid.randomized.p,
                          metric = metric,
                          maximize = maximize,
                          verbose = grid.verbose,
                          n.cores = n.cores)
    k <- gs$best.tune$k
    eta <- gs$best.tune$eta
    kappa <- gs$best.tune$kappa
    select <- gs$best.tune$select
    fit <- gs$best.tune$fit
    maxstep <- gs$best.tune$maxstep
  }

  # [ SPLS ] ====
  if (verbose) msg0("Training Sparse Partial Least Squares ", type , "...",
                     newline.pre = TRUE)
  if (type == "Classification") {
    # Cannot include select, scale.y, or trace options; see source
    mod <- spls::splsda(data.matrix(x), y1,
                        K = k,
                        eta = eta,
                        kappa = kappa,
                        classifier = classifier,
                        # select = select,
                        fit = fit,
                        scale.x = scale.x,
                        # scale.y = scale.y,
                        maxstep = maxstep)
  } else {
    mod <- spls::spls(x, y,
                      K = k,
                      eta = eta,
                      kappa = kappa,
                      select = select,
                      fit = fit,
                      scale.x = scale.x,
                      scale.y = scale.y,
                      maxstep = maxstep,
                      trace = verbose)
  }

  if (trace > 0) mod

  # [ FITTED ] ====
  fitted <- predict(mod, x, type = "fit")
  if (type == "Classification") {
    fitted <- factor(fitted)
    levels(fitted) <- levels(y)
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ COEFFS ] ====
  coeffs <- spls::coef.spls(mod)

  # [ PREDICTED ] ====
  predicted <- se.prediction <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test, type = "fit")
    if (type == "Classification") {
      predicted <- factor(predicted)
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(coeffs = coeffs)
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
                  se.prediction = se.prediction,
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

} # rtemis::s.SPLS
