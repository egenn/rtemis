# s_SVM.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org

#' Support Vector Machines (C, R)
#'
#' Train an SVM learner using `e1071::svm`
#'
#' \[gS\] denotes parameters that will be tuned by cross-validation if more than one value is passed.
#' Regarding SVM tuning, the following guide from the LIBSVM authors can be useful:
#' http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
#' They suggest searching for cost = 2 ^ seq(-5, 15, 2) and
#' gamma = 2 ^ seq(-15, 3, 2)
#'
#' @inheritParams s_GLM
#' @inheritParams s_CART
#' @inheritParams resample
#' @param class.weights Float, length = n levels of outcome: Weights for each
#' outcome class.For classification, `class.weights` takes precedence over
#' `ifw`, therefore set `class.weights = NULL` if using `ifw`.
#' @param kernel Character: "linear", "polynomial", "radial", "sigmoid"
#' @param degree \[gS\] Integer: Degree for `kernel = "polynomial"`.
#' @param cost \[gS\] Float: Cost of constraints violation; the C constant of the
#' regularization term in the Lagrange
#'   formulation.
#' @param probability Logical: If TRUE, model allows probability estimates
#' @param gamma \[gS\] Float: Parameter used in all kernels except `linear`
#' @param coef0 \[gS\] Float: Parameter used by kernels `polynomial` and
#' `sigmoid`
#' @param ... Additional arguments to be passed to `e1071::svm`
#'
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_SVM <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  class.weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  kernel = "radial",
  degree = 3,
  gamma = NULL,
  coef0 = 0,
  cost = 1,
  probability = TRUE,
  metric = NULL,
  maximize = NULL,
  plot.fitted = NULL,
  plot.predicted = NULL,
  print.plot = FALSE,
  plot.theme = rtTheme,
  n.cores = rtCores,
  question = NULL,
  verbose = TRUE,
  grid.verbose = verbose,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_SVM))
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
  mod.name <- "SVM"

  # Dependencies ----
  dependency_check("e1071")

  # Arguments ----
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s_SVM))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  gridsearch.type <- match.arg(gridsearch.type)

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
  x <- data.matrix(dt$x)
  y <- dt$y
  x.test <- dt$x.test
  if (!is.null(x.test)) x.test <- data.matrix(x.test)
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .class.weights <- if (is.null(class.weights) & ifw) dt$class.weights else
    class.weights
  x0 <- if (upsample | downsample) dt$x0 else x
  y0 <- if (upsample | downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(gamma)) gamma <- 1 / NCOL(x)
  if (type == "Classification") nlevels <- length(levels(y))

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

  gs <- NULL
  if (kernel == "linear") {
    if (gridCheck(cost)) {
      gs <- gridSearchLearn(
        x0,
        y0,
        mod.name,
        resample.params = grid.resample.params,
        grid.params = list(cost = cost),
        fixed.params = list(
          kernel = "linear",
          class.weights = class.weights,
          ifw = ifw,
          ifw.type = ifw.type,
          upsample = upsample,
          downsample = downsample,
          resample.seed = resample.seed
        ),
        search.type = gridsearch.type,
        randomized.p = gridsearch.randomized.p,
        metric = metric,
        maximize = maximize,
        verbose = verbose,
        grid.verbose = grid.verbose,
        n.cores = n.cores
      )
      cost <- gs$best.tune$cost
    }
  } else if (kernel == "polynomial") {
    if (gridCheck(cost, degree, gamma, coef0)) {
      gs <- gridSearchLearn(
        x,
        y,
        mod.name,
        resample.params = grid.resample.params,
        grid.params = list(
          cost = cost,
          degree = degree,
          gamma = gamma,
          coef0 = coef0
        ),
        fixed.params = list(
          kernel = "polynomial",
          class.weights = class.weights,
          ifw = ifw,
          ifw.type = ifw.type,
          upsample = upsample,
          downsample = downsample,
          resample.seed = resample.seed
        ),
        metric = metric,
        maximize = maximize,
        verbose = verbose,
        grid.verbose = grid.verbose,
        n.cores = n.cores
      )
      cost <- gs$best.tune$cost
      degree <- gs$best.tune$degree
      gamma <- gs$best.tune$gamma
      coef0 <- gs$best.tune$coef0
    }
  } else if (kernel == "sigmoid") {
    if (gridCheck(cost, gamma, coef0)) {
      gs <- gridSearchLearn(
        x,
        y,
        mod.name,
        resample.params = grid.resample.params,
        grid.params = list(cost = cost, gamma = gamma, coef0 = coef0),
        fixed.params = list(
          kernel = "sigmoid",
          class.weights = class.weights,
          ifw = ifw,
          ifw.type = ifw.type,
          upsample = upsample,
          downsample = downsample,
          resample.seed = resample.seed
        ),
        metric = metric,
        maximize = maximize,
        verbose = verbose,
        grid.verbose = grid.verbose,
        n.cores = n.cores
      )
      cost <- gs$best.tune$cost
      gamma <- gs$best.tune$gamma
      coef0 <- gs$best.tune$coef0
    }
  } else {
    if (gridCheck(cost, gamma)) {
      gs <- gridSearchLearn(
        x,
        y,
        mod.name,
        resample.params = grid.resample.params,
        grid.params = list(cost = cost, gamma = gamma),
        fixed.params = list(
          kernel = "radial",
          class.weights = class.weights,
          ifw = ifw,
          ifw.type = ifw.type,
          upsample = upsample,
          downsample = downsample,
          resample.seed = resample.seed
        ),
        metric = metric,
        maximize = maximize,
        verbose = verbose,
        grid.verbose = grid.verbose,
        n.cores = n.cores
      )
      cost <- gs$best.tune$cost
      gamma <- gs$best.tune$gamma
    }
  }

  if (kernel == "linear") {
    parameters <- list(
      kernel = kernel,
      cost = cost,
      class.weights = .class.weights
    )
  } else if (kernel == "polynomial") {
    parameters <- list(
      kernel = kernel,
      cost = cost,
      gamma = gamma,
      coef0 = coef0,
      degree = degree,
      class.weights = .class.weights
    )
  } else if (kernel == "sigmoid") {
    parameters <- list(
      kernel = kernel,
      cost = cost,
      gamma = gamma,
      coef0 = coef0,
      class.weights = .class.weights
    )
  } else {
    parameters <- list(
      kernel = kernel,
      cost = cost,
      gamma = gamma,
      class.weights = .class.weights
    )
  }

  # e1071::svm ----
  if (verbose)
    msg2("Training SVM", type, "with", kernel, "kernel...", newline.pre = TRUE)
  mod <- e1071::svm(
    x = x,
    y = y,
    kernel = kernel,
    degree = degree,
    cost = cost,
    gamma = gamma,
    coef0 = coef0,
    probability = probability,
    class.weights = .class.weights,
    ...
  )

  # Fitted ----
  fitted.prob <- predict(mod, x, probability = TRUE)
  fitted.prob <- attr(fitted.prob, "probabilities")[, levels(y)[1]]
  fitted <- predict(mod)

  if (type == "Regression") fitted <- as.numeric(fitted)
  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted.prob <- predict(mod, x.test, probability = TRUE)
    predicted.prob <- attr(predicted.prob, "probabilities")[, levels(y)[1]]
    predicted <- predict(mod, x.test)
    if (!is.null(y.test)) {
      if (type == "Regression") predicted <- as.numeric(predicted)
      error.test <- mod_error(y.test, predicted, predicted.prob)
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
    parameters = parameters,
    call = call,
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
} # rtemis::s_SVM
