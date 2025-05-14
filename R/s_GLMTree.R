# s_GLMTree.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Generalized Linear Model Tree \[R\]
#'
#' Train a GLMTree for regression or classification using
#' `partykit::glmtree`
#'
#' @inheritParams s_CART
## @param offset Numeric vector of a priori known offsets
#' @param ... Additional arguments passed to `partykit::mob_control`
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @export

s_GLMTree <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  weights = NULL,
  #   offset = NULL,
  alpha = 0.05,
  bonferroni = TRUE,
  minsize = NULL,
  maxdepth = Inf,
  #  mtry = Inf,
  #  trim = 0.1,
  #  breakties = FALSE,
  #  parm = NULL,
  #  dfsplit = TRUE,
  prune = NULL,
  #  restart = TRUE,
  #  verbose = FALSE,
  #  caseweights = TRUE,
  #  ytype = "vector",
  #  xtype = "matrix",
  #  terminal = "object",
  #  inner = terminal,
  #  model = TRUE,
  #  numsplit = "left",
  #  catsplit = "binary",
  #  vcov = "opg",
  #  ordinal = "chisq",
  #  nrep = 10000,
  minsplit = minsize,
  minbucket = minsize,
  #  applyfun = NULL,
  epsilon = 1e-8,
  maxit = 25,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  na.action = na.exclude,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  metric = NULL,
  maximize = NULL,
  n.cores = rtCores,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  grid.verbose = verbose,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_GLMTree))
    return(invisible(9))
  }
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "GLMTree"

  # Dependencies ----
  dependency_check("partykit")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_GLMTree))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name, "/")
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
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  # x0, y0 are passed to gridSearchLearn
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  checkType(type, c("Classification", "Regression"), mod.name)
  if (type == "Classification") {
    if (length(levels(y)) != 2) {
      stop("Can only perform binary classification")
    }
  }
  family <- if (type == "Regression") gaussian() else binomial()
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

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

  # Formula ----
  features <- paste(xnames, collapse = " + ")
  .formula <- as.formula(paste0(y.name, " ~ ", features))
  df.train <- cbind(
    x,
    y = if (type == "Classification") reverseLevels(y) else y
  )

  # Grid Search ----
  if (gridCheck(alpha, maxdepth, minsize, minsplit, minbucket)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        alpha = alpha,
        maxdepth = maxdepth,
        minsize = minsize,
        minsplit = minsplit,
        minbucket = minbucket
      ),
      fixed.params = list(
        bonferroni = bonferroni,
        prune = prune,
        na.action = na.action,
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        downsample = downsample,
        resample.seed = resample.seed
      ),
      search.type = gridsearch.type,
      randomized.p = gridsearch.randomized.p,
      weights = weights,
      metric = metric,
      maximize = maximize,
      verbose = grid.verbose,
      n.cores = n.cores
    )
    alpha <- gs$best.tune$alpha
    maxdepth <- gs$best.tune$maxdepth
    minsize <- gs$best.tune$minsize
    minsplit <- gs$best.tune$minsplit
    minbucket <- gs$best.tune$minbucket
  } else {
    gs <- NULL
  }
  parameters <- list(
    alpha = alpha,
    maxdepth = maxdepth,
    minsize = minsize,
    minsplit = minsplit,
    minbucket = minbucket,
    bonferroni = bonferroni,
    prune = prune,
    na.action = na.action
  )

  # glmtree ----
  if (verbose) msg2("Training GLMTree...", newline.pre = TRUE)
  mod <- partykit::glmtree(
    formula = .formula,
    data = df.train,
    family = family,
    na.action = na.action,
    weights = .weights,
    alpha = alpha,
    maxdepth = maxdepth,
    minsize = minsize,
    minsplit = minsplit,
    minbucket = minbucket,
    epsilon = epsilon,
    maxit = maxit,
    prune = prune,
    ...
  )

  # Fitted ----
  fitted.prob <- NULL
  fitted <- predict(mod, x, type = "response")
  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
    levels(fitted) <- levels(y)
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test, type = "response")
    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
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
    # varimp = varimp,
    question = question,
    extra = NULL
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
} # rtemis::s_GLMTree
