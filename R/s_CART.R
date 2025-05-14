# s_CART.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Classification and Regression Trees \[C, R, S\]
#'
#' Train a CART for regression or classification using `rpart`
#'
#' \[gS\] indicates grid search will be performed automatically if more than one
#' value is passed
#'
#' @inheritParams s_GLM
#' @param method Character: "auto", "anova", "poisson", "class" or "exp".
#' @param cp \[gS\] Float: Complexity threshold for allowing a split.
#' @param maxdepth \[gS\] Integer: Maximum depth of tree.
#' @param maxcompete Integer: The number of competitor splits saved in the
#' output
#' @param usesurrogate See `rpart::rpart.control`
#' @param xval Integer: Number of cross-validations
#' @param surrogatestyle See `rpart::rpart.control`
#' @param maxsurrogate Integer: The number of surrogate splits retained in the
#' output (See `rpart::rpart.control`).
#' @param minsplit \[gS\] Integer: Minimum number of cases that must belong in a
#' node before considering a split.
#' @param minbucket \[gS\] Integer: Minimum number of cases allowed in a child
#' node.
#' @param prune.cp \[gS\] Numeric: Complexity for cost-complexity pruning after
#' tree is built
#' @param use.prune.rpart.rt (Testing only, do not change)
#' @param return.unpruned Logical: If TRUE and `prune.cp` is set, return
#' unpruned tree under `extra` in `rtMod`.
#' @param grid.resample.params List: Output of [setup.resample] defining
#' grid search parameters.
#' @param gridsearch.type Character: Type of grid search to perform:
#' "exhaustive" or "randomized".
#' @param gridsearch.randomized.p Float (0, 1): If
#' `gridsearch.type = "randomized"`, randomly test this proportion of
#' combinations.
#' @param save.gridrun Logical: If TRUE, save grid search models.
#' @param metric Character: Metric to minimize, or maximize if
#' `maximize = TRUE` during grid search. Default = NULL, which results in
#' "Balanced Accuracy" for Classification,
#' "MSE" for Regression, and "Coherence" for Survival Analysis.
#' @param maximize Logical: If TRUE, `metric` will be maximized if grid
#' search is run.
#' @param parms List of additional parameters for the splitting function.
#' See `rpart::rpart("parms")`
#' @param cost Vector, Float (> 0): One for each variable in the model.
#' See `rpart::rpart("cost")`
#' @param model Logical: If TRUE, keep a copy of the model.
#' @param grid.verbose Logical: Passed to `gridSearchLearn`
#' @param n.cores Integer: Number of cores to use.
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @export

s_CART <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  method = "auto",
  parms = NULL,
  minsplit = 2,
  minbucket = round(minsplit / 3),
  cp = 0.01,
  maxdepth = 20,
  maxcompete = 0,
  maxsurrogate = 0,
  usesurrogate = 2,
  surrogatestyle = 0,
  xval = 0,
  cost = NULL,
  model = TRUE,
  prune.cp = NULL,
  use.prune.rpart.rt = TRUE,
  return.unpruned = FALSE,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  save.gridrun = FALSE,
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
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE)
) {
  # .call <- match.call()
  tree.depth <- getFromNamespace("tree.depth", "rpart")

  # Intro ----
  if (missing(x)) {
    print(args(s_CART))
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
  mod.name <- "CART"

  # Dependencies ----
  dependency_check("rpart")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_CART))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name, "/")
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
  if (type != "Survival") df.train <- data.frame(y = y, x)
  if (method == "auto") {
    if (type == "Regression") {
      method <- "anova"
      if (is.null(metric)) metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    } else if (type == "Classification") {
      method <- "class"
      if (is.null(metric)) metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Survival") {
      method <- "exp"
      if (is.null(metric)) metric <- "Concordance"
      if (is.null(maximize)) maximize <- TRUE
    }
  }
  if (is.null(cost)) cost <- rep(1, NCOL(x))
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
    } else if (type == "Regression") {
      metric <- "MSE"
    } else {
      metric <- "Concordance"
    }
  }
  if (type == "Classification") n.classes <- length(levels(y))

  # Formula ----
  features <- paste(xnames, collapse = " + ")
  .formula <- as.formula(paste0(y.name, " ~ ", features))

  # Grid Search ----
  if (gridCheck(maxdepth, minsplit, minbucket, cp, prune.cp)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        maxdepth = maxdepth,
        minsplit = minsplit,
        minbucket = minbucket,
        cp = cp,
        prune.cp = prune.cp
      ),
      fixed.params = list(
        method = method,
        model = model,
        maxcompete = maxcompete,
        maxsurrogate = maxsurrogate,
        usesurrogate = usesurrogate,
        surrogatestyle = surrogatestyle,
        xval = xval,
        cost = cost,
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
      save.mod = save.gridrun,
      verbose = grid.verbose,
      n.cores = n.cores
    )
    maxdepth <- gs$best.tune$maxdepth
    minsplit <- gs$best.tune$minsplit
    minbucket <- gs$best.tune$minbucket
    cp <- gs$best.tune$cp
    prune.cp <- gs$best.tune$prune.cp
  } else {
    gs <- NULL
  }
  parameters <- list(
    prune.cp = prune.cp,
    method = method,
    model = model,
    minsplit = minsplit,
    minbucket = minbucket,
    cp = cp,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate,
    usesurrogate = usesurrogate,
    surrogatestyle = surrogatestyle,
    maxdepth = maxdepth,
    xval = xval,
    cost = cost
  )

  control <- list(
    minsplit = minsplit,
    minbucket = minbucket,
    cp = cp,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate,
    usesurrogate = usesurrogate,
    surrogatestyle = surrogatestyle,
    maxdepth = maxdepth,
    xval = xval
  )

  # rpart ----
  if (verbose) msg2("Training CART...", newline.pre = TRUE)
  mod <- rpart::rpart(
    formula = .formula,
    data = df.train,
    weights = .weights,
    method = method,
    model = model,
    control = control,
    cost = cost,
    parms = parms
  )

  # Cost-Complexity Pruning ----
  if (!is.null(prune.cp)) {
    if (return.unpruned) mod.unpruned <- mod
    if (use.prune.rpart.rt) {
      mod <- prune.rpart.rt(mod, cp = prune.cp)
    } else {
      mod <- rpart::prune(mod, cp = prune.cp)
    }
  }

  # Fitted ----
  fitted.prob <- NULL
  if (type == "Regression" || type == "Survival") {
    fitted <- predict(mod, x, type = "vector")
  } else if (type == "Classification") {
    if (n.classes == 2) {
      fitted.prob <- predict(mod, x, type = "prob")[, rtenv$binclasspos]
    } else {
      fitted.prob <- predict(mod, x, type = "prob")
    }
    fitted <- predict(mod, x, type = "class")
  }

  attr(fitted, "names") <- NULL
  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression" || type == "Survival") {
      predicted <- predict(mod, x.test, type = "vector")
      predicted.prob <- NULL
    } else if (type == "Classification") {
      predicted.prob <- predict(mod, x.test, type = "prob")[, rtenv$binclasspos]
      predicted <- predict(mod, x.test, type = "class")
    }

    attr(predicted, "names") <- NULL
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- predicted.prob <- error.test <- NULL
  }

  # Outro ----
  varimp <- rep(NA, NCOL(x))
  varimp.cart <- if (!is.null(mod$variable.importance))
    as.matrix(mod$variable.importance) else NULL
  varimp.index <- match(rownames(varimp.cart), colnames(x))
  varimp[varimp.index] <- varimp.cart
  varimp[is.na(varimp)] <- 0
  names(varimp) <- colnames(x)
  extra <- list(
    imetrics = list(
      n.nodes = NROW(mod$frame),
      depth = max(tree.depth(as.numeric(rownames(mod$frame))))
    )
  )
  if (!is.null(prune.cp) && return.unpruned) extra$mod.unpruned <- mod.unpruned
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    gridsearch = gs,
    parameters = parameters,
    # call = .call,
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
    varimp = varimp,
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
} # rtemis::s_CART
