# s_XGBoost.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' XGBoost Classification and Regression (C, R)
#'
#' Tune hyperparameters using grid search and resampling,
#' train a final model, and validate it
#'
#' \[gS\]: indicates parameter will be autotuned by grid search if multiple
#' values are passed. Learn more about XGBoost's parameters here:
#' http://xgboost.readthedocs.io/en/latest/parameter.html
#'
#' @inheritParams s_CART
#' @param booster Character: "gbtree", "gblinear": Booster to use.
#' @param nrounds Integer: Maximum number of rounds to run. Can be set to a high number
#' as early stopping will limit nrounds by monitoring inner CV error
#' @param force.nrounds Integer: Number of rounds to run if not estimating optimal number by CV
#' @param lambda \[gS\] L2 regularization on weights
# @param lambda_bias \[gS\] for *linear* booster: L2 regularization on bias
#' @param alpha \[gS\] L1 regularization on weights
#' @param eta \[gS\] Numeric (0, 1): Learning rate.
#' @param gamma \[gS\] Numeric: Minimum loss reduction required to make further partition
#' @param max_depth \[gS\] Integer: Maximum tree depth.
#' @param min_child_weight \[gS\] Numeric: Minimum sum of instance weight needed in a child.
#' @param max_delta_step \[gS\] Numeric: Maximum delta step we allow each leaf output to
#' be. O means no constraint. 1-10 may help control the update, especially with
#' imbalanced outcomes.
#' @param subsample \[gS\] Numeric: subsample ratio of the training instance
#' @param colsample_bytree \[gS\] Numeric: subsample ratio of columns when constructing each tree
#' @param colsample_bylevel \[gS\] Numeric
#' @param tree_method \[gS\] XGBoost tree construction algorithm
#' @param sketch_eps \[gS\] Numeric (0, 1):
#' @param num_parallel_tree Integer: N of trees to grow in parallel: Results in Random Forest -like algorithm.
#'  (Default = 1; i.e. regular boosting)
#' @param base_score Numeric: The mean outcome response.
#' @param objective (Default = NULL)
#' @param sample_type Character: Type of sampling algorithm for `dart` booster
#' "uniform": dropped trees are selected uniformly.
#' "weighted": dropped trees are selected in proportion to weight.
#' @param normalize_type Character.
#' @param rate_drop \[gS\] Numeric: Dropout rate for `dart` booster.
#' @param one_drop \[gS\] Integer {0, 1}: When this flag is enabled, at least one tree
#' is always dropped during the dropout.
#' @param skip_drop \[gS\] Numeric \[0, 1\]: Probability of skipping the dropout
#' procedure during a boosting iteration. If a dropout is skipped, new trees are added
#' in the same manner as gbtree. Non-zero `skip_drop` has higher priority than
#' `rate_drop` or `one_drop`.
#' @param obj Function: Custom objective function. See `?xgboost::xgboost`
#' @param feval Function: Custom evaluation function. See `?xgboost::xgboost`
#' @param xgb.verbose Integer: Verbose level for XGB learners used for tuning.
#' @param print_every_n Integer: Print evaluation metrics every this many iterations
#' @param early_stopping_rounds Integer: Training on resamples of `x.train` (tuning) will stop if performance
#'   does not improve for this many rounds
#' @param missing String or Numeric: Which values to consider as missing.
#' @param importance Logical: If TRUE, calculate variable importance.
#' @param trace Integer: If > 0, print parameter values to console.
#' @param nthread Integer: Number of threads for xgboost using OpenMP. Only parallelize resamples
#' using `n.cores` or the xgboost execution using this setting. At the moment of
#' writing, parallelization via this parameter causes a linear booster to fail most of
#' the times. Therefore, default is rtCores for 'gbtree', 1 for 'gblinear'
#' @param .gs Internal use only
#' @param ... Additional arguments passed to `xgboost::xgb.train`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_XGBoost <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  booster = c("gbtree", "gblinear", "dart"),
  missing = NA,
  nrounds = 1000L,
  force.nrounds = NULL,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  obj = NULL,
  feval = NULL,
  xgb.verbose = NULL,
  print_every_n = 100L,
  early_stopping_rounds = 50L,
  eta = .01,
  gamma = 0,
  max_depth = 2,
  min_child_weight = 5,
  max_delta_step = 0,
  subsample = .75,
  colsample_bytree = 1,
  colsample_bylevel = 1,
  lambda = 0,
  #   lambda_bias = 0,
  alpha = 0,
  tree_method = "auto",
  sketch_eps = .03,
  num_parallel_tree = 1,
  base_score = NULL,
  objective = NULL,
  sample_type = "uniform",
  normalize_type = "forest",
  rate_drop = 0, # dart
  one_drop = 0,
  skip_drop = 0,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  metric = NULL,
  maximize = NULL,
  importance = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  grid.verbose = FALSE,
  trace = 0,
  save.gridrun = FALSE,
  n.cores = 1,
  nthread = rtCores,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  .gs = FALSE,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_XGBoost))
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
  mod.name <- "XGBoost"

  # Dependencies ----
  dependency_check("xgboost")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  #   if (n.trees > max.trees) {
  #     if (verbose) msg2("n.trees specified is greater than max.trees, setting n.trees to", max.trees)
  #     n.trees <- max.trees
  #   }
  booster <- match.arg(booster)
  if (is.null(importance)) {
    importance <- booster != "gblinear"
  }

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
  if (any(sapply(x, is.factor))) {
    x <- preprocess(x, oneHot = TRUE)
    if (!is.null(x.test)) x.test <- preprocess(x.test, oneHot = TRUE)
  }
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (type == "Classification") y.num <- as.numeric(y) - 1
  nclass <- ifelse(type == "Classification", length(levels(y)), 0)
  if (is.null(objective)) {
    if (type == "Regression") {
      objective <- "reg:squarederror"
    } else {
      objective <- ifelse(nclass == 2, "binary:logistic", "multi:softmax")
    }
  }
  if (type == "Regression") {
    if (is.null(base_score)) base_score <- mean(y)
    xg.dat.train <- xgboost::xgb.DMatrix(
      data = as.matrix(x),
      label = y,
      missing = missing
    )
  } else {
    if (is.null(base_score)) base_score <- mean(as.numeric(y.num))
    xg.dat.train <- xgboost::xgb.DMatrix(
      data = as.matrix(x),
      label = y.num,
      weight = .weights,
      missing = missing
    )
  }

  if (!is.null(x.test)) {
    xg.dat.test <- xgboost::xgb.DMatrix(
      data = as.matrix(x.test),
      label = if (type == "Classification") as.numeric(y.test) - 1 else y.test,
      missing = missing
    )
  }

  if (verbose)
    msg20("Training XGBoost (", booster, " booster)...", newline.pre = TRUE)

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

  if (booster != "gblinear") {
    gc <- gridCheck(
      eta,
      gamma,
      max_depth,
      subsample,
      colsample_bytree,
      colsample_bylevel,
      lambda
    )
  } else {
    gc <- gridCheck(eta, lambda)
  }

  if (!.gs && (gc || is.null(force.nrounds))) {
    grid.params <- if (booster == "gblinear") {
      list(
        eta = eta,
        lambda = lambda
      )
    } else {
      list(
        eta = eta,
        gamma = gamma,
        max_depth = max_depth,
        min_child_weight = min_child_weight,
        max_delta_step = max_delta_step,
        subsample = subsample,
        colsample_bytree = colsample_bytree,
        colsample_bylevel = colsample_bylevel,
        lambda = lambda,
        alpha = alpha
      )
    }
    if (booster == "dart") {
      grid.params <- c(
        grid.params,
        list(
          rate_drop = rate_drop,
          one_drop = one_drop,
          skip_drop = skip_drop
        )
      )
    }
    gs <- gridSearchLearn(
      x = x0,
      y = y0,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = grid.params,
      fixed.params = list(
        nrounds = nrounds,
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        resample.seed = resample.seed,
        sample_type = sample_type,
        normalize_type = normalize_type,
        .gs = TRUE
      ),
      search.type = gridsearch.type,
      weights = weights,
      metric = metric,
      maximize = maximize,
      save.mod = save.gridrun,
      verbose = verbose,
      grid.verbose = grid.verbose,
      n.cores = n.cores
    )

    nrounds <- gs$best.tune$nrounds
    eta <- gs$best.tune$eta
    lambda <- gs$best.tune$lambda

    if (booster %in% c("gbtree", "dart")) {
      gamma <- gs$best.tune$gamma
      max_depth <- gs$best.tune$max_depth
      min_child_weight <- gs$best.tune$min_child_weight
      max_delta_step <- gs$best.tune$max_delta_step
      subsample <- gs$best.tune$subsample
      colsample_bytree <- gs$best.tune$colsample_bytree
      colsample_bylevel <- gs$best.tune$colsample_bylevel
      alpha <- gs$best.tune$alpha
    }

    if (booster == "dart") {
      rate_drop <- gs$best.tune$rate_drop
      one_drop <- gs$best.tune$one_drop
      skip_drop <- gs$best.tune$skip_drop
    }

    # Now ready to train final full model
    .gs <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.nrounds)) nrounds <- force.nrounds
  parameters <- list(
    booster = booster,
    eta = eta,
    gamma = gamma,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    max_delta_step = max_delta_step,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    colsample_bylevel = colsample_bylevel,
    lambda = lambda,
    # lambda_bias = lambda_bias,
    alpha = alpha,
    tree_method = tree_method,
    sketch_eps = sketch_eps,
    num_parallel_tree = num_parallel_tree,
    objective = objective,
    base_score = base_score
  )

  if (booster == "dart") {
    parameters$rate_drop <- rate_drop
    parameters$one_drop <- one_drop
    parameters$skip_drop <- skip_drop
  }

  if (objective == "multi:softmax") parameters$num_class <- nclass
  #   if (verbose) {
  #       # => add params
  #     parameterSummary(booster, eta, gamma, max_depth, min_child_weight,
  #                      newline.pre = TRUE)
  #   }

  # XGBoost ----
  if (verbose) msg2("Training XGBoost with", nrounds, "rounds...")
  watchlist <- if (.gs) {
    list(train = xg.dat.train, valid = xg.dat.test)
  } else {
    NULL
  }
  if (trace > 0) printls(parameters)
  mod <- xgboost::xgb.train(
    parameters,
    data = xg.dat.train,
    nrounds = nrounds,
    watchlist = watchlist,
    obj = obj,
    feval = feval,
    verbose = verbose,
    print_every_n = print_every_n,
    early_stopping_rounds = if (.gs) early_stopping_rounds else NULL,
    nthread = nthread,
    ...
  )

  # Fitted ----
  fitted <- predict(mod, xg.dat.train)
  fitted.prob <- NULL
  if (type == "Classification") {
    if (nclass == 2) {
      fitted.prob <- 1 - fitted
      fitted <- factor(
        ifelse(fitted.prob >= .5, 1, 0),
        levels = c(1, 0),
        labels = levels(y)
      )
    } else {
      fitted <- factor(fitted, levels = seq(nclass) - 1, labels = levels(y))
    }
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, xg.dat.test)
    if (type == "Classification") {
      if (nclass == 2) {
        predicted.prob <- 1 - predicted
        predicted <- factor(
          ifelse(predicted.prob >= .5, 1, 0),
          levels = c(1, 0),
          labels = levels(y)
        )
      } else {
        predicted <- factor(
          predicted,
          levels = seq(nclass) - 1,
          labels = levels(y)
        )
      }
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Relative Influence / Variable Importance ----
  varimp <- NULL
  # This may take a while
  if (importance) {
    if (verbose) msg2("Estimating variable importance...")
    .xgbvarimp <- xgboost::xgb.importance(
      model = mod,
      feature_names = colnames(x)
    )
    varimp <- .xgbvarimp$Gain
    names(varimp) <- .xgbvarimp$Feature
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
    varimp = varimp,
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
} # rtemis::s_XGBoost
