# s_XRF.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' XGBoost Random Forest Classification and Regression (C, R)
#'
#' Tune hyperparameters using grid search and resampling,
#' train a final model, and validate it
#'
#' \[gS\]: indicates parameter will be autotuned by grid search if multiple
#' values are passed. Learn more about XGBoost's parameters here:
#' http://xgboost.readthedocs.io/en/latest/parameter.html
#'
#' @inheritParams s_GLM
#' @inheritParams s_XGBoost
#' @param booster Character: Booster to use. Options: "gbtree", "gblinear"
#' @param num_parallel_tree Integer: Number of trees to grow
#' @param base_score Numeric: The mean outcome response (Defaults to mean)
#' @param objective (Default = NULL)
#' @param sample_type Character. Default = "uniform"
#' @param normalize_type Character. Default = "forest"
#' @param obj Function: Custom objective function. See `?xgboost::xgboost`
#' @param feval Function: Custom evaluation function. See `?xgboost::xgboost`
#' @param xgb.verbose Integer: Verbose level for XGB learners used for tuning.
#' @param print_every_n Integer: Print evaluation metrics every this many iterations
#' @param early_stopping_rounds Integer: Training on resamples of `x.train` (tuning) will stop if performance
#'   does not improve for this many rounds
#' @param missing String or Numeric: Which values to consider as missing. Default = NA
#' @param nthread Integer: Number of threads for xgboost using OpenMP. Only parallelize resamples
#' using `n.cores` or the xgboost execution using this setting. At the moment of writing, parallelization via this
#' parameter causes a linear booster to fail most of the times. Therefore, default is rtCores
#' for 'gbtree', 1 for 'gblinear'
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_XRF <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  num_parallel_tree = 1000,
  booster = c("gbtree", "gblinear", "dart"),
  missing = NA,
  nrounds = 1,
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
  eta = 1,
  gamma = 0,
  max_depth = 12,
  min_child_weight = 1,
  max_delta_step = 0,
  subsample = .75,
  colsample_bytree = 1,
  colsample_bylevel = 1,
  lambda = 0,
  alpha = 0,
  tree_method = "auto",
  sketch_eps = .03,
  base_score = NULL,
  objective = NULL,
  sample_type = "uniform",
  normalize_type = "forest",
  rate_drop = 0,
  one_drop = 0,
  skip_drop = 0,
  .gs = FALSE,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  metric = NULL,
  maximize = NULL,
  importance = TRUE,
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
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_XRF))
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
  mod.name <- "XRF"

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
  booster <- match.arg(booster)

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
    if (verbose) msg2("Preprocessing training data...")
    x <- preprocess(x, oneHot = TRUE)
    if (!is.null(x.test)) {
      if (verbose) msg2("Preprocessing testing data...")
      x.test <- preprocess(x.test, oneHot = TRUE)
    }
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

  gc <- gridCheck(
    eta,
    gamma,
    max_depth,
    subsample,
    colsample_bytree,
    colsample_bylevel,
    lambda
  )
  if (!.gs && gc) {
    grid.params <- list(
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
    gamma <- gs$best.tune$gamma
    max_depth <- gs$best.tune$max_depth
    min_child_weight <- gs$best.tune$min_child_weight
    max_delta_step <- gs$best.tune$max_delta_step
    subsample <- gs$best.tune$subsample
    colsample_bytree <- gs$best.tune$colsample_bytree
    colsample_bylevel <- gs$best.tune$colsample_bylevel
    lambda <- gs$best.tune$lambda
    alpha <- gs$best.tune$alpha
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
  if (verbose)
    msg2("Training XGBoost Random Forest with", num_parallel_tree, "trees...")
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
} # rtemis::s_XRF
