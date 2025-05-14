# s_LightGBM.R
# ::rtemis::
# 2023 E.D. Gennatas rtemis.org
# https://lightgbm.readthedocs.io/en/latest/R/index.html
# LightGBM parameters:
# https://lightgbm.readthedocs.io/en/latest/Parameters.html
# https://lightgbm.readthedocs.io/en/latest/R/articles/basic_walkthrough.html
# For custom loss functions, e.g. focal loss:
# https://maxhalford.github.io/blog/lightgbm-focal-loss/

#' LightGBM Classification and Regression (C, R)
#'
#' Tune hyperparameters using grid search and resampling,
#' train a final model, and validate it
#'
#' \[gS\]: indicates parameter will be autotuned by grid search if multiple
#' values are passed.
#' LightGBM trains trees leaf-wise (best-first) rather than depth-wise.
#' For categorical variables, convert to integer and indicate to lgb they are categorical,
#' so that they are not treated as numeric.
#'
#' @inheritParams s_CART
#' @param boosting Character: \[gS\] "gbdt", "rf", "dart", "goss"
#' @param max_nrounds Integer: Maximum number of rounds to run. Can be set to a high number
#' as early stopping will limit nrounds by monitoring inner CV error
#' @param force_nrounds Integer: Number of rounds to run if not estimating optimal number by CV
#' @param early_stopping_rounds Integer: Training on resamples of `x` (tuning) will
#' stop if performance does not improve for this many rounds
#' @param nrounds_default Integer: Default number of rounds to run if
#' cross-validation fails - likely will never be used
#' @param num_leaves Integer: \[gS\] Maximum tree leaves for base learners.
#' @param max_depth Integer: \[gS\] Maximum tree depth for base learners, <=0 means no limit.
#' @param learning_rate Numeric: \[gS\] Boosting learning rate
#' @param feature_fraction Numeric (0, 1): \[gS\] Fraction of features to consider at each iteration
#' (i.e. tree)
#' @param subsample Numeric: \[gS\] Subsample ratio of the training set.
#' @param subsample_freq Integer: Subsample every this many iterations
#' @param lambda_l1 Numeric: \[gS\] L1 regularization term
#' @param lambda_l2 Numeric: \[gS\] L2 regularization term
#' @param max_cat_threshold Integer: Max number of splits to consider for categorical
#' variable
#' @param min_data_per_group Integer: Minimum number of observations per categorical
#' group
#' @param linear_tree Logical: \[gS\] If `TRUE`, use linear trees
#' @param tree_learner Character: \[gS\] "serial", "feature", "data", "voting"
#' @param importance Logical: If `TRUE`, calculate variable importance
#' @param objective (Default = NULL)
#' @param lightgbm_verbose Integer: Passed to `lightgbm::train`, `< 0`: Fatal,
#' `0`: Error (Warning), `1`: Info, `> 1`: Debug
#' @param save.gridrun Logical: If `TRUE`, save all grid search models
#' @param n_threads Integer: Number of threads for lightgbm using OpenMP. Only
#' parallelize resamples using `n.cores` or the lightgbm execution using this setting.
#' @param force_col_wise Logical: If `TRUE`, force column-wise histogram building
#' (See https://lightgbm.readthedocs.io/en/latest/Parameters.html)
#' @param force_row_wise Logical: If `TRUE`, force row-wise histogram building
#' (See https://lightgbm.readthedocs.io/en/latest/Parameters.html)
#' @param .gs (Internal use only)
#' @param ... Extra arguments appended to `lgb.train`'s `params`.
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export
#' @examples
#' \dontrun{
#' x <- rnormmat(500, 10)
#' y <- x[, 3] + .5 * x[, 5]^2 + rnorm(500)
#' dat <- data.frame(x, y)
#' mod <- s_LightGBM(dat)
#' }

s_LightGBM <- function(
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
  boosting = "gbdt",
  objective = NULL,
  # Hyperparameters
  max_nrounds = 1000L,
  force_nrounds = NULL,
  early_stopping_rounds = 10L,
  nrounds_default = 100L,
  num_leaves = 32L,
  max_depth = -1L,
  learning_rate = .01,
  feature_fraction = 1.0,
  subsample = .8,
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  tree_learner = "serial",
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
  lightgbm_verbose = -1,
  save.gridrun = FALSE,
  n.cores = 1,
  n_threads = 0,
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_LightGBM))
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
  mod.name <- if (boosting == "rf") "LightRF" else "LightGBM"

  # Dependencies ----
  dependency_check("lightgbm")

  # Arguments ----
  nrounds <- max_nrounds
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
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
    factor_index <- xnames[which(sapply(x, is.factor))]
    x <- preprocess(x, factor2integer = TRUE, factor2integer_startat0 = TRUE)
    if (!is.null(x.test)) {
      x.test <- preprocess(
        x.test,
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      )
    }
  } else {
    factor_index <- NULL
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
  if (type == "Classification") y.num <- as.integer(y) - 1
  nclass <- ifelse(type == "Classification", length(levels(y)), 0)
  if (is.null(objective)) {
    if (type == "Regression") {
      objective <- "regression"
    } else {
      objective <- ifelse(nclass == 2, "binary", "multiclass")
    }
  }
  dat.train <- lightgbm::lgb.Dataset(
    data = as.matrix(x),
    categorical_feature = factor_index,
    label = if (type == "Classification") as.integer(y) - 1 else y,
    weight = .weights
  )
  # if (!is.null(factor_index)) {
  #     dat.train$set_categorical_feature(factor_index)
  #     lightgbm::lgb.Dataset.set.categorical(dat.train, factor_index)
  # }

  if (!is.null(x.test)) {
    dat.test <- lightgbm::lgb.Dataset(
      data = as.matrix(x.test),
      categorical_feature = factor_index,
      label = if (type == "Classification") as.integer(y.test) - 1 else y.test
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
    num_leaves,
    max_depth,
    learning_rate,
    feature_fraction,
    subsample,
    lambda_l1,
    lambda_l2
  )

  tuned <- FALSE
  if (!.gs && (gc || (is.null(force_nrounds) && boosting != "rf"))) {
    grid.params <-
      list(
        num_leaves = num_leaves,
        max_depth = max_depth,
        learning_rate = learning_rate,
        feature_fraction = feature_fraction,
        subsample = subsample,
        lambda_l1 = lambda_l1,
        lambda_l2 = lambda_l2
      )
    gs <- gridSearchLearn(
      x = x0,
      y = y0,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = grid.params,
      fixed.params = list(
        boosting = boosting,
        max_nrounds = max_nrounds,
        early_stopping_rounds = early_stopping_rounds,
        objective = objective,
        tree_learner = tree_learner,
        linear_tree = linear_tree,
        subsample_freq = subsample_freq,
        max_cat_threshold = max_cat_threshold,
        min_data_per_group = min_data_per_group,
        ifw = ifw,
        ifw.type = ifw.type,
        upsample = upsample,
        resample.seed = resample.seed,
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
    if (nrounds <= 0) {
      warning(
        "Could not get CV estimate for best N rounds. Defaulting to",
        nrounds_default
      )
      nrounds <- nrounds_default
    }
    num_leaves <- gs$best.tune$num_leaves
    max_depth <- gs$best.tune$max_depth
    learning_rate <- gs$best.tune$learning_rate
    feature_fraction <- gs$best.tune$feature_fraction
    subsample <- gs$best.tune$subsample
    lambda_l1 <- gs$best.tune$lambda_l1
    lambda_l2 <- gs$best.tune$lambda_l2
    tuned <- TRUE

    # Now ready to train final full model
    .gs <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force_nrounds)) nrounds <- force_nrounds

  parameters <- list(
    boosting = boosting,
    objective = objective,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    subsample = subsample,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    num_threads = n_threads,
    force_col_wise = force_col_wise,
    force_row_wise = force_row_wise,
    tree_learner = tree_learner,
    linear_tree = linear_tree,
    subsample_freq = subsample_freq,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group
  )
  extraargs <- list(...)
  if (!is.null(extraargs)) {
    parameters <- c(parameters, extraargs)
  }

  if (type == "Classification" && nclass > 2) {
    parameters$num_class <- nclass
  }

  # LightGBM ----
  if (verbose) {
    if (tuned) {
      msg2(
        "Training",
        mod.name,
        type,
        "with tuned hyperparameters...",
        newline.pre = TRUE
      )
    } else {
      msg20("Training ", mod.name, " ", type, "...", newline.pre = TRUE)
    }
  }

  mod <- lightgbm::lgb.train(
    params = parameters,
    data = dat.train,
    nrounds = nrounds,
    valids = if (.gs) list(train = dat.train, valid = dat.test) else
      list(train = dat.train),
    early_stopping_rounds = if (.gs) early_stopping_rounds else NULL,
    verbose = lightgbm_verbose
  )

  # Fitted ----
  fitted <- predict(mod, as.matrix(x))
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
      fitted.prob <- fitted
      fitted <- factor(
        max.col(fitted),
        levels = seq(nclass),
        labels = levels(y)
      )
    }
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, as.matrix(x.test))
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
          max.col(predicted),
          levels = seq(nclass),
          labels = levels(y)
        )
      }
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Variable Importance ----
  varimp <- NULL
  if (importance) {
    if (verbose) msg2("Estimating", mod.name, "variable importance...")
    .lgbvarimp <- lightgbm::lgb.importance(model = mod, percentage = TRUE)
    varimp <- .lgbvarimp$Gain
    names(varimp) <- .lgbvarimp$Feature
  }

  # Outro ----
  rt <- rtModSet(
    mod = mod,
    extra = list(factor_index = factor_index),
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
} # rtemis::s_LightGBM

#' Random Forest using LightGBM
#'
#' @inheritParams s_LightGBM
#' @param nrounds Integer: Number of trees to grow
#'
#' @author ED Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- rnormmat(500, 10)
#' y <- x[, 3] + .5 * x[, 5]^2 + rnorm(500)
#' dat <- data.frame(x, y)
#' mod <- s_LightRF(dat)
#' }

s_LightRF <- function(
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
  objective = NULL,
  # Hyperparameters
  nrounds = 500L,
  early_stopping_rounds = -1L,
  num_leaves = 4096L,
  max_depth = -1L,
  learning_rate = 1,
  feature_fraction = 1.0,
  subsample = .623,
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  tree_learner = "data_parallel",
  # Grid Search
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  metric = NULL,
  maximize = NULL,
  #
  importance = TRUE,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  grid.verbose = FALSE,
  lightgbm_verbose = -1,
  save.gridrun = FALSE,
  n.cores = 1,
  n_threads = rtCores,
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  .gs = FALSE,
  ...
) {
  s_LightGBM(
    x,
    y = y,
    x.test = x.test,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    weights = weights,
    ifw = ifw,
    ifw.type = ifw.type,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed,
    boosting = "rf",
    objective = objective,
    max_nrounds = nrounds,
    force_nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    tree_learner = tree_learner,
    .gs = .gs,
    grid.resample.params = grid.resample.params,
    gridsearch.type = gridsearch.type,
    metric = metric,
    maximize = maximize,
    importance = importance,
    print.plot = print.plot,
    plot.fitted = plot.fitted,
    plot.predicted = plot.predicted,
    plot.theme = plot.theme,
    question = question,
    verbose = verbose,
    grid.verbose = grid.verbose,
    lightgbm_verbose = lightgbm_verbose,
    save.gridrun = save.gridrun,
    n.cores = n.cores,
    n_threads = n_threads,
    force_col_wise = force_col_wise,
    force_row_wise = force_row_wise,
    outdir = outdir,
    save.mod = save.mod,
    ...
  )
} # rtemis::s_LightRF

predict_LightGBM <- function(
  x,
  newdata,
  classification.output = c("prob", "class"),
  ...
) {
  if (!is.null(x$extra$factor_index)) {
    newdata <- preprocess(
      newdata,
      factor2integer = TRUE,
      factor2integer_startat0 = TRUE
    )
  }
  predicted <- predict(x$mod, as.matrix(newdata))
  if (x$type == "Classification") {
    classification.output <- match.arg(classification.output)
    ylevels <- levels(x$y.train)
    nclass <- length(ylevels)
    if (nclass == 2) {
      if (rtenv$binclasspos == 1) {
        predicted.prob <- 1 - predicted
      } else {
        predicted.prob <- predicted
      }
      if (classification.output == "prob") {
        return(predicted.prob)
      } else {
        return(factor(
          ifelse(predicted.prob >= .5, 1, 0),
          levels = c(1, 0),
          labels = ylevels
        ))
      }
    } else {
      if (classification.output == "prob") {
        return(predicted)
      } else {
        return(factor(
          max.col(predicted),
          levels = seq(nclass),
          labels = ylevels
        ))
      }
    }
  }
  return(predicted)
} # rtemis::predict_LightGBM
