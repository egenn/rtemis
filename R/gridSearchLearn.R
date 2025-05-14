# gridSearchLearn.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org
# new version using future
# => support for Survival

#' \pkg{rtemis} internal: Grid Search for Hyperparameter Tuning of \pkg{rtemis} Learners
#'
#' Train models using a combination of parameter values for model selection
#'
#' Note that weights, if defined (and not NULL), should be passed directly to `gridSearchLearn`
#' as they need to be resampled along `x` and `y`, and should not be passed along with
#' `grid.params`. `ifw` and `ifw.type` should be passed as part of `grid.params`
#' and will be passed on to the learner.
#' Includes a special case for training [s_H2OGBM] or [s_GBM] which requires extracting and averaging n.trees
#' along with params.
#'
#' @param x features - training set. Will be resampled to multiple train-test sets
#' @param y outcome - training set. Will be resampled to multiple train-test sets
#' @param mod Character: \pkg{rtemis} model. See `select_learn()` gives available models
#' @param grid.params List of named elements, each is a vector of values
#' @param fixed.params List of named elements, each is a single value
#' @param search.type Character: "exhaustive" (Default), "randomized". Type of
#' grid search to use. Exhaustive search will try all combinations of
#' parameters. Randomized will try a random sample of size
#' `randomize.p` * `N of total combinations`
#' @param resample.params List: Output of `setup.grid.resample()`
#' @param randomized.p Float (0, 1): For `search.type == "exhaustive"`,
#' sample this portion of combination.
#' @param weights Float, vector: Case weights
#' @param error.aggregate.fn Function: Use this when aggregating error metrics.
#' @param metric Character: Metric to minimize or maximize
#' @param maximize Logical: If TRUE, maximize `metric`
#' @param save.mod Logical: If TRUE, save all trained models.
#' @param verbose Logical: If TRUE, print messages to screen
#' @param call.depth Integer: passed to `msg2`.
#' @param grid.verbose Logical: Passed to `learner`'s `verbose`
#' argument
#' @param n.cores Integer: Number of cores to use
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

gridSearchLearn <- function(
  x,
  y,
  mod,
  grid.params,
  fixed.params = NULL,
  search.type = c("exhaustive", "randomized"),
  resample.params = setup.resample(),
  randomized.p = .05,
  weights = NULL,
  error.aggregate.fn = mean,
  metric = NULL,
  maximize = NULL,
  save.mod = FALSE,
  verbose = TRUE,
  trace = 0,
  call.depth = 1,
  grid.verbose = FALSE,
  n.cores = rtCores
) {
  # Dependencies ----
  dependency_check(c("future", "future.apply"))

  # Intro ----
  start.time <- intro(
    verbose = verbose,
    call.depth = call.depth,
    message = "Running grid search...",
    newline.pre = TRUE
  )
  future::plan(rtPlan)
  # rtemis_init(n.cores, context = "Inner resampling")

  # Arguments ----
  if (missing(x) || missing(y)) {
    print(args(gridSearchLearn))
    stop("Input missing")
  }
  search.type <- match.arg(search.type)
  n.resamples <- resample.params$n.resamples
  n.cores <- as.numeric(n.cores)[1]
  # if (inherits(future::plan(), "sequential") && n.cores > 1) {
  #     warning(
  #         "n.cores set to ", n.cores, ", but future plan is sequential.",
  #         "\n n.cores will be set to 1.",
  #         "\nUse `plan(multisession)` or similar to allow parallel execution."
  #     )
  #     n.cores <- 1
  # }
  # if outer is sequential, tweak inner
  if (inherits(future::plan(), "sequential")) {
    future::plan(list("sequential", future::tweak(rtPlan, workers = n.cores)))
    if (trace > 0) {
      msg2(magenta(
        "Inner resampling: Future plan set to",
        bold(rtPlan),
        "with",
        bold(n.cores),
        "workers"
      ))
    }
  } else {
    future::plan(rtPlan, workers = n.cores)
    if (trace > 0) {
      msg2(magenta(
        "Inner resampling plan set to",
        bold(rtPlan),
        "with",
        bold(n.cores),
        "workers"
      ))
    }
  }

  # Data ----
  x <- as.data.frame(x)

  # Grid ----
  # Filter out any grid.params with "NULL" value: expand.grid will fail otherwise.
  # Since these will not be present in best.tune, assignment to the non-existent named
  # elements will result in NULL,
  # as required. This is needed for functions with parameters that can take NULL value
  grid.params <- Filter(Negate(is.null), grid.params)
  param.grid <- expand.grid(
    c(list(res.id = seq(n.resamples)), grid.params),
    stringsAsFactors = FALSE
  )
  n.param.combs <- NROW(param.grid) / n.resamples
  if (search.type == "randomized") {
    index.per.resample <- sample(
      n.param.combs,
      round(randomized.p * n.param.combs)
    )
    param.grid <- param.grid[rep(index.per.resample, n.resamples), ]
  }
  learner <- select_learn(mod, fn = FALSE)
  res <- resample(
    y = y,
    rtset = resample.params,
    verbosity = as.integer(verbose)
  )

  if (!is.null(resample.params$id.colname)) {
    x <- x[, -(names(x) == resample.params$id.colname)]
  }

  # Grid run ----
  if (verbose) {
    parameterSummary(grid.params, fixed.params, title = "Search parameters")
    msg2(
      hilite(
        "Tuning",
        select_learn(mod, desc = TRUE),
        "by",
        search.type,
        "grid search."
      )
    )
    msg20(
      n.resamples,
      " inner resamples; ",
      NROW(param.grid),
      " models total; running on ",
      singorplu(n.cores, "worker"),
      " (",
      Sys.getenv("R_PLATFORM"),
      ")"
    )
  }

  # learner1 ----
  p <- progressr::progressor(steps = NROW(param.grid))
  learner1 <- function(
    index,
    learner,
    x,
    y,
    res,
    param.grid,
    fixed.params,
    weights,
    verbose,
    save.mod,
    nres
  ) {
    if (verbose) {
      msg2(
        "Running grid line #",
        index,
        " of ",
        NROW(param.grid),
        "...",
        sep = ""
      )
    }
    res1 <- res[[param.grid[index, 1]]]
    x.train1 <- x[res1, , drop = FALSE]
    y.train1 <- y[res1]
    weights1 <- weights[res1]
    x.test1 <- x[-res1, , drop = FALSE]
    y.test1 <- y[-res1]

    args <- c(
      list(
        x = x.train1,
        y = y.train1,
        x.test = x.test1,
        y.test = y.test1,
        weights = weights1,
        print.plot = FALSE,
        verbose = verbose
      ),
      as.list(param.grid[index, 2:NCOL(param.grid), drop = FALSE]),
      fixed.params
    )

    mod1 <- do.call(learner, args)

    out1 <- list(
      id = index,
      res.id = param.grid[index, 1],
      error.train = mod1$error.train,
      error.test = mod1$error.test,
      type = mod1$type,
      params = args
    )

    # '-- Learner-specific collect ----
    if (learner == "s_H2OGBM") {
      out1$est.n.trees <-
        mod1$mod@model$model_summary$number_of_trees
    }
    if (learner == "s_GBM" || learner == "s_GBM3") {
      out1$est.n.trees <- which.min(mod1$mod$valid.error)
      if (length(out1$est.n.trees) == 0) out1$est.n.trees <- NA
    }
    if (learner == "s_LightGBM") {
      out1$best_iter <- mod1$mod$best_iter
      out1$best_score <- mod1$mod$best_score
    }
    if (learner == "s_XGBoost") {
      out1$best_iteration <- mod1$mod$best_iteration
      out1$best_score <- mod1$mod$best_score
    }
    if (learner == "s_GLMNET") {
      out1$lambda.min <- mod1$mod$lambda.min
      out1$lambda.1se <- mod1$mod$lambda.1se
    }
    if (learner %in% c("s_LINAD", "s_LINOA")) {
      out1$est.n.leaves <- mod1$mod$n.leaves
    }
    if (learner == "s_LIHADBoost") {
      out1$sel.n.steps <- mod1$mod$selected.n.steps
    }
    if (save.mod) out1$mod1 <- mod1
    p(sprintf("Inner resample: %i/%i...", index, nres))
    out1
  } # /learner1

  nres <- NROW(param.grid)
  grid_run <- future.apply::future_lapply(
    seq_len(nres),
    learner1,
    learner,
    x,
    y,
    res,
    param.grid,
    fixed.params,
    weights,
    verbose = grid.verbose,
    save.mod = save.mod,
    nres = nres,
    future.seed = TRUE
  )

  # Metric ----
  type <- grid_run[[1]]$type
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
    } else if (type == "Regression") {
      metric <- "MSE"
    } else {
      metric <- "Concordance"
    }
  }

  if (is.null(maximize)) {
    maximize <- metric %in%
      c(
        "Accuracy",
        "Balanced Accuracy",
        "Concordance"
      )
  }
  select.fn <- if (maximize) which.max else which.min
  verb <- if (maximize) "maximize" else "minimize"

  # Aggregate ----
  n.params <- length(grid.params)
  # Average test errors
  if (type %in% c("Regression", "Survival")) {
    error.test.all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r$error.test)
    )))
  } else if (type == "Classification") {
    error.test.all <- as.data.frame(t(sapply(
      grid_run,
      function(r) unlist(r$error.test$Overall)
    )))
  }
  error.test.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
  error.test.mean.by.param.id <- aggregate(
    error.test.all,
    by = list(param.id = error.test.all$param.id),
    error.aggregate.fn
  )[, -1]
  tune.results <- cbind(expand.grid(grid.params), error.test.mean.by.param.id)

  # N of iterations is the one hyperparameter that may be determined
  # automatically, we therefore need to extract it and average it

  # '- GBM, H2OGBM ----
  if (learner %in% c("s_H2OGBM", "s_GBM", "s_GBM3")) {
    est.n.trees.all <- data.frame(
      n.trees = plyr::laply(
        grid_run,
        function(x) x$est.n.trees
      )
    )
    est.n.trees.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.n.trees.by.param.id <- aggregate(
      n.trees ~ param.id,
      est.n.trees.all,
      error.aggregate.fn
    )
    tune.results <- cbind(
      n.trees = round(est.n.trees.by.param.id$n.trees),
      tune.results
    )
    n.params <- n.params + 1
  }

  # '- LightGBM ----
  if (learner == "s_LightGBM") {
    if (verbose) {
      msg2(hilite("Extracting best N of iterations from LightGBM models..."))
    }
    est.nrounds.all <- data.frame(
      nrounds = plyr::laply(grid_run, \(m) m$best_iter)
    )
    est.nrounds.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.nrounds.by.param.id <- aggregate(
      nrounds ~ param.id,
      est.nrounds.all,
      error.aggregate.fn
    )
    tune.results <- cbind(
      nrounds = round(est.nrounds.by.param.id$nrounds),
      tune.results
    )
    n.params <- n.params + 1
  }

  # '- XGBoost ----
  if (learner == "s_XGBoost") {
    if (verbose) {
      msg2(hilite("Extracting best N of iterations from XGBoost models..."))
    }
    est.nrounds.all <- data.frame(
      nrounds = plyr::laply(
        grid_run,
        \(m) m$best_iteration
      )
    )
    est.nrounds.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.nrounds.by.param.id <- aggregate(
      nrounds ~ param.id,
      est.nrounds.all,
      error.aggregate.fn
    )
    tune.results <- cbind(
      nrounds = round(est.nrounds.by.param.id$nrounds),
      tune.results
    )
    n.params <- n.params + 1
  }

  # '- GLMNET ----
  if (learner == "s_GLMNET") {
    if (verbose) {
      msg2(hilite("Extracting best lambda from GLMNET models..."))
    }
    if (is.null(grid.params$lambda)) {
      # if lambda was NULL, cv.glmnet was run and optimal lambda was estimated
      lambda.all <- data.frame(
        lambda = plyr::laply(grid_run, \(x) x[[fixed.params$which.cv.lambda]])
      )
      lambda.all$param.id <- rep(1:n.param.combs, each = n.resamples)
      lambda.by.param.id <- aggregate(
        lambda ~ param.id,
        lambda.all,
        error.aggregate.fn
      )
      tune.results <- cbind(lambda = lambda.by.param.id$lambda, tune.results)
      n.params <- n.params + 1
    }
  }

  # '- LINAD ----
  if (learner %in% c("s_LINAD", "s_LINOA")) {
    if (verbose) {
      msg2(hilite("Extracting best N leaves from LINAD models..."))
    }
    est.n.leaves.all <- data.frame(
      n.leaves = plyr::laply(
        grid_run,
        \(x) ifelse(length(x$est.n.leaves) == 0, 1, x$est.n.leaves)
      )
    )
    est.n.leaves.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.n.leaves.by.param.id <- aggregate(
      n.leaves ~ param.id,
      est.n.leaves.all,
      error.aggregate.fn
    )
    tune.results <- cbind(
      n.leaves = round(est.n.leaves.by.param.id$n.leaves),
      tune.results
    )
    n.params <- n.params + 1
  }

  # '- LIHADBoost ----
  if (learner == "s_LIHADBoost") {
    if (verbose) {
      msg2(hilite("Extracting best N steps from LIHADBoost models..."))
    }
    est.n.steps.all <- data.frame(
      n.steps = plyr::laply(
        grid_run,
        \(x) x$sel.n.steps
      )
    )
    est.n.steps.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.n.steps.by.param.id <- aggregate(
      n.steps ~ param.id,
      est.n.steps.all,
      error.aggregate.fn
    )
    tune.results <- cbind(
      n.steps = round(est.n.steps.by.param.id$n.steps),
      tune.results
    )
    n.params <- n.params + 1
  }

  # Consider explicitly ordering hyperparam values in increasing order,
  # so that in case of tie, lowest value is chosen -
  # if that makes sense, e.g. n.leaves, etc.
  best.tune <- tune.results[
    select.fn(tune.results[[metric]]),
    seq_len(n.params),
    drop = FALSE
  ]
  if (verbose) {
    parameterSummary(
      best.tune,
      title = paste("Best parameters to", verb, metric)
    )
  }

  # Outro ----
  outro(start.time, verbose = verbose)
  gs <- list(
    type = search.type,
    p = randomized.p,
    resample.params = resample.params,
    tune.results = tune.results,
    error.test.all = error.test.all,
    best.tune = best.tune,
    metric = metric,
    maximize = maximize,
    params = list(
      fixed = fixed.params,
      search = grid.params
    ),
    error.aggregate.fn = deparse(substitute(error.aggregate.fn))
  )

  if (save.mod) gs$mods <- grid_run

  class(gs) <- c("gridSearch", "list")
  gs
} # rtemis::gridSearchLearn

#' \pkg{rtemis} internal: Grid check
#'
#' Checks if grid search needs to be performed.
#' All tunable parameters should be passed to this function, individually or as
#' a list. If any argument has more than one assigned values, the function
#' returns TRUE, otherwise FALSE. This can be used to check whether
#' `gridSearchLearn` must be run.
#'
#' The idea is that if you know which parameter values you want to use, you
#' define them directly
#'   e.g. `alpha = 0, lambda = .2`.
#' If you don't know, you enter the set of values to be tested,
#'   e.g. `alpha = c(0, .5, 1), lambda = seq(.1, 1, .1)`.
#' @param ... Parameters; will be converted to a list

gridCheck <- function(...) {
  args <- list(...)
  any(as.logical(lapply(args, function(a) length(a) > 1)))
} # rtemis::gridCheck


#' `print` method for `gridSearch` object
#'
#' @method print gridSearch
#' @param x Object of class `gridSearch` created by `gridSearchLearn`
#' @param ... Unused
#'
#' @export
#' @author E.D. Gennatas

print.gridSearch <- function(x, ...) {
  objcat("gridSearch")
  type <- if (x$type == "exhaustive") {
    "An exhaustive grid search"
  } else {
    paste0("A randomized grid search (p = ", x$p, ")")
  }
  resamples <- if (x$resample.params$resample == "kfold") {
    "independent folds"
  } else if (x$resample.params$resample == "strat.sub") {
    "stratified subsamples"
  } else if (x$resample.params$resample == "bootstraps") {
    "bootstraps"
  } else if (x$resample.params$resample == "strat.boot") {
    "stratified bootstraps"
  }
  cat(
    type,
    " was performed using ",
    x$resample.params$n.resamples,
    " ",
    resamples,
    ".\n",
    sep = ""
  )
  cat(
    x$metric,
    "was",
    ifelse(x$maximize, "maximized", "minimized"),
    "with the following parameters:\n"
  )
  printls(x$best.tune)
} # rtemis::print.gridSearch
