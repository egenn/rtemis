# gridSearchLearn.R
# ::rtemis::
# 2016-8 E.D. Gennatas lambdamd.org
# TODO: Add support for Survival

#' \pkg{rtemis} internal: Grid Search for Hyperparameter Tuning of \pkg{rtemis} Learners
#'
#' Train models using a combination of parameter values for model selection
#'
#' Note that weights, if defined (and not NULL), should be passed directly to \code{gridSearchLearn}
#' as they need to be resampled along \code{x} and \code{y}, and should not be passed along with
#' \code{grid.params}. \code{ipw} and \code{ipw.type} should be passed as part of \code{grid.params}
#' and will be passed on to the learner.
#' Includes a special case for training \link{s.H2OGBM} or \link{s.GBM} which requires extracting and averaging n.trees
#' along with params.
#'
#' @param x features - training set. Will be resampled to multiple train-test sets
#' @param y outcome - training set. Will be resampled to multiple train-test sets
#' @param mod Character: \pkg{rtemis} model. See \code{modSelect()} gives available models
#' @param grid.params List of named elements, each is a vector of values
#' @param fixed.params List of named elements, each is a single value
#'   (Classification will always maximize Accuracy)
#' @param search.type Character: "exhaustive" (Default), "randomized". Type of grid search to use. Exhaustive search will
#' try all combinations of parameters. Randomized will try a random sample of size \code{randomize.p} * N
#' of all combinations
#' @param resample.rtset List: Output of \code{rtset.grid.resample()}
#' @param randomized.p Float (0, 1): For \code{search.type == "exhaustive"}, sample this portion of combination. Default = .05
#' @param weights Float, vector: Case weights
#' @param error.aggregate.fn Function: Use this when aggregating error metrics. Default = mean
#' @param metric Character: Metric to minimize or maximize
#' @param maximize Logical: If TRUE, maximize \code{metric}
#' @param save.mod Logical: If TRUE, save all trained models. Default = FALSE
#' @param verbose Logical: If TRUE, print messages to screen
#' @param call.depth Integer: passed to \link{msg}. Default = 2
#' @param grid.verbose Logical: Passed to \code{learner}'s \code{verbose} argument
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional arguments to be passed to \link{resample}
#'
#' @author E.D. Gennatas
#' @noRd

gridSearchLearn <- function(x, y, mod,
                            grid.params,
                            fixed.params = NULL,
                            search.type = c("exhaustive", "randomized"),
                            resample.rtset = rtset.resample(),
                            randomized.p = .05,
                            weights = NULL,
                            error.aggregate.fn = mean,
                            metric = NULL,
                            maximize = NULL,
                            save.mod = FALSE,
                            verbose = TRUE,
                            call.depth = 1,
                            grid.verbose = FALSE,
                            n.cores = rtCores, ...) {

  # Intro ====
  start.time <- intro(verbose = verbose,
                      call.depth = call.depth,
                      message = "Running grid search...",
                      newline.pre = TRUE)

  # Arguments ====
  if (missing(x) | missing(y)) {
    print(args(gridSearchLearn))
    stop("Input missing")
  }
  search.type <- match.arg(search.type)
  n.resamples <- resample.rtset$n.resamples
  n.cores <- as.numeric(n.cores)[1]

  # Data ====
  x <- as.data.frame(x)

  # Grid ====
  # Filter out any grid.params with "NULL" value: expand.grid will fail otherwise.
  # Since these will not be present in best.tune, assignment to the non-existent named elements will result in NULL,
  # as required. This is needed for functions with parameters that can take NULL value
  grid.params <- Filter(Negate(is.null), grid.params)
  param.grid <- expand.grid(c(list(res.id = seq(n.resamples)), grid.params), stringsAsFactors = FALSE)
  n.param.combs <- NROW(param.grid) / n.resamples
  if (search.type == "randomized") {
    index.per.resample <- sample(n.param.combs, round(randomized.p * n.param.combs))
    param.grid <- param.grid[rep(index.per.resample, n.resamples), ]
  }
  learner <- modSelect(mod, fn = FALSE)
  res <- resample(y = y, rtset = resample.rtset, verbose = verbose)

  # {Grid function} ====
  learner1 <- function(index, learner,
                       x, y,
                       res,
                       param.grid,
                       fixed.params,
                       weights,
                       verbose,
                       save.mod) {
    if (verbose) msg("Running grid line #", index, " of ", NROW(param.grid), "...", sep = "")
    res1 <- res[[param.grid[index, 1]]]
    x.train1 <- x[res1, ]
    y.train1 <- y[res1]
    weights1 <- weights[res1]
    x.test1 <- x[-res1, ]
    y.test1 <- y[-res1]
    args <- c(list(x = x.train1, y = y.train1,
                   x.test = x.test1, y.test = y.test1,
                   weights = weights1,
                   print.plot = FALSE, verbose = verbose),
              as.list(param.grid[index, 2:NCOL(param.grid), drop = FALSE]),
              fixed.params)
    mod1 <- do.call(learner, args)
    out1 <- list(id = index,
                 res.id = param.grid[index, 1],
                 error.train = mod1$error.train,
                 error.test = mod1$error.test,
                 type = mod1$type,
                 params = args)

    # '-- Learner-specific collect ====
    if (learner == "s.H2OGBM") out1$est.n.trees <- mod1$mod@model$model_summary$number_of_trees
    if (learner == "s.GBM" | learner == "s.GBM3") {
      out1$est.n.trees <- which.min(mod1$mod$valid.error)
      if (length(out1$est.n.trees) == 0) out1$est.n.trees <- NA
    }
    if (learner == "s.GLMNET") {
      out1$lambda.min <- mod1$mod$lambda.min
      out1$lambda.1se <- mod1$mod$lambda.1se
    }
    if (learner %in% c("s.LINAD", "s.LINOA")) out1$est.n.leaves <- mod1$mod$n.leaves
    if (learner == "s.LIHADBOOST") out1$sel.n.steps <- mod1$mod$selected.n.steps
    if (save.mod) out1$mod1 <- mod1
    out1
  }

  # Grid run ====
  if (verbose) parameterSummary(grid.params, fixed.params, title = "Search parameters")
  if (verbose) msg("Tuning", modSelect(mod, desc = TRUE), "by", search.type, "grid search:")
  if (verbose) msg(n.resamples, " resamples; ", NROW(param.grid), " models total; running on ",
                   n.cores, " ", ifelse(n.cores > 1, "cores", "core"),
                   " (", Sys.getenv("R_PLATFORM"), ")\n", sep = "")
  if (verbose) {
    pbapply::pboptions(type = "timer")
  } else {
    pbapply::pboptions(type = "none")
  }
  grid.run <- pbapply::pblapply(seq(NROW(param.grid)), learner1,
                                learner,
                                x, y, res,
                                param.grid,
                                fixed.params,
                                weights,
                                verbose = grid.verbose,
                                save.mod = save.mod,
                                cl = n.cores)

  # Metric ====
  type <- grid.run[[1]]$type
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
    maximize <- if (metric %in% c("Accuracy", "Balanced Accuracy", "Concordance")) TRUE else FALSE
  }
  select.fn <- if (maximize) which.max else which.min
  verb <- if (maximize) "maximize" else "minimize"

  # Aggregate ====
  n.params <- length(grid.params)
  # Average test errors
  if (type %in% c("Regression", "Survival")) {
    error.test.all <- as.data.frame(t(sapply(grid.run, function(r) unlist(r$error.test))))
  } else if (type == "Classification") {
    error.test.all <- as.data.frame(t(sapply(grid.run, function(r) unlist(r$error.test$Overall))))
  }
  error.test.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
  error.test.mean.by.param.id <- aggregate(error.test.all,
                                           by = list(param.id = error.test.all$param.id),
                                           error.aggregate.fn)[, -1]
  tune.results <- cbind(expand.grid(grid.params), error.test.mean.by.param.id)

  # N of iterations is the one hyperparameter that may be determined automatically,
  # we therefore need to extract it and average it

  # '- GBM, H2OGBM ====
  if (learner %in% c("s.H2OGBM", "s.GBM", "s.GBM3")) {
    est.n.trees.all <- data.frame(n.trees = plyr::laply(grid.run, function(x) x$est.n.trees))
    est.n.trees.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.n.trees.by.param.id <- aggregate(n.trees ~ param.id, est.n.trees.all,
                                         error.aggregate.fn)
    tune.results <- cbind(n.trees = round(est.n.trees.by.param.id$n.trees), tune.results)
    n.params <- n.params + 1
  }

  # '- XGBoost ====
  if (learner == "XGB") {
    est.nrounds.all <- data.frame(nrounds = plyr::laply(grid.run, function(x) x$best_iteration))
    est.nrounds.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.nrounds.by.param.id <- aggregate(nrounds ~ param.id, est.nrounds.all,
                                         error.aggregate.fn)
    tune.results <- cbind(nrounds = round(est.nrounds.by.param.id$nrounds), tune.results)
    n.params <- n.params + 1
  }

  # '- GLMNET ====
  if (learner == "s.GLMNET") {
    if (is.null(grid.params$lambda)) {
      # if lambda was NULL, cv.glmnet was run and optimal lambda was estimated
      lambda.all <- data.frame(lambda = plyr::laply(grid.run, function(x) x[[fixed.params$which.cv.lambda]]))
      lambda.all$param.id <- rep(1:n.param.combs, each = n.resamples)
      lambda.by.param.id <- aggregate(lambda ~ param.id, lambda.all,
                                      error.aggregate.fn)
      tune.results <- cbind(lambda = lambda.by.param.id$lambda, tune.results)
      n.params <- n.params + 1
    }
  }

  # '- LINAD ====
  if (learner %in% c("s.LINAD", "s.LINOA")) {
    # ERROR
    est.n.leaves.all <- data.frame(n.leaves = plyr::laply(grid.run,
                                                          function(x) ifelse(length(x$est.n.leaves) == 0, 1, x$est.n.leaves)))
    est.n.leaves.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.n.leaves.by.param.id <- aggregate(n.leaves ~ param.id, est.n.leaves.all,
                                          error.aggregate.fn)
    tune.results <- cbind(n.leaves = round(est.n.leaves.by.param.id$n.leaves), tune.results)
    n.params <- n.params + 1
  }

  # '- LIHADBOOST ====
  if (learner == "s.LIHADBOOST") {
    est.n.steps.all <- data.frame(n.steps = plyr::laply(grid.run, function(x) x$sel.n.steps))
    est.n.steps.all$param.id <- rep(seq_len(n.param.combs), each = n.resamples)
    est.n.steps.by.param.id <- aggregate(n.steps ~ param.id, est.n.steps.all,
                                          error.aggregate.fn)
    tune.results <- cbind(n.steps = round(est.n.steps.by.param.id$n.steps), tune.results)
    n.params <- n.params + 1
  }

  # TODO: consider explicitly ordering hyperparam values in increasing order if this makes sense,
  # so that in case of tie, lowest value is chosen - if that makes sense, e.g. n.leaves, etc.
  best.tune <- tune.results[select.fn(tune.results[[metric]]), seq_len(n.params),
                            drop = FALSE]
  if (verbose) parameterSummary(best.tune, title = paste("Best parameters to", verb, metric))


  # Outro ====
  outro(start.time, verbose = verbose)
  gs <- list(type = search.type,
             p = randomized.p,
             resample.rtset = resample.rtset,
             tune.results = tune.results,
             error.test.all = error.test.all,
             best.tune = best.tune,
             metric = metric,
             maximize = maximize,
             params = list(fixed = fixed.params,
                           search = grid.params),
             error.aggregate.fn = deparse(substitute(error.aggregate.fn)))

  if (save.mod) gs$mods <- grid.run

  class(gs) <- c("gridSearch", "list")
  gs

} # rtemis::gridSearchLearn


#' \pkg{rtemis} internal: Grid check
#'
#' Checks if grid search needs to be performed.
#' All tunable parameters should be passed to this function, individually or as a list. If any
#' argument has more than one assigned values, the function returns TRUE, otherwise FALSE. This can
#' be used to check whether \link{gridSearchLearn} must be run.
#'
#' The idea is that if you know which parameter values you want to use, you define them directly
#'   e.g. \code{alpha = 0, lambda = .2}.
#' If you don't know, you enter the set of values to be tested,
#'   e.g. \code{alpha = c(0, .5, 1), lambda = seq(.1, 1, .1)}.
#' @param ... Parameters; will be converted to a list

gridCheck <- function(...) {

  args = list(...)
  any(as.logical(lapply(args, function(a) length(a) > 1)))

} # rtemis::gridCheck


#' \code{print} method for \code{gridSearch} object
#'
#' @method print gridSearch
#' @export
#' @author E.D. Gennatas

print.gridSearch <- function(x, ...) {

  objcat("gridSearch object")
  type <- if (x$type == "exhaustive") "An exhaustive grid search"
                 else paste0("A randomized grid search (p = ", x$p, ")")
  resamples <- if (x$resample.rtset$resample == "kfold") {
    "independent folds"
  } else if (x$resample.rtset$resample == "strat.sub") {
    "stratified subsamples"
  } else if (x$resample.rtset$resample == "bootstraps") {
    "bootstraps"
  } else if (x$resample.rtset$resample == "strat.boot") {
    "stratified bootstraps"
  }
  cat(type, " was performed using ", x$resample.rtset$n.resamples, " ", resamples, ".\n", sep = "")
  cat(x$metric, "was", ifelse(x$maximize, "maximized", "minimized"), "with the following parameters:\n")
  printls(x$best.tune)

} # rtemis::print.gridSearch
