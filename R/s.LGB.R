# s.LGB.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Light Gradient Boosting (LightGBM) [C, R]
#'
#' Train a Light GBM model for Classification or Regression
#'
#' [gS]: indicates parameter will be autotuned by grid search if multiple values are passed
#' ***This function needs updating/testing with the latest LightGBM update
#' Read more about Light GBM parameter tuning on
#' \href{https://github.com/Microsoft/LightGBM/blob/master/docs/Parameters-tuning.md}{Microsoft's LightGBM github site}
#' and
#' \href{https://sites.google.com/view/lauraepp/home}{Laurae++'s site}
#'
#' Some suggestions from Microsoft's site:
#'
#' To increase accuracy:
#' \itemize{
#'   \item Use large max_bin (may be slower)
#'   \item Use small learning_rate with large num_iterations
#'   \item Use large num_leaves(may cause over-fitting)
#'   \item Use bigger training data
#'   \item Try dart
#' }
#'
#' To minimize over-fitting:
#' \itemize{
#'   \item Use small max_bin
#'   \item Use small num_leaves
#'   \item Use min_data_in_leaf and min_sum_hessian_in_leaf
#'   \item Use bagging by set bagging_fraction and bagging_freq
#'   \item Use feature sub-sampling by set feature_fraction
#'   \item Use bigger training data
#'   \item Try lambda_l1, lambda_l2 and min_gain_to_split to regularization
#'   \item Try max_depth to avoid growing deep tree
#' }
#' Since LGB runs in parallel, default is to use all threads for LGB and set n.cores for
#' gridSearchLearn to 1
#'
#' @inheritParams s.GLM
#' @param objective String: Objective. Default = "regression" for regression, "binary_logloss" for
#' two-class classification, and "multi_logloss" for multi-class
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s.LGB <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  weights = NULL,
                  ipw = TRUE,
                  ipw.type = 2,
                  upsample = FALSE,
                  upsample.seed = NULL,
                  objective = NULL,
                  metric = NULL,
                  maximize = NULL,
                  boosting = "gbdt",
                  n.trees = 50000,
                  learning_rate = 0.01,
                  num_leaves = 31,
                  tree_learner = "serial",
                  max_depth = -1,
                  min_data_in_leaf = 4,
                  min_sum_hessian_in_leaf = 1e-3,
                  feature_fraction = 1,
                  feature_fraction_seed = 2,
                  max_bin = 255,
                  bagging_fraction = 1,
                  bagging_freq = 0,
                  bagging_seed = 3,
                  early_stopping_round = 10,
                  lambda_l1 = 0,
                  lambda_l2 = 0,
                  min_gain_to_split = 0,
                  drop_rate = .1, # only dart \/
                  skip_drop = .5,
                  max_drop = 50,
                  uniform_drop = FALSE,
                  xgboost_dart_mode = FALSE,
                  drop_seed = 4,
                  top_rate = .2, # only goss \/
                  other_rate = .1,
                  # categorical_feature = "",
                  resampler = "strat.sub",
                  n.resamples = 4,
                  cv.p = 0.75,
                  cv.groups = 4,
                  stratify.var = NULL,
                  target.length = NULL,
                  seed = NULL,
                  importance = FALSE,
                  plot.res = TRUE,
                  save.res = FALSE,
                  save.res.mod = FALSE,
                  error.curve = FALSE, # not yet implemented
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  LGB.verbose = 0,
                  n.cores = 1,
                  num_threads = detectCores(),
                  parallel.type = c("psock", "fork"),
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
                  save.dump = FALSE, ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.LGB))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "LGB"

  # [ DEPENDENCIES ] ====
  if (!depCheck(c("lightgbm", "pbapply"), verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.LGB))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (is.null(n.cores)) n.cores <- parallel::detectCores()
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = F), "/")
  parallel.type <- match.arg(parallel.type) # won't be using this anyway

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  df.train <- data.frame(x, y)
  colnames(df.train)[ncol(df.train)] <- y.name
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (is.null(stratify.var)) stratify.var <- y
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  num_class <- 1
  if (type == "Classification") {
    nclass <- length(levels(y))
    if (nclass > 2) num_class <- nclass
    y.int <- as.integer(y) - 1
    df.train[, ncol(df.train)] <- y.int
  } else {
    y.int <- y
  }
  categorical <- sapply(x, is.factor)
  categorical_feature <- if (any(categorical)) which(categorical) else ""
  if (any(categorical)) {
    for (i in categorical_feature) {
      df.train[, i] <- as.integer(df.train[, i])
      if (!is.null(x.test)) x.test[, i] <- as.integer(x.test[, i])
    }
  }
  nfeatures <- ncol(x)
  if (is.null(.weights)) .weights <- rep(1, NROW(y))/NROW(y) # sum = 1

  # [ OBJECTIVE ] ====
  if (type == "Classification") {
    # num_class <- length(levels(y))
    if (is.null(objective)) objective <- if (num_class > 2) "multiclass" else "binary"
    if (is.null(metric)) metric <- if (num_class > 2) "multi_logloss" else "binary_logloss"
    if (is.null(maximize)) maximize <- FALSE
  } else {
    # num_class <- 1
    if (is.null(objective)) objective <- "regression"
    if (is.null(metric)) metric <- "l2"
    if (is.null(maximize)) maximize <- FALSE
  }
  select.fn <- if (maximize) which.max else which.min

  # [ PARAMETERS ] ====
  # All parameters not present in grid
  params <- list(objective = objective,
                 metric = metric,
                 num_class = num_class,
                 boosting = boosting,
                 bagging_seed = bagging_seed,
                 early_stopping_round = early_stopping_round,
                 categorical_feature = categorical_feature,
                 num_threads = num_threads)
  if (boosting == "dart") {
    params <- c(params, list(drop_rate = drop_rate,
                             skip_drop = skip_drop,
                             max_drop = max_drop,
                             uniform_drop = uniform_drop,
                             xgboost_dart_mode = xgboost_dart_mode,
                             drop_seed = drop_seed))
  } else if (boosting == "goss") {
    params <- c(params, list(top_rate = top_rate,
                             other_rate = other_rate))
  }

  # [ MAIN ] ====
  if (n.resamples > 0) {

    # {{ GRID SEARCH WITH INTERNAL RESAMPLING }}

    # [ RESAMPLES ] ====
    n.resamples <- as.integer(n.resamples)
    if (is.null(target.length)) target.length <- length(y)
    res.part <- resample(y = stratify.var,
                         n.resamples = n.resamples,
                         resampler = resampler,
                         cv.p = cv.p,
                         cv.groups = cv.groups,
                         target.length = target.length,
                         seed = seed)

    # [ {GRID} FN ] ====
    LGB1 <- function(index, grid,
                     .data,
                     res.part,
                     params,
                     nrounds,
                     early_stopping_round,
                     weights,
                     LGB.verbose) {
      s.out1 <- list(mod.name = "grid.LGB", call = NULL)
      grid.line <- grid[index, ]
      params1 <- as.list(grid.line[1, 1:(ncol(grid.line) - 2)]) # CHECK
      s.out1$params1 <- params1 <- c(params, params1)
      res.id <- grid.line$res.id
      # msg("/// Running grid line ", index, " of ", NROW(grid), "...", sep = "")
      weights.train <- weights[res.part[[res.id]]]
      dtrain1 <- .data[res.part[[res.id]], ]
      dtrain1 <- lightgbm::lgb.Dataset(data.matrix(dtrain1[, 1:nfeatures]),
                                       label = dtrain1[, nfeatures + 1],
                                       weight = weights.train)
      weights.test <- weights[-res.part[[res.id]]]
      dtest1 <- .data[-res.part[[res.id]], ]
      dtest1 <- lightgbm::lgb.Dataset.create.valid(dtrain1, data.matrix(dtest1[, 1:nfeatures]),
                                                   label = dtest1[, nfeatures + 1],
                                                   weight = weights.test)

      # watchlist <- if(LGB.verbose > 0) list(test = dtest1, train = dtrain1) else list()
      mod.LGB1 <- lightgbm::lgb.train(params = params1,
                                      data = dtrain1,
                                      nrounds = nrounds,
                                      valids = list(test = dtest1),
                                      verbose = LGB.verbose)
      if (save.res.mod) s.out1$mod.LGB1 <- mod.LGB1
      s.out1$best_iter <- mod.LGB1$best_iter
      s.out1$best_score <- mod.LGB1$best_score

      # pred1 <- predict(mod.LGB1, data.test1)
      # pred1 <- predict(mod.LGB1, LGBoost::LGB.DMatrix(data = as.matrix(x.test.g)))
      # err1 <- mse(pred1, y.test.g)
      # s.out1$mse <- err1

      # TODO: Check error curves
      if (error.curve) {
        if (type == "Regression") {
          # ntreelimit <- seq(1, nrounds, 10)
          ntreelimit <- 1:(mod.LGB1$best_iter + early_stopping_round)
          fitted.1.series <- sapply(ntreelimit, function(i) {
            predict(mod.LGB1, dtrain1, ntreelimit = i) })
          mse.train.1.series <- apply(fitted.1.series, 2, function(f) mse(dtrain1$y, f))
          predicted.1.series <- sapply(ntreelimit, function(i) {
            predict(mod.LGB1, dtest1, ntreelimit = i) })
          mse.test.1.series <- apply(predicted.1.series, 2, function(p) mse(dtest1$y, p))
          if (plot.res) mplot3.xy(ntreelimit, list(mse.test.1.series, mse.train.1.series),
                                  type = "l", group.legend = F, xlab = "N iterations", ylab = "MSE", lwd = 4,
                                  vline = mod.LGB1$best_iter, vline.lty = 2, vline.lwd = 2,
                                  legend.tc = paste("best n.trees =", mod.LGB1$best_iter,
                                                    "\nMSE.test =", ddSci(mse.test.1.series[mod.LGB1$best_iter])))
        } # TODO: add else for Classification accuracy curves
      }
      return(s.out1)
    } # END {GRID} FN

    # [ GRID SEARCH ] ====
    if (boosting == "gbdt") {
      grid <- expand.grid(learning_rate = learning_rate,
                          num_leaves = num_leaves,
                          tree_learner = tree_learner,
                          max_depth = max_depth,
                          min_data_in_leaf = min_data_in_leaf,
                          min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                          feature_fraction = feature_fraction,
                          feature_fraction_seed = feature_fraction_seed,
                          max_bin = max_bin,
                          bagging_fraction = bagging_fraction,
                          bagging_freq = bagging_freq,
                          lambda_l1 = lambda_l1,
                          lambda_l2 = lambda_l2,
                          min_gain_to_split = min_gain_to_split,
                          res.id = 1:n.resamples,
                          stringsAsFactors = FALSE)
      if (verbose) gridSummary(learning_rate,
                               num_leaves,
                               tree_learner,
                               max_depth,
                               min_data_in_leaf,
                               min_sum_hessian_in_leaf,
                               feature_fraction,
                               feature_fraction_seed,
                               max_bin,
                               bagging_fraction,
                               bagging_freq,
                               lambda_l1,
                               lambda_l2,
                               min_gain_to_split)
    } else if (boosting == "dart") {
      grid <- expand.grid(learning_rate = learning_rate,
                          num_leaves = num_leaves,
                          tree_learner = tree_learner,
                          max_depth = max_depth,
                          min_data_in_leaf = min_data_in_leaf,
                          min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                          feature_fraction = feature_fraction,
                          feature_fraction_seed = feature_fraction_seed,
                          max_bin = max_bin,
                          bagging_fraction = bagging_fraction,
                          bagging_freq = bagging_freq,
                          lambda_l1 = lambda_l1,
                          lambda_l2 = lambda_l2,
                          min_gain_to_split = min_gain_to_split,
                          drop_rate = drop_rate,
                          skip_drop = skip_drop,
                          max_drop = max_drop,
                          uniform_drop = uniform_drop,
                          drop_seed = drop_seed,
                          res.id = 1:n.resamples,
                          stringsAsFactors = FALSE)
      if (verbose) gridSummary(learning_rate,
                               num_leaves,
                               tree_learner,
                               max_depth,
                               min_data_in_leaf,
                               min_sum_hessian_in_leaf,
                               feature_fraction,
                               feature_fraction_seed,
                               max_bin,
                               bagging_fraction,
                               bagging_freq,
                               lambda_l1,
                               lambda_l2,
                               min_gain_to_split,
                               drop_rate,
                               skip_drop,
                               max_drop,
                               uniform_drop,
                               drop_seed)
    } else if (boosting == "goss") {
      grid <- expand.grid(learning_rate = learning_rate,
                          num_leaves = num_leaves,
                          tree_learner = tree_learner,
                          max_depth = max_depth,
                          min_data_in_leaf = min_data_in_leaf,
                          min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                          feature_fraction = feature_fraction,
                          feature_fraction_seed = feature_fraction_seed,
                          max_bin = max_bin,
                          bagging_fraction = bagging_fraction,
                          bagging_freq = bagging_freq,
                          lambda_l1 = lambda_l1,
                          lambda_l2 = lambda_l2,
                          min_gain_to_split = min_gain_to_split,
                          top_rate = top_rate,
                          other_rate = other_rate,
                          res.id = 1:n.resamples,
                          stringsAsFactors = FALSE)
      if (verbose) gridSummary(learning_rate,
                               num_leaves,
                               tree_learner,
                               max_depth,
                               min_data_in_leaf,
                               min_sum_hessian_in_leaf,
                               feature_fraction,
                               feature_fraction_seed,
                               max_bin,
                               bagging_fraction,
                               bagging_freq,
                               lambda_l1,
                               lambda_l2,
                               min_gain_to_split,
                               top_rate,
                               other_rate)
    }

    grid$id <- 1:NROW(grid)
    n.gridLines <- NROW(grid)
    # if (n.gridLines < n.cores) n.cores <- n.gridLines

    # [ GRID RUN ] ====
    if (verbose) msg("Running LGB grid search:",
                     "\n                                 N models total = ", n.gridLines,
                     "\n                                 N resamples = ", n.resamples, sep = "")
    # Leave parallelism to LGB
    # if (!verbose) pbapply::pboptions(type = "none") # no progress bar
    # if (n.cores > 1) {
    #   if (parallel.type == "psock") {
    #     if (verbose) msg("Starting PSOCK cluster on", n.cores, "cores...")
    #     cl <- makePSOCKcluster(n.cores)
    #     on.exit(stopCluster(cl))
    #     clusterEvalQ(cl, library("rtemis"))
    #   } else {
    #     if (verbose) msg("Parallelizing by forking on", n.cores, "cores...")
    #     cl <- n.cores
    #   }
    # } else {
    #   cl <- 1
    # }
    if (!is.null(logFile)) sink() # pause writing to file
    grid.run <- pbapply::pblapply(1:n.gridLines, LGB1,
                                  grid = grid,
                                  .data = df.train,
                                  res.part = res.part,
                                  params = params,
                                  nrounds = n.trees,
                                  early_stopping_round = early_stopping_round,
                                  weights = .weights,
                                  LGB.verbose = LGB.verbose,
                                  cl = 1)
    if (!is.null(logFile)) sink(logFile, append = TRUE, split = verbose) # Resume writing to log
    msg("LGB grid search complete")
    names(grid.run) <- paste0("LGB.gridLine.", 1:n.gridLines)
    grid.performance <- data.frame(grid, plyr::ldply(grid.run,
                                                     function(g) data.frame(best.nrounds = g$best_iter,
                                                                            best.score = g$best_score)))
    grid.performance$tune.id <- factor(rep(c(1:(n.gridLines/n.resamples)), n.resamples))

    grid.by.tune.id <- grid[grid$res.id == 1, 1:(ncol(grid) - 1)]

    grid.performance.by.tune.id <- data.frame(grid.by.tune.id,
                                              aggregate(cbind(best.nrounds = grid.performance$best.nrounds,
                                                              best.score = grid.performance$best.score),
                                                        by = list(tune.id = grid.performance$tune.id),
                                                        mean))

    best.tune <- grid.performance.by.tune.id[select.fn(grid.performance.by.tune.id$best.score), ]
    best.tune$best.nrounds <- as.integer(best.tune$best.nrounds)

    params <- c(params,
                learning_rate = best.tune$learning_rate,
                num_leaves = best.tune$num_leaves,
                tree_learner = best.tune$tree_learner,
                max_depth = best.tune$max_depth,
                min_data_in_leaf = best.tune$min_data_in_leaf,
                min_sum_hessian_in_leaf = best.tune$min_sum_hessian_in_leaf,
                feature_fraction = best.tune$feature_fraction,
                feature_fraction_seed = best.tune$feature_fraction_seed,
                max_bin = best.tune$max_bin,
                bagging_fraction = best.tune$bagging_fraction,
                bagging_freq = best.tune$bagging_freq,
                lambda_l1 = best.tune$lambda_l1,
                lambda_l2 = best.tune$lambda_l2,
                min_gain_to_split = best.tune$min_gain_to_split)
    if (metric == "multi_logloss") params$num_class <- num_class
    if (boosting == "dart") {
      params <- c(params,
                  list(drop_rate = best.tune$drop_rate,
                       skip_drop = best.tune$skip_drop,
                       max_drop = best.tune$max_drop,
                       uniform_drop = best.tune$uniform_drop,
                       drop_seed = best.tune$drop_seed))
    } else if (boosting == "goss") {
      params <- c(params,
                  list(top_rate = best.tune$top_rate,
                       other_rate = best.tune$other_rate))
    }
    n.trees <- best.tune$best.nrounds
    if (verbose) parameterSummary(best.tune, title = "Tuning Results")
  } else {

    # {{ NO GRID SEARCH NOR INTERNAL RESAMPLING }} ====
    res.part <- grid.performance <- grid.performance.by.tune.id <- best.tune <- NULL

    params <- c(params,
                learning_rate = learning_rate,
                num_leaves = num_leaves,
                tree_learner = tree_learner,
                max_depth = max_depth,
                min_data_in_leaf = min_data_in_leaf,
                min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                feature_fraction = feature_fraction,
                feature_fraction_seed = feature_fraction_seed,
                max_bin = max_bin,
                bagging_fraction = bagging_fraction,
                bagging_freq = bagging_freq,
                lambda_l1 = lambda_l1,
                lambda_l2 = lambda_l2,
                min_gain_to_split = min_gain_to_split)
    if (metric == "multi_logloss") params$num_class <- num_class

    if (boosting == "dart") {
      params <- c(params,
                  list(drop_rate = drop_rate,
                       skip_drop = skip_drop,
                       max_drop = max_drop,
                       uniform_drop = uniform_drop,
                       drop_seed = drop_seed))
    } else if (boosting == "goss") {
      params <- c(params,
                  list(top_rate = top_rate,
                       other_rate = other_rate))
    }
  }

  # [ FULL LGB ] ====
  dtrain <- lightgbm::lgb.Dataset(data.matrix(x),
                                  label = y.int,
                                  weight = .weights)
  params$early_stopping_round <- NULL
  if (verbose) msg("Training full LGB model...", newline = TRUE)
  mod <- lightgbm::lgb.train(params,
                             data = dtrain,
                             nrounds = n.trees,
                             verbose = LGB.verbose)

  # [ FITTED ] ====
  fitted <- predict(mod, data.matrix(df.train[1:(ncol(df.train) - 1)]), reshape = TRUE)
  if (type == "Classification") {
    # round() gives correct result whether response is integer or probability
    fitted.prob <- fitted
    if (nclass == 2) {
      fitted <- factor(levels(y)[round(fitted) + 1], levels = levels(y))
    } else {
      fitted <- factor(apply(fitted.prob, 1, which.max), levels = levels(y))
    }
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, data.matrix(x.test), reshape = TRUE)
    if (type == "Classification") {
      predicted.prob <- predicted
      if (nclass == 2) {
        predicted <- factor(levels(y)[round(predicted) + 1], levels = levels(y))
      } else {
        predicted <- factor(apply(predicted.prob, 1, which.max), levels = levels(y))
      }
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ VARIABLE IMPORTANCE ] ====
  .importance <- NULL
  # This may take a little while
  if (importance) {
    if (verbose) msg("Estimating variable importance...")
    .importance <- lightgbm::lgb.importance(model = mod)
  }

  # [ OUTRO ] ====
  # sink(logOut, append = T, split = T)
  extra <- list(weights = .weights,
                resampler = resampler,
                boosting = boosting,
                resamples = res.part,
                grid = grid,
                grid.run = if (save.res) grid.run else NULL,
                grid.performance = grid.performance,
                grid.performance.by.tune.id = grid.performance.by.tune.id,
                best.tune = best.tune,
                params = params,
                nrounds = n.trees,
                objective = objective,
                metric = metric,
                fitted.prob = if (type == "Classification") fitted.prob else NULL,
                predicted.prob = if (type == "Classification") predicted.prob else NULL,
                importance = .importance)
  rt <- rtModSet(rtclass = rtclass,
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 parameters = params,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
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

  if (!is.null(outdir) & save.dump) {
    DUMPpath <- paste0(outdir, "s.LGB.dump")
    lightgbm::saveRDS.lgb.Booster(mod, DUMPpath)
  }
  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::s.LGB
