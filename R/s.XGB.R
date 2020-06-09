# s.XGB.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io
# TODO: check if all objective functions must be minimized, or change which.min to variable
# TODO: weights and ipw do not seem to work, upsample works, check weights passing
# and add scale_pos_weight
# TODO: change fittedClass.raw to fitted.prob
# add which.max / which.min dependent on maximize

#' XGboost Classification and Regression [C, R]
#'
#' Tune hyperparameters using grid search and resampling,
#' train a final model, and validate it
#'
#' [gS]: indicates parameter will be autotuned by grid search if multiple values are passed.
#' (s.XGB does its own grid search, similar to gridSearchLearn, may switch to gridSearchLearn similar to s.GBM)
#' Learn more about XGboost's parameters here: http://xgboost.readthedocs.io/en/latest/parameter.html
#' Case weights and therefore IPW do not seem to work, despite following documentation.
#' See how ipw = T fails and upsample = T works in imbalanced dataset.
#' 11.24.16: Updated to work with latest development version of XGBoost from github, which changed some of
#' \code{xgboost}'s return values and is therefore not compatible with older versions
#' \link{s.XGBLIN} is a wrapper for \code{s.XGB} with \code{booster = "gblinear"}
#' @inheritParams s.GLM
#' @param booster Character: Booster to use. Options: "gbtree", "gblinear"
#' @param silent 0: print XGBoost messages; 1: print no XGBoost messages
#' @param nrounds Integer: Maximum number of rounds to run. Can be set to a high number as early stopping
#'   will limit nrounds by monitoring inner CV error
#' @param force.nrounds Integer: Number of rounds to run if not estimating optimal number by CV
#' @param lambda [gS] L2 regularization on weights
#' @param lambda_bias [gS] for *linear* booster: L2 regularization on bias
#' @param alpha [gS] L1 regularization on weights
#' @param eta [gS] Float (0, 1): Learning rate. Default = .1
#' @param gamma [gS] Float: Minimum loss reduction required to make further partition
#' @param max.depth [gS] Integer: Maximum tree depth. (Default = 6)
#' @param subsample [gS] Float:
#' @param colsample.bytree [gS]
#' @param colsample.bylevel [gS]
#' @param tree.method [gS] XGBoost tree construction algorithm (Default = "auto")
#' @param sketch.eps [gS] Float (0, 1):
#' @param num.parallel.tree Integer: N of trees to grow in parallel: Results in Random Forest -like algorithm.
#'  (Default = 1; i.e. regular boosting)
#' @param base.score Float: The mean outcome response (no need to set)
#' @param objective (Default = NULL)
#' @param sample.type (Default = "uniform")
#' @param normalize.type (Default = "forest")
#' @param obj Function: Custom objective function. See \code{?xgboost::xgboost}
#' @param feval Function: Custom evaluation function. See \code{?xgboost::xgboost}
#' @param xgb.verbose Integer: Verbose level for XGB learners used for tuning.
#' @param print_every_n Integer: Print evaluation metrics every this many iterations
#' @param early.stopping.rounds Integer: Training on resamples of \code{x.train} (tuning) will stop if performance
#'   does not improve for this many rounds
#' @param missing String or Numeric: Which values to consider as missing. Default = NA
#' @param nthread Integer: Number of threads for xgboost using OpenMP. Only parallelize resamples
#' using \code{n.cores} or the xgboost execution using this setting. At the moment of writing, parallelization via this
#' parameter causes a linear booster to fail most of the times. Therefore, default is rtCores
#' for 'gbtree', 1 for 'gblinear'
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s.XGB <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  booster = c("gbtree", "gblinear", "dart"),
                  silent = 1,
                  missing = NA,
                  nrounds = 500L,
                  force.nrounds = NULL,
                  weights = NULL,
                  ipw = TRUE,
                  ipw.type = 2,
                  upsample = FALSE,
                  downsample = FALSE,
                  resample.seed = NULL,
                  obj = NULL,
                  feval = NULL,
                  maximize = NULL,
                  xgb.verbose = NULL,
                  print_every_n = 100L,
                  early.stopping.rounds = 50L,
                  eta = .1,
                  gamma = 0,
                  max.depth = 3,
                  min.child.weight = 5,
                  max.delta.step = 0,
                  subsample = .75,
                  colsample.bytree = NULL,
                  colsample.bylevel = 1,
                  lambda = NULL,
                  lambda.bias = 0,
                  alpha = 0,
                  tree.method = "auto",
                  sketch.eps = .03,
                  num.parallel.tree = 1,
                  base.score = NULL,
                  objective = NULL,
                  sample.type = "uniform",
                  normalize.type = "forest",
                  rate.drop = .1,
                  skip.drop = .5,
                  resampler = "strat.sub",
                  n.resamples = 10,
                  train.p = 0.75,
                  strat.n.bins = 4,
                  stratify.var = NULL,
                  target.length = NULL,
                  seed = NULL,
                  # outcome = NULL,
                  error.curve = FALSE,
                  plot.res = TRUE,
                  save.res = FALSE,
                  save.res.mod = FALSE,
                  importance = FALSE,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  question = NULL,
                  rtclass = NULL,
                  save.dump = FALSE,
                  verbose = TRUE,
                  n.cores = 1,
                  nthread = NULL,
                  parallel.type = c("psock", "fork"),
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE)) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.XGB))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)

  # [ DEPENDENCIES ] ====
  if (!depCheck(c("xgboost", "pbapply"), verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.XGB))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  booster <- match.arg(booster)
  if (booster == "gbtree") {
    mod.name <- "XGB"
  } else if (booster == "dart") {
    mod.name <- "XGBDART"
  } else {
    mod.name <- "XGBLIN"
  }
  if (is.null(nthread)) nthread <- ifelse(booster == "gblinear", 1, rtCores)
  if (is.null(lambda)) lambda <- ifelse(booster == "gblinear", 0, 1)
  if (is.null(colsample.bytree)) colsample.bytree <- ifelse(NCOL(x) > 100, .75, 1)
  if (is.null(n.cores)) n.cores <- rtCores
  if (is.null(xgb.verbose)) xgb.verbose <- ifelse(verbose, 1, 0)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  parallel.type <- match.arg(parallel.type)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  index.factor <- which(sapply(x, is.factor))
  n.factor <- length(index.factor)
  if (n.factor > 0) stop("Please convert all features to numeric before running s.XGB")
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (type == "Classification") y.num <- as.numeric(y) - 1
  nclass <- ifelse(type == "Classification", length(levels(y)), 0)
  if (is.null(objective)) {
    if (type == "Regression") {
      objective <- "reg:linear"
    } else {
      objective <- ifelse(nclass == 2, "binary:logistic", "multi:softmax")
    }
  }
  if (type == "Regression") {
    if (is.null(base.score)) base.score <- mean(y)
    xg.dat <- xgboost::xgb.DMatrix(as.matrix(x),
                                   label = y,
                                   missing = missing)
  } else {
    if (is.null(base.score)) base.score <- mean(as.numeric(y.num))
    xg.dat <- xgboost::xgb.DMatrix(as.matrix(x),
                                   label = y.num,
                                   weight = .weights,
                                   missing = missing)
  }

  if (is.null(stratify.var)) stratify.var <- y
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
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
                         train.p = train.p,
                         strat.n.bins = strat.n.bins,
                         target.length = target.length,
                         seed = seed)

    # [ {GRID} FN ] ====
    xgb.1 <- function(index, grid,
                      x.int, y.int,
                      res.part,
                      nrounds,
                      objective,
                      nclass,
                      weights,
                      xgb.verbose,
                      nthread) {
      s.out.1 <- list(mod.name = "grid.XGB", call = NULL)
      grid.line <- grid[index, ]
      params.1 <- as.list(grid.line[1, 1:(ncol(grid.line) - 2)])
      params.1$booster <- booster
      params.1$objective <- objective
      if (objective == "multi:softmax") params.1$num.class <- nclass
      s.out.1$params.1 <- params.1
      res.id <- grid.line$res.id

      x.train.g <- as.matrix(x.int[res.part[[res.id]], ])
      x.test.g <- as.matrix(x.int[-res.part[[res.id]], ])
      y.train.g <- y.int[res.part[[res.id]]]
      y.test.g <- y.int[-res.part[[res.id]]]
      data.train.1 <- xgboost::xgb.DMatrix(as.matrix(x.train.g),
                                           missing = missing,
                                           label = y.train.g)
      if (!is.null(weights)) xgboost::setinfo(data.train.1, "weight", weights[res.part[[res.id]]])
      data.test.1 <- xgboost::xgb.DMatrix(data = as.matrix(x.test.g), missing = missing, label = y.test.g)
      # xgboost will minimizwe the second element of this list
      # - check by making verbose and running on 1 core
      watchlist <- list(train = data.train.1, test = data.test.1)
      if (verbose) cat("\n")
      mod.xgb.1 <- xgboost::xgb.train(params = params.1,
                                      data = data.train.1,
                                      nrounds = nrounds,
                                      watchlist = watchlist,
                                      obj = obj,
                                      feval = feval,
                                      verbose = xgb.verbose,
                                      print_every_n = print_every_n,
                                      early_stopping_rounds = early.stopping.rounds,
                                      maximize = maximize,
                                      nthread = nthread)
      if (save.res.mod) s.out.1$mod.xgb.1 <- mod.xgb.1
      s.out.1$best_iteration <- bestInd <- mod.xgb.1$best_iteration
      s.out.1$best_score <- mod.xgb.1$best_score

      # Check error curves
      if (error.curve) {
        if (type == "Regression") {
          ntreelimit <- 1:(mod.xgb.1$bestInd + early.stopping.rounds)
          fitted.1.series <- sapply(ntreelimit, function(i) {
            predict(mod.xgb.1, data.train.1, ntreelimit = i) })
          mse.train.1.series <- apply(fitted.1.series, 2, function(f) mse(y.train.g, f))
          predicted.1.series <- sapply(ntreelimit, function(i) {
            predict(mod.xgb.1, data.test.1, ntreelimit = i) })
          mse.test.1.series <- apply(predicted.1.series, 2, function(p) mse(y.test.g, p))
          if (plot.res) mplot3.xy(ntreelimit, list(mse.test.1.series, mse.train.1.series),
                                  type = "l", group.legend = F, xlab = "N iterations", ylab = "MSE", lwd = 4,
                                  vline = mod.xgb.1$bestInd, vline.lty = 2, vline.lwd = 2,
                                  legend.tc = paste("best n.trees =", mod.xgb.1$bestInd,
                                                    "\nMSE.test =", ddSci(mse.test.1.series[bestInd])))
        } # add else for Classification accuracy curves
      }
      return(s.out.1)
    } # END {GRID} FN

    # [ GRID ] ====
    if (booster == "gbtree") {
      grid <- expand.grid(eta = eta,
                          gamma = gamma,
                          max.depth = max.depth,
                          min.child.weight = min.child.weight,
                          max.delta.step = max.delta.step,
                          subsample = subsample,
                          colsample.bytree = colsample.bytree,
                          colsample.bylevel = colsample.bylevel,
                          lambda = lambda,
                          alpha = alpha,
                          tree.method = tree.method,
                          sketch.eps = sketch.eps,
                          num.parallel.tree = num.parallel.tree,
                          res.id = 1:n.resamples)
      if (verbose) gridSummary(eta, gamma, max.depth, min.child.weight, max.delta.step, subsample,
                               colsample.bytree, colsample.bylevel, lambda, alpha, tree.method, sketch.eps,
                               num.parallel.tree)
    } else if (booster == "dart") {
      grid <- expand.grid(eta = eta,
                          gamma = gamma,
                          max.depth = max.depth,
                          min.child.weight = min.child.weight,
                          max.delta.step = max.delta.step,
                          subsample = subsample,
                          colsample.bytree = colsample.bytree,
                          colsample.bylevel = colsample.bylevel,
                          lambda = lambda,
                          alpha = alpha,
                          tree.method = tree.method,
                          sample.type = sample.type,
                          normalize.type = normalize.type,
                          rate.drop = rate.drop,
                          skip.drop = skip.drop,
                          sketch.eps = sketch.eps,
                          num.parallel.tree = num.parallel.tree,
                          res.id = 1:n.resamples)
      if (verbose) gridSummary(eta, gamma, max.depth, min.child.weight, max.delta.step, subsample,
                               colsample.bytree, colsample.bylevel, lambda, alpha, tree.method,
                               sample.type, normalize.type, rate.drop, skip.drop, sketch.eps,
                               num.parallel.tree)
    } else {
      grid <- expand.grid(lambda = lambda,
                          alpha = alpha,
                          lambda.bias = lambda.bias,
                          res.id = 1:n.resamples)
      if (verbose) gridSummary(lambda, alpha, lambda.bias)
    }
    grid$id <- 1:NROW(grid)
    n.gridLines <- NROW(grid)
    if (n.gridLines < n.cores) n.cores <- n.gridLines

    # [ GRID RUN ] ====
    if (verbose) msg("Running XGB grid search:",
                     "\n                            N models total = ", n.gridLines,
                     "\n                            N resamples = ", n.resamples,
                     "\n                            N parallel resamples = ", n.cores,
                     "\n                            N XGboost threads = ", nthread, sep = "")
    if (type == "Regression") y.int <- y else y.int <- y.num
    if (!verbose) pbapply::pboptions(type = "none") # no progress bar
    if (n.cores > 1) {
      if (parallel.type == "psock") {
        if (verbose) msg("Starting PSOCK cluster on", n.cores, "cores...")
        cl <- makePSOCKcluster(n.cores)
        on.exit(stopCluster(cl))
        clusterEvalQ(cl, library("rtemis"))
      } else {
        if (verbose) msg("Parallelizing by forking on", n.cores, "cores...")
        cl <- n.cores
      }
    } else {
      cl <- 1
    }
    if (!is.null(logFile)) sink() # pause writing to file
    grid.run <- pbapply::pblapply(1:n.gridLines, xgb.1,
                                  grid = grid,
                                  x.int = as.data.frame(x),
                                  y.int = y.int,
                                  res.part = res.part,
                                  nrounds = nrounds,
                                  objective = objective,
                                  nclass = nclass,
                                  weights = .weights,
                                  xgb.verbose = xgb.verbose,
                                  nthread = nthread,
                                  cl = n.cores)
    if (!is.null(logFile)) sink(logFile, append = TRUE, split = verbose) # Resume writing to log
    if (verbose) msg("Grid search complete")
    names(grid.run) <- paste0("xgb.gridLine.", 1:n.gridLines)
    grid.performance <- data.frame(grid, plyr::ldply(grid.run,
                                                     function(g) data.frame(best.nrounds = g$best_iteration,
                                                                            bestScore = g$best_score)))
    grid.performance$tune.id <- factor(rep(c(1:(n.gridLines/n.resamples)), n.resamples))

    if (booster == "gbtree") {
      grid.by.tune.id <- expand.grid(eta = eta,
                                     gamma = gamma,
                                     max.depth = max.depth,
                                     min.child.weight = min.child.weight,
                                     max.delta.step = max.delta.step,
                                     subsample = subsample,
                                     colsample.bytree = colsample.bytree,
                                     colsample.bylevel = colsample.bylevel,
                                     lambda = lambda,
                                     alpha = alpha,
                                     tree.method = tree.method,
                                     sketch.eps = sketch.eps,
                                     num.parallel.tree = num.parallel.tree)
    } else {
      grid.by.tune.id <- expand.grid(lambda = lambda,
                                     alpha = alpha,
                                     lambda.bias = lambda.bias)
    }

    grid.performance.by.tune.id <- data.frame(grid.by.tune.id,
                                              aggregate(cbind(best.nrounds = grid.performance$best.nrounds,
                                                              bestScore = grid.performance$bestScore),
                                                        by = list(tune.id = grid.performance$tune.id),
                                                        mean))

    best.tune <- grid.performance.by.tune.id[which.min(grid.performance.by.tune.id$bestScore), ]
    best.tune$best.nrounds <- as.integer(best.tune$best.nrounds)

    if (booster == "gbtree") {
      params <- list(booster = booster,
                     silent = silent,
                     eta = best.tune$eta,
                     gamma = best.tune$gamma,
                     max.depth = best.tune$max.depth,
                     min.child.weight = best.tune$min.child.weight,
                     max.delta.step = best.tune$max.delta.step,
                     subsample = best.tune$subsample,
                     colsample.bytree = best.tune$colsample.bytree,
                     colsample.bylevel = best.tune$colsample.bylevel,
                     lambda = best.tune$lambda,
                     alpha = best.tune$alpha,
                     tree.method = best.tune$tree.method,
                     sketch.eps = best.tune$sketch.eps,
                     num.parallel.tree = best.tune$num.parallel.tree,
                     objective = objective,
                     base.score = base.score)
      if (objective == "multi:softmax") params$num.class <- nclass
    } else if (booster == "dart") {
      params <- list(booster = booster,
                     sample.type = sample.type,
                     normalize.type = normalize.type,
                     rate.drop = rate.drop,
                     skip.drop = skip.drop,
                     silent = silent,
                     eta = eta,
                     gamma = gamma,
                     max.depth = max.depth,
                     min.child.weight = min.child.weight,
                     max.delta.step = max.delta.step,
                     subsample = subsample,
                     colsample.bytree = colsample.bytree,
                     colsample.bylevel = colsample.bylevel,
                     lambda = lambda,
                     alpha = alpha,
                     tree.method = tree.method,
                     sketch.eps = sketch.eps,
                     num.parallel.tree = num.parallel.tree,
                     objective = objective,
                     base.score = base.score,
                     nthread = nthread)
    } else {
      params <- list(booster = booster,
                     silent = silent,
                     lambda = best.tune$lambda,
                     alpha = best.tune$alpha,
                     lambda.bias = best.tune$lambda.bias,
                     objective = objective,
                     base.score = base.score)
      if (objective == "multi:softmax") params$num.class <- nclass
    }
    nrounds <- best.tune$best.nrounds
    if (verbose) parameterSummary(best.tune, title = "Tuning Results",
                                  newline.pre = TRUE)
  } else {

    # {{ NO GRID SEARCH NOR INTERNAL RESAMPLING }} ====
    res.part <- grid.performance <- grid.performance.by.tune.id <- best.tune <- NULL
    if (booster == "gbtree") {
      params <- list(booster = booster,
                     silent = silent,
                     eta = eta,
                     gamma = gamma,
                     max.depth = max.depth,
                     min.child.weight = min.child.weight,
                     max.delta.step = max.delta.step,
                     subsample = subsample,
                     colsample.bytree = colsample.bytree,
                     colsample.bylevel = colsample.bylevel,
                     lambda = lambda,
                     alpha = alpha,
                     tree.method = tree.method,
                     sketch.eps = sketch.eps,
                     num.parallel.tree = num.parallel.tree,
                     objective = objective,
                     base.score = base.score)
      if (objective == "multi:softmax") params$num.class <- nclass
    } else {
      params <- list(booster = booster,
                     silent = silent,
                     lambda = lambda,
                     alpha = alpha,
                     lambda.bias = lambda.bias,
                     objective = objective,
                     base.score = base.score)
      if (objective == "multi:softmax") params$num.class <- nclass
    }
  }

  # [ FULL XGBOOST ] ====
  if (verbose) msg("Training full XGB model with", booster, "booster...", newline.pre = TRUE)
  if (!is.null(objective)) objective <- deparse(substitute(objective))
  if (!is.null(feval)) feval <- deparse(substitute(feval))
  if (!is.null(force.nrounds)) nrounds <- force.nrounds
  mod <- xgboost::xgb.train(params,
                            xg.dat,
                            nrounds,
                            obj = obj,
                            feval = feval,
                            verbose = verbose,
                            print_every_n = print_every_n)

  # [ FITTED ] ====
  fitted <- predict(mod, xg.dat)
  if (type == "Classification") {
    # round() gives correct result whether response is integer or probability
    fitted.prob <- 1 - fitted
    fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
    levels(fitted) <- levels(y)
  } else {
    fitted.prob <- NULL
  }

  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    data.test <- xgboost::xgb.DMatrix(data = as.matrix(x.test), missing = missing)
    predicted <- predict(mod, data.test)
    if (type == "Classification") {
      predicted.prob <- 1 - predicted
      predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ RELATIVE INFLUENCE / VARIABLE IMPORTANCE ] ====
  .importance <- NULL
  # This may take a while
  if (importance) {
    if (verbose) msg("Estimating variable importance...")
    .importance <- xgboost::xgb.importance(model = mod, feature_names = colnames(x))
  }

  # [ OUTRO ] ====
  # sink(logOut, append = T, split = T)
  extra <- list(resampler = resampler,
                booster = booster,
                base.score = base.score,
                resamples = res.part,
                grid = grid,
                grid.run = if (save.res) grid.run else NULL,
                grid.performance = grid.performance,
                grid.performance.by.tune.id = grid.performance.by.tune.id,
                best.tune = best.tune,
                params = params,
                nrounds = nrounds,
                objective = objective,
                feval = feval)

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
                 fitted.prob = fitted.prob,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = NULL,
                 error.test = error.test,
                 varimp = .importance,
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

} # rtemis::s.XGB
