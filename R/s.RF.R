# s.RF.R
# ::rtemis::
# 2016-8 E.D. Gennatas lambdamd.org

#' Random Forest Classification and Regression [C, R]
#'
#' Train a Random Forest for regression or classification using \code{randomForest}
#'
#' If \code{autotue = TRUE}, \code{randomForest::tuneRF} will be run to determine best \code{mtry}
#'   value.
#'
#' @inheritParams s.GLM
#' @inheritParams s.CART
#' @param n.trees Integer: Number of trees to grow. Default = 1000
#' @param autotune Logical: If TRUE, use \code{randomForest::tuneRF} to determine \code{mtry}
#' @param n.trees.try Integer: Number of trees to train for tuning, if \code{autotune = TRUE}
#' @param stepFactor Float: If \code{autotune = TRUE}, at each tuning iteration, \code{mtry} is multiplied or
#' divided by this value. Default = 1.5
#' @param mtryStart Integer: If \code{autotune = TRUE}, start at this value for \code{mtry}
#' @param mtry [gS] Integer: Number of features sampled randomly at each split
#' @param nodesize [gS]: Integer: Minimum size of terminal nodes. Default = 5 (Regression);
#' 1 (Classification)
#' @param maxnodes [gS]: Integer: Maximum number of terminal nodes in a tree. Default = NULL; trees
#' grown to maximum possible
#' @param replace Logical: If TRUE, sample cases with replacement during training. Default = TRUE
#' @param classwt Vector, Float: Priors of the classes for classification only. Need not add up to 1
#' @param upsample Logical: If TRUE, upsample training set cases not belonging in majority outcome
#' group
#' @param strata Vector, Factor: Will be used for stratified sampling
#' @param outdir String, Optional: Path to directory to save output
#' @param sampsize Integer: Size of sample to draw. In Classification, if \code{strata} is defined, this
#' can be a vector of the same length, in which case, corresponding values determine how many cases are drawn from
#' the strata.
#' @param sampsize.ratio Float (0, 1): Heuristic of sorts to increase sensitivity in unbalanced
#' cases. Sample with replacement from minority case to create bootstraps of length N cases.
#' Select \code{(sampsize.ratio * N minority cases)} cases from majority class.
# @param importance Logical: If TRUE, estimate variable relative importance. Default = TRUE
#' @param proximity Logical: If TRUE, calculate proximity measure among cases. Default = FALSE
#' @param do.trace Logical or integer: If TRUE, \code{randomForest} will outpout information while it is running.
#' If an integer, \code{randomForest} will report progress every this many trees. Default = \code{n.trees/10} if
#' \code{verbose = TRUE}
#' @param tune.do.trace Same as \code{do.trace} but for tuning, if \code{autotune = TRUE}
#' @param imetrics Logical: If TRUE, calculate interpretability metrics (N of trees and N of nodes) and save under
#' the 'extra' field of \link{rtMod}
#' @param print.tune.plot Logical: passed to \code{randomForest::tuneRF}. Default = FALSE
#' @param proximity.tsne Logical: If TRUE, perform t-SNE on proximity matrix. Will be saved under 'extra' field of
#' \link{rtMod}. Default = FALSE
#' @param discard.forest Logical: If TRUE, remove forest from \link{rtMod} object to save space.
#' Default = FALSE
#' @param tsne.perplexity Numeric: Perplexity parameter for \code{Rtsne::Rtsne}
#' @param plot.tsne.train Logical: If TRUE, plot training set tSNE projections
#' @param plot.tsne.test Logical: If TRUE, plot testing set tSNE projections
#' @param ... Additional arguments to be passed to \code{randomForest::randomForest}
#' @return \link{rtMod} object
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Ensembles
#' @export

s.RF <- function(x, y = NULL,
                 x.test = NULL, y.test = NULL,
                 x.name = NULL, y.name = NULL,
                 n.trees = 1000,
                 autotune = FALSE,
                 n.trees.try = 1000,
                 stepFactor = 1.5,
                 mtry = NULL,
                 mtryStart = mtry,
                 grid.resample.rtset = rtset.resample("kfold", 5),
                 metric = NULL,
                 maximize = NULL,
                 classwt = NULL,
                 ipw = TRUE,
                 ipw.type = 2,
                 upsample = FALSE,
                 downsample = FALSE,
                 resample.seed = NULL,
                 importance = TRUE,
                 proximity = FALSE,
                 replace = TRUE,
                 nodesize = NULL,
                 maxnodes = NULL,
                 strata = NULL,
                 sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                 sampsize.ratio = NULL,
                 do.trace = NULL,
                 tune.do.trace = FALSE,
                 imetrics = FALSE,
                 n.cores = rtCores,
                 print.tune.plot = FALSE,
                 print.plot = TRUE,
                 plot.fitted = NULL,
                 plot.predicted = NULL,
                 plot.theme = getOption("rt.fit.theme", "lightgrid"),
                 proximity.tsne = FALSE,
                 discard.forest = FALSE,
                 tsne.perplexity = 5,
                 plot.tsne.train = FALSE,
                 plot.tsne.test = FALSE,
                 question = NULL,
                 rtclass = NULL,
                 verbose = TRUE,
                 grid.verbose = TRUE,
                 outdir = NULL,
                 save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ Intro ] ====
  if (missing(x)) {
    print(args(s.RF))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "RF"

  # [ Dependencies ] ====
  if (!depCheck("randomForest", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.RF))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (is.null(do.trace)) do.trace <- if (verbose) n.trees/10 else FALSE
  if (proximity.tsne) proximity <- TRUE

  # [ Data ] ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .classwt <- if (is.null(classwt) & ipw) dt$class.weights else classwt
  x0 <- if (upsample|downsample) dt$x0 else x
  y0 <- if (upsample|downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(n.trees, mtry, pad = 4, newline.pre = TRUE)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  if (is.null(mtry)) {
    mtry <- if (type == "Classification") floor(sqrt(NCOL(x))) else max(floor(NCOL(x)/3), 1)
  }

  if (is.null(nodesize)) {
    nodesize <- if (type == "Classification") 1 else 5
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

  # [ Grid Search ] ====
  if (gridCheck(mtry, nodesize, maxnodes, sampsize.ratio)) {
    gs <- gridSearchLearn(x0, y0,
                          mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(mtry = mtry,
                                             nodesize = nodesize,
                                             maxnodes = maxnodes,
                                             sampsize.ratio = sampsize.ratio),
                          fixed.params = list(ntree = n.trees,
                                              classwt = classwt,
                                              replace = replace,
                                              importance = FALSE,
                                              proximity = FALSE,
                                              strata = strata,
                                              na.action = na.exclude,
                                              do.trace = do.trace),
                          metric = metric,
                          maximize = maximize,
                          verbose = grid.verbose,
                          n.cores = n.cores)
    mtry <- gs$best.tune$mtry
    nodesize <- gs$best.tune$nodesize
    maxnodes <- gs$best.tune$maxnodes
    sampsize.ratio <- gs$best.tune$sampsize.ratio
  } else {
    gs <- NULL
  }

  # In case tuning fails, use defaults
  if (length(mtry) == 0) {
    warning("Tuning failed; setting mtry to default")
    mtry <- if (type == "Classification") floor(sqrt(NCOL(x))) else max(floor(NCOL(x)/3), 1)
  }

  if (length(nodesize) == 0) {
    warning("Tuning failed; setting nodesize to default")
    nodesize <- if (type == "Classification") 1 else 5
  }

  # maxnodes and sampsize.ratio can be NULL

  # [ SAMPSIZE.RATIO ] ====
  # sampsize.ratio
  if (!is.null(sampsize.ratio) && length(sampsize.ratio) == 1) {
    strata <- y
    # Get frequency of minority class
    freq.minority <- min(table(y))
    sampsize <- c(freq.minority, freq.minority * sampsize.ratio)
    if (verbose) msg("sampsize set to", sampsize)
  }

  # [ tuneRF ] ====
  # This is an alternative to doing our own gridsearch for mtry, use randomForest's own tuning
  # function for mtry
  if (autotune) {
    if (verbose) msg("Tuning for mtry...")
    tuner <- randomForest::tuneRF(x = x, y = y,
                                  mtryStart = mtryStart,
                                  ntreeTry = n.trees.try,
                                  stepFactor = stepFactor,
                                  trace = verbose,
                                  plot = print.tune.plot,
                                  strata = strata,
                                  do.trace = tune.do.trace,
                                  classwt = .classwt)
    mtry <- tuner[which.min(tuner[, 2]), 1]
    if (verbose) msg("Best mtry :", mtry)
  }

  # [ RF ] ====
  if (verbose) msg("Training Random Forest", type, "with", n.trees, "trees...",
                   newline.pre = TRUE)
  mod <- randomForest::randomForest(x = x, y = y,
                                    ntree = n.trees,
                                    mtry = mtry,
                                    classwt = .classwt,
                                    replace = replace,
                                    nodesize = nodesize,
                                    maxnodes = maxnodes,
                                    importance = FALSE,
                                    proximity = proximity,
                                    strata = strata,
                                    sampsize = sampsize,
                                    na.action = na.exclude,
                                    do.trace = do.trace)

  # [ Fitted ] ====
  if (proximity) {
    fit <- predict(mod, x, proximity = TRUE)
    fitted <- fit$predicted
    proximity.train <- fit$proximity
  } else {
    fitted <- predict(mod, x)
    proximity.train <- NULL
  }
  attr(fitted, "names") <- NULL
  fitted.prob <- if (type == "Classification") predict(mod, x, "prob") else NULL
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ Predicted ] ====
  if (!is.null(x.test)) {
    if (proximity) {
      pred <- predict(mod, x.test, proximity = proximity)
      predicted <- pred$predicted
      proximity.test <- pred$proximity
    } else {
      predicted <- predict(mod, x.test)
      proximity.test <- NULL
    }
    if (type == "Regression") predicted <- as.numeric(predicted)
    predicted.prob <- if (type == "Classification") predict(mod, x.test, "prob") else NULL
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- predicted.prob <- error.test <- proximity.test <- NULL
  }

  # Discard forest to save memory
  if (discard.forest) mod$forest <- NULL

  # [ PROXIMITY T-SNE ] ====
  # Add failsafe
  if (proximity.tsne & type == "Classification") {
    cat("Running t-SNE on 1-proximity\n")
    rf.tsne.train <- Rtsne::Rtsne(X = 1 - mod$proximity,
                                  is_distance = T,
                                  dims = 2, verbose = T,
                                  perplexity = tsne.perplexity)
    proximity.tsne.train <- rf.tsne.train
    par(pty = "s")
    if (plot.tsne.train) {
      plot(x = rf.tsne.train$Y[, 1], y = rf.tsne.train$Y[, 2],
           cex = 1.5, asp = 1,
           col = c("red", "green", "blue")[as.numeric(y)],
           main = "t-SNE of Proximity Based on Random Forest\n(training set)",
           xlab = "t-SNE projection #1", ylab = "t-SNE projection #2")
    }

    if (!is.null(x.test)) {
      rf.tsne.test <- Rtsne::Rtsne(X = 1 - proximity.test, is_distance = T, dims = 2, verbose = T,
                                   perplexity = tsne.perplexity)
      proximity.tsne.test <- rf.tsne.test
      par(pty = "s")
      if (plot.tsne.test) {
        plot(x = rf.tsne.test$Y[, 1], y = rf.tsne.test$Y[, 2],
             cex = 1.5, asp = 1,
             col = c("red", "green", "blue", "orange", "purple")[as.numeric(y.test)],
             main = "t-SNE of Proximity Based on Random Forest\n(testing set)",
             xlab = "t-SNE projection #1", ylab = "t-SNE projection #2")
      }
    }
  } else {
    proximity.tsne.train <- NULL
    proximity.tsne.test <- NULL
  } # End t-SNE

  # [ Outro ] ====
  extra <- list(gridSearch = gs,
                sampsize = sampsize,
                fitted.prob = fitted.prob,
                predicted.prob = predicted.prob)
  if (!is.null(proximity.train)) extra$proximity.train <- proximity.train
  if (!is.null(proximity.test)) extra$proximity.test <- proximity.test
  if (!is.null(proximity.tsne.train)) extra$proximity.tsne.train <- proximity.tsne.train
  if (!is.null(proximity.tsne.test)) extra$proximity.tsne.test <- proximity.tsne.test
  if (imetrics) {
    n.nodes <- sum(sapply(seq(n.trees), function(i) NROW(randomForest::getTree(mod, k = i))) * 2)
    extra$imetrics <- list(n.trees = n.trees, n.nodes = n.nodes)
  }
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
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 varimp = mod$importance, # for importance = FALSE, rf still returns a vector
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

} # rtemis::s.RF
