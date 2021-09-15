# elevate.R
# ::rtemis::
# 2016-8 E.D. Gennatas lambdamd.org
# add names Repeat1, etc. to resamples list

#' Tune, Train, and Test an \pkg{rtemis} Learner by Nested Resampling
#'
#' \code{elevate} is a high-level function to tune, train, and test an \pkg{rtemis} model
#' by nested resampling, with optional preprocessing and decomposition of input features
#'
#' - Note on resampling: You can never use an outer resampling method with replacement
#' if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the training and
#' testing sets of the inner resamples, leading to artificially decreased error.
#'
#' - If there is an error while running either the outer or inner resamples in parallel, the error
#' message returned by R will likely be unhelpful. Repeat the command after setting both inner
#' and outer resample run to use a single core, which should provide an informative message.
#' @inheritParams s.GLM
#' @inheritParams resample
#' @param mod Character: Learner to use. Options: \link{modSelect}
#' @param mod.params Optional named list of parameters to be passed to \code{mod}. All parameters can
#' be passed as part of \code{...} as well
#' @param .preprocess Optional named list of parameters to be passed to \link{preprocess}. Set using
#' \link{rtset.preprocess}, e.g. \code{decom = rtset.preprocess(impute = TRUE)}
#' @param .decompose Optional named list of parameters to be used for decomposition / dimensionality
#' reduction. Set using \link{rtset.decompose}, e.g. \code{decom = rtset.decompose("ica", 12)}
#' @param .resample Optional named list of parameters to be passed to \link{resample}.
#' NOTE: If set, this takes precedence over setting the individual resampling arguments ()
#' @param res.index List where each element is a vector of training set indices. Use this for manual or
#' precalculated train/test splits
#' @param res.group Integer, vector, length = length(y): Integer vector, where numbers define fold membership.
#' e.g. for 10-fold on a dataset with 1000 cases, you could use group = rep(1:10, each = 100)
#' @param n.repeats Integer: Number of times the external resample should be repeated. This allows you to do,
#' for example, 10 times 10-fold crossvalidation. Default = 1. In most cases it makes sense to use 1 repeat of
#' many resamples, e.g. 25 stratified subsamples,
#' @param stratify.var Numeric vector: Used to stratify external sampling (if applicable)
#'   Defaults to outcome \code{y}
#' @param x.name Character: Name of predictor dataset
#' @param y.name Character: Name of outcome
# #' @param save.data Logical: Save train, test, fitted, and predicted data for each resample.
# #'   Defaults to TRUE
#' @param cex Float: cex parameter for elevate plot
#' @param col Color for elevate plot
#' @param n.cores Integer: Number of cores to use. Default = 1. You are likely parallelizing either in the inner
#' (tuning) or the learner itself is parallelized. Don't parallelize the parallelization
#' @param parallel.type Character: "psock" (Default), "fork"
#' @param print.res.plot Logical: Print model performance plot for each resample.
#'   Defaults to FALSE
# @param yhat.plots Logical: Print aggregate plots for fitted vs. true and predicted vs. true
#'   from all resamples. Defaults to TRUE
# @param plot.mean Logical: Print plot of type \code{plot.type} plot of models' error. Defaults to TRUE
# @param plot.type Character: Type of plot to draw for all models' errors: "density" (Default), "histogram"
#' @param headless Logical: If TRUE, turn off all plotting.
#' @param outdir Character: Path where output should be saved
#' @param save.mods Logical: If TRUE, retain trained models in object, otherwise discard (save space
#' if running many resamples). Default = TRUE
#' @param save.tune Logical: If TRUE, save the best.tune data frame for each resample (output of gridSearchLearn)
#' @param save.rt Logical: If TRUE and \code{outdir} is set, save all models to \code{outdir}
#' @param save.plots Logical: If TRUE, save plots to outdir
#' @param bag.fitted Logical: If TRUE, use all models to also get a bagged prediction on the full sample. To get a
#' bagged prediction on new data using the same models, use \link{predict.rtModCV}
#' @param bag.fn Function to use to average prediction if \code{bag.fitted = TRUE}. Default = \code{median}
#' @param trace Integer: (Not really used) Print additional information if > 0. Default = 0
#' @param res.verbose Logical: Passed to \link{resLearn}, passed to each individual learner's \code{verbose} argument
#' @param save.res Logical: If TRUE, save the full output of each model trained on differents resamples under
#' subdirectories of \code{outdir}
#' @param ... Additional mod.params to be passed to learner (Will be concatenated with \code{mod.params}, so that you can use
#' either way to pass learner arguments)
#' @return Object of class \code{rtModCV} (Regression) or \code{rtModCVclass} (Classification)
#' \item{error.test.repeats}{the mean or aggregate error, as appropriate, for each repeat}
#' \item{error.test.repeats.mean}{the mean error of all repeats, i.e. the mean of \code{error.test.repeats}}
#' \item{error.test.repeats.sd}{if \code{n.repeats} > 1, the standard deviation of \code{error.test.repeats}}
#' \item{error.test.res}{the error for each resample, for each repeat}
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' # Regression
#' x <- rnormmat(100, 50)
#' w <- rnorm(50)
#' y <- x %*% w + rnorm(50)
#' mod <- elevate(x, y)
#' # Classification
#' data(Sonar, package = "mlbench")
#' mod <- elevate(Sonar)
#' }
#' @export

elevate <- function(x, y = NULL,
                    mod = "ranger",
                    mod.params = list(),
                    .preprocess = NULL,
                    .decompose = NULL,
                    .resample = NULL,
                    weights = NULL,
                    resampler = "strat.sub",
                    n.resamples = 10,
                    n.repeats = 1,
                    stratify.var = NULL,
                    train.p = .8,
                    strat.n.bins = 4,
                    target.length = NULL,
                    seed = NULL,
                    res.index = NULL,
                    res.group = NULL,
                    bag.fn = median,
                    x.name = NULL,
                    y.name = NULL,
                    save.mods = TRUE,
                    save.tune = TRUE,
                    # save.data = TRUE,
                    cex = 1.4,
                    col = "#18A3AC",
                    bag.fitted = FALSE,
                    n.cores = 1,
                    parallel.type = ifelse(.Platform$OS.type == "unix", "fork", "psock"),
                    print.plot = TRUE,
                    plot.fitted = FALSE,
                    plot.predicted = TRUE,
                    plot.theme = getOption("rt.fit.theme", "lightgrid"),
                    print.res.plot = FALSE,
                    # yhat.plots = TRUE,
                    # plot.mean = NULL,
                    # plot.type = "density",
                    question = NULL,
                    verbose = TRUE,
                    trace = 0,
                    res.verbose = FALSE,
                    headless = FALSE,
                    outdir = NULL,
                    save.plots = FALSE,
                    save.rt = ifelse(!is.null(outdir), TRUE, FALSE),
                    save.mod = TRUE,
                    save.res = FALSE, ...) {

  # Intro ====
  if (missing(x)) {
    cat("Usage:\n")
    print(args(elevate))
    invisible(9)
  }
  if (toupper(mod) == "KNN") stop("KNN is not supported by elevate")
  if (!is.null(outdir)) {
    outdir <- normalizePath(outdir, mustWork = FALSE)
    dir.create(outdir, showWarnings = FALSE)
  }

  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)

  # Dependencies ====
  if (!depCheck("plyr", "pbapply", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  # Allow elevate(df, "mod")
  # i.e single df with features and outcome followed by mod name
  if (is.character(y) & length(y) == 1) {
    mod <- y
    y <- NULL
  }
  mod.name <- toupper(mod)
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  # learner <- modSelect(mod, fn = FALSE)
  mod <- toupper(mod)
  if (headless) print.res.plot <- print.plot <- plot.mean <- yhat.plots <- FALSE
  if (!is.null(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  # If learner is multicore, run CV in series
  # Note: for mod "XGB" and "XGBLIN", only set n.cores > 1 if not using OpenMP

  # Override all resampler arguments if .resample given
  if (!is.null(.resample)) {
    resampler <- .resample$resampler
    n.resamples <- .resample$n.resamples
    stratify.var <- .resample$stratify.var
    train.p <- .resample$train.p
    strat.n.bins <- .resample$strat.n.bins
    target.length <- .resample$target.length
    seed <- .resample$seed
  }

  # Combine mod.params with (...)
  extraArgs <- list(...)
  mod.params <- c(mod.params, extraArgs)

  if (mod == "GBM") {
    if (!is.null(mod.params$n.cores)) {
      if (verbose) msg("Warning: using GBM with n.cores > 1, setting n.cores for resLearn to 1")
      if (mod.params$n.cores > 1) n.cores <- 1
    } else {
      n.cores <- 1
    }
  } else if (mod == "H2OGBM" | mod == "H2ORF" | mod == "H2OGLM" | mod == "H2ODL" | mod == "XGB" |
             mod == "XGBLIN" | mod == "LGB") {
    if (verbose) msg("Warning: using", mod, "- n.cores for resamples set to 1")
    n.cores <- 1
  }
  # if(!verbose) res.verbose <- FALSE
  # if (is.null(res.verbose)) res.verbose <- ifelse(n.cores > 1, FALSE, TRUE)

  if (save.rt & is.null(outdir)) outdir <- paste0("./elevate.", mod)

  # Plot mean if not kfold, loocv
  # if (is.null(plot.mean)) {
  #   plot.mean <- if (resampler %in% c("kfold", "loocv")) FALSE else TRUE
  # }
  if (.Platform$OS.type == "windows" & parallel.type == "fork") {
    warning("Forking is not possible in Windows, using PSOCK cluster instead")
    parallel.type <- "psock"
  }

  # Data ====
  dt <- dataPrepare(x, y, NULL, NULL)
  x <- dt$x
  y <- dt$y
  xnames <- dt$xnames
  type <- dt$type
  if (verbose) dataSummary(x, y, type = type, testSet = FALSE)
  # if (verbose & length(mod.params) > 0) parameterSummary(mod.params, title = paste(mod, "Parameters"))
  if (type == "Classification") {
    nclasses <- length(levels(y))
    if (nclasses != length(unique(y))) warning("elevate: Outcome contains empty classes")
  }

  # Decompose ====
  if (!is.null(.decompose)) {
    decomposer <- decomSelect(.decompose$decom)
    x <- do.call(decomposer, c(list(x = x), .decompose[-1], verbose = verbose))$projections.train
    if (verbose) dataSummary(x, y, type = type, testSet = FALSE)
  }

  # outdir ====
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
    if (!dir.exists(outdir)) dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    if (verbose) cat("Output directory set to", outdir, "\n")
    filename <- paste0(outdir, "elevate.", mod.name, ".rds")
  }

  # resLearn ====
  resample.rtset = list(resampler = resampler,
                        n.resamples = n.resamples,
                        stratify.var = stratify.var,
                        train.p = train.p,
                        strat.n.bins = strat.n.bins,
                        target.length = target.length,
                        seed = seed,
                        group = res.group,
                        index = res.index)
  if (n.cores > 1) print.res.plot <- FALSE
  # nres <- if (resampler == "loocv") NROW(y) else n.resamples
  # if (verbose) msg("Cross-validating ", mod.name, " ", type, " with ", nres,
  #                  " resamples on ", n.cores, ifelse(n.cores == 1, " core", " cores"), "...", sep = "")
  # if (verbose) msg("Training", mod.name, "on", nres, "resamples...")
  if (!is.null(logFile) & trace < 2) sink() # pause writing to file
  res.outdir <- if (save.res) outdir else NULL
  res.run <- mods <- res <- list()
  if (save.tune) best.tune <- list()
  if (trace > 1) msg("Starting resLearn")
  for (i in seq(n.repeats)) {
    res.run[[i]] <- resLearn(x = x, y = y,
                             mod = mod.name,
                             resample.rtset = resample.rtset,
                             weights = weights,
                             params = mod.params,
                             .preprocess = .preprocess,
                             verbose = verbose,
                             res.verbose = res.verbose,
                             trace = trace,
                             save.mods = save.mods,
                             outdir = res.outdir,
                             n.cores = n.cores,
                             parallel.type = parallel.type)
    mods[[i]] <- res.run[[i]]$mods
    res[[i]] <- res.run[[i]]$res
    if (save.tune) {
      best.tune[[i]] <- plyr::ldply(mods[[i]], function(r) r$mod1$extra$gridSearch$best.tune)
      if (NROW(best.tune[[i]]) > 0) colnames(best.tune[[i]])[1] <- "Resample"
    }
  }

  # Get resample parameters (added for res.groups and res.index)
  n.resamples <- attr(res.run[[1]]$res, "N")
  resampler <- attr(res.run[[1]]$res, "type")

  # nres <- length(res)
  if (!is.null(logFile) & trace < 2) sink(logFile, append = TRUE, split = verbose) # Resume writing to log
  names(mods) <- paste0("elevate.", mod.name, ".repeat", seq(mods))

  # Res fitted  ====
  # The fitted values and training error from each resample
  y.train.res <- lapply(seq(n.repeats), function(n)
    lapply(mods[[n]], function(m) m$mod1$y.train))
  fitted.res <- lapply(seq(n.repeats), function(n)
    lapply(mods[[n]], function(m) m$mod1$fitted))
  names(y.train.res) <- names(fitted.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))

  if (resampler != "loocv") {
    # Only if not LOOCV
    if (type == "Classification") {
      error.train.res <- lapply(seq(n.repeats), function(n)
        plyr::ldply(mods[[n]],
                    function(m) as.data.frame(m$mod1$error.train$Overall),
                    .id = NULL))
      names(error.train.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
    } else {
      error.train.res <- lapply(seq(n.repeats), function(n)
        plyr::ldply(mods[[n]], function(m) m$mod1$error.train, .id = NULL))
      names(error.train.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
    }
  } else {
    # LOOCV
    error.train.res <- error.train.res.mean <- NULL
  }

  if (!is.null(error.train.res)) {
    error.train.res.mean <- lapply(seq(n.repeats), function(n)
      as.data.frame(t(colMeans(error.train.res[[n]]))))
    names(error.train.res.mean) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
  }
  if (type == "Classification") {
    error.train.res.aggr <- lapply(seq(n.repeats), function(n)
      modError(unlist(y.train.res[[n]]), unlist(fitted.res[[n]]))$Overall)
  } else {
    error.train.res.aggr <- lapply(seq(n.repeats), function(n)
      modError(unlist(y.train.res[[n]]), unlist(fitted.res[[n]])))
  }
  names(error.train.res.aggr) <- paste0("elevate.", mod.name, ".repeat", seq(mods))

  # if (verbose) {
  #   for (i in seq(n.repeats)) {
  #     if (resampler == "kfold" | resampler == "loocv") {
  #       errorSummary(error.train.res.aggr[[i]], mod.name, pre = paste0("Repeat #", i ," Aggregated Train"))
  #     } else {
  #       errorSummary(error.train.res.mean[[i]], mod.name, pre = paste0("Repeat #", i ," Mean Train"))
  #     }
  #   }
  # }

  # Res predicted ====
  # The predicted values and testing error from each resample
  y.test.res <- lapply(seq(n.repeats), function(n)
    lapply(mods[[n]], function(m) m$mod1$y.test))
  names(y.test.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
  predicted.res <- lapply(seq(n.repeats), function(n)
    lapply(mods[[n]], function(m) m$mod1$predicted))
  names(predicted.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))

  # Only if not LOOCV
  if (resampler != "loocv") {
    if (type == "Classification") {
      error.test.res <- lapply(seq(n.repeats), function(n)
        plyr::ldply(mods[[n]],
                    function(m) as.data.frame(m$mod1$error.test$Overall),
                    .id = NULL))
      names(error.test.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
    } else {
      error.test.res <- lapply(seq(n.repeats), function(n)
        plyr::ldply(mods[[n]], function(m) m$mod1$error.test, .id = NULL))
      names(error.test.res) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
    }
  } else {
    # LOOCV
    error.test.res <- error.test.res.mean <- NULL
  }

  if (!is.null(error.test.res)) {
    error.test.res.mean <- lapply(seq(n.repeats), function(n)
      data.frame(t(colMeans(error.test.res[[n]]))))
    names(error.test.res.mean) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
  }

  if (type == "Classification") {
    error.test.res.aggr <- lapply(seq(n.repeats), function(n)
      modError(unlist(y.test.res[[n]]), unlist(predicted.res[[n]]))$Overall)
  } else {
    error.test.res.aggr <- lapply(seq(n.repeats), function(n)
      modError(unlist(y.test.res[[n]]), unlist(predicted.res[[n]])))
  }
  names(error.test.res.aggr) <- paste0("elevate.", mod.name, ".repeat", seq(mods))

  # Exclude or TODO: fix for different resamplers
  # if (verbose) {
  #   for (i in seq(n.repeats)) {
  #     errorSummary(error.test.res.aggr[[i]], mod.name, pre = paste0("Repeat #", i ," Aggregated Test"))
  #   }
  # }

  # Mean repeats error ====
  if (resampler == "loocv" | resampler == "kfold") {
    error.train.repeats <- plyr::ldply(error.train.res.aggr, .id = NULL)
    error.test.repeats <- plyr::ldply(error.test.res.aggr, .id = NULL)
  } else {
    error.train.repeats <- plyr::ldply(error.train.res.mean, .id = NULL)
    error.test.repeats <- plyr::ldply(error.test.res.mean, .id = NULL)
  }
  rownames(error.train.repeats) <- paste0("repeat", seq(n.repeats))
  error.train.repeats.mean <- data.frame(t(colMeans(error.train.repeats)))
  error.test.repeats.mean <- data.frame(t(colMeans(error.test.repeats)))
  error.train.repeats.sd <- data.frame(t(apply(error.train.repeats, 2, sd)))
  error.test.repeats.sd <- data.frame(t(apply(error.test.repeats, 2, sd)))

  # Bag fitted ====
  # mod.res used for bagging
  fitted.bag <- error.bag <- NULL
  if (bag.fitted) {
    fitted.bag <- lapply(mods, function(i) sapply(i, function(j) as.numeric(predict(j$mod1, x))))
    if (type == "Classification") {
      fitted.bag <- lapply(fitted.bag, function(i) apply(i, 1, function(j) round(bag.fn(j))))
      fitted.bag <- lapply(fitted.bag, function(i) levels(y)[i])
    } else {
      fitted.bag <- lapply(fitted.bag, function(i) apply(i, 1, bag.fn))
    }
    error.bag <- lapply(fitted.bag, function(i) modError(y, i))
  }

  # Summary ====
  if (verbose) {
    boxcat("elevate " %+% rtHighlight$bold(mod.name), pad = 0)
    cat("   N repeats = " %+% rtHighlight$bold(n.repeats), "\n")
    cat("   N resamples = " %+% rtHighlight$bold(n.resamples), "\n")
    cat("   Resampler = " %+% rtHighlight$bold(resampler), "\n")

    # If you LOOCV or KFOLD, report error of aggregate left-out sets,
    # otherwise mean error across test sets
    if (resampler == "loocv" | resampler == "kfold") {
      if (type == "Regression") {
        cat("   MSE of ", n.resamples, " aggregated test sets in each repeat = ",
            rtHighlight$bold(paste(ddSci(plyr::laply(error.test.res.aggr, function(x) x$MSE)),
                  collapse = ", ")), "\n", sep = "")
        cat("   MSE reduction in each repeat = ",
            rtHighlight$bold(paste(ddSci(plyr::laply(error.test.res.aggr, function(x) x$MSE.RED) * 100),
                  collapse = "%, "), "%\n", sep = ""))
      } else {
        cat("   Balanced Accuracy of ", n.resamples, " aggregated test sets in each repeat = ",
            rtHighlight$bold(paste(ddSci(plyr::laply(error.test.res.aggr,
                                    function(x) x$`Balanced Accuracy`)),
                  collapse = ", ")), "\n", sep = "")
      }
    } else {
      # strat.sub, strat.boot, bootstrap
      if (type == "Regression") {
        cat("   Mean MSE of ", n.resamples, " resamples in each repeat = ",
            rtHighlight$bold(paste(ddSci(plyr::laply(error.test.res.mean, function(x) x$MSE)),
                  collapse = ", ")), "\n", sep = "")
        cat("   Mean MSE reduction in each repeat = ",
            rtHighlight$bold(paste(ddSci(plyr::laply(error.test.res.mean, function(x) x$MSE.RED * 100)),
                  collapse = "%, "), "%\n", sep = ""))
      } else {
        cat("   Mean Balanced Accuracy of ", n.resamples, " test sets in each repeat = ",
            rtHighlight$bold(paste(ddSci(plyr::laply(error.test.res.mean, function(x) x$`Balanced.Accuracy`)),
                  collapse = ", ")), "\n", sep = "")
      }
    }

    # n.repeats mean
    if (n.repeats > 1) {
      if (type == "Regression") {
        cat("   Mean MSE across", n.repeats, "repeats =",
            rtHighlight$bold(ddSci(error.test.repeats.mean$MSE)), "\n")
        cat("   MSE was reduced on average by",
            rtHighlight$bold(ddSci(error.test.repeats.mean$MSE.RED)), "\n")
      } else if (type == "Classification") {
        cat("   Mean Balanced Accuracy across", n.repeats, "repeats =",
            rtHighlight$bold(ddSci(error.test.repeats.mean$`Balanced.Accuracy`)), "\n")
      }
    }
  }

  # Outro ====
  if (type == "Classification") {
    y.train.res.aggr <- lapply(seq(n.repeats), function(i)
      factor(c(y.train.res[[i]], recursive = TRUE, use.names = FALSE)))
    y.test.res.aggr <- lapply(seq(n.repeats), function(i)
      factor(c(y.test.res, recursive = TRUE, use.names = FALSE)))
    fitted.res.aggr <- lapply(seq(n.repeats), function(i)
      factor(c(fitted.res, recursive = TRUE, use.names = FALSE)))
    predicted.res.aggr <- lapply(seq(n.repeats), function(i)
      factor(c(predicted.res, recursive = TRUE, use.names = FALSE)))
    for (i in seq(n.repeats)) {
      levels(y.train.res.aggr[[i]]) <- levels(y.test.res.aggr[[i]]) <-
        levels(predicted.res.aggr[[i]]) <- levels(y)
    }

    fitted.prob.aggr <- plyr::llply(mods, function(i)
      unlist(plyr::llply(i, function(j) j$mod1$fitted.prob), use.names = FALSE))
    predicted.prob.aggr <- plyr::llply(mods, function(i)
      unlist(plyr::llply(i, function(j) j$mod1$predicted.prob), use.names = FALSE))
    names(y.train.res.aggr) <- names(y.test.res.aggr) <-
      names(predicted.res.aggr) <- names(fitted.res.aggr) <-
      names(fitted.prob.aggr) <- names(predicted.prob.aggr) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
  } else {
    y.train.res.aggr <- lapply(seq(n.repeats), function(i)
      c(y.train.res[[i]], recursive = TRUE, use.names = FALSE))
    y.test.res.aggr <- lapply(seq(n.repeats), function(i)
      c(y.test.res[[i]], recursive = TRUE, use.names = FALSE))
    fitted.res.aggr <- lapply(seq(n.repeats), function(i)
      c(fitted.res[[i]], recursive = TRUE, use.names = FALSE))
    predicted.res.aggr <- lapply(seq(n.repeats), function(i)
      c(predicted.res[[i]], recursive = TRUE, use.names = FALSE))
    fitted.prob.aggr <- predicted.prob.aggr <- NULL
    names(y.train.res.aggr) <- names(y.test.res.aggr) <-
      names(predicted.res.aggr) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
  }

  if (!save.tune) {
    best.tune <- NULL
  } else {
    names(best.tune) <- paste0("elevate.", mod.name, ".repeat", seq(mods))
  }

  # Variable importance ====
  varimp <- plyr::llply(mods, function(r) {
    .vi <- plyr::ldply(r, function(n) t(n$mod1$varimp), .id = NULL)
    rownames(.vi) <- names(r)
    .vi
  })

  if (type == "Classification") {
    rt <- rtModCVclass$new(mod = mods,
                           mod.name = mod.name,
                           type = type,
                           y.train = y,
                           x.name = x.name,
                           y.name = y.name,
                           xnames = xnames,
                           parameters = list(preprocess = .preprocess,
                                             decompose = .decompose,
                                             mod.params = mod.params,
                                             best.tune = best.tune),
                           n.repeats = n.repeats,
                           resampler.params = list(resampler = resampler,
                                                   n.resamples = n.resamples,
                                                   stratify.var = stratify.var,
                                                   train.p = train.p,
                                                   strat.n.bins = strat.n.bins,
                                                   target.length = target.length,
                                                   seed = seed),
                           resamples = res,
                           y.train.res = y.train.res,
                           y.train.res.aggr = y.train.res.aggr,
                           fitted.res = fitted.res,
                           fitted.res.aggr = fitted.res.aggr,
                           fitted.prob.aggr = fitted.prob.aggr,
                           error.train.res = error.train.res,
                           error.train.res.mean = error.train.res.mean,
                           error.train.res.aggr = error.train.res.aggr,
                           error.train.repeats = error.train.repeats,
                           error.train.repeats.mean = error.train.repeats.mean,
                           error.train.repeats.sd = error.train.repeats.sd,
                           y.test.res = y.test.res,
                           y.test.res.aggr = y.test.res.aggr,
                           predicted.res = predicted.res,
                           predicted.res.aggr = predicted.res.aggr,
                           predicted.prob.aggr = predicted.prob.aggr,
                           error.test.res = error.test.res,
                           error.test.res.mean = error.test.res.mean,
                           error.test.res.aggr = error.test.res.aggr,
                           error.test.repeats = error.test.repeats,
                           error.test.repeats.mean = error.test.repeats.mean,
                           error.test.repeats.sd = error.test.repeats.sd,
                           fitted.bag = fitted.bag,
                           error.bag = error.bag,
                           varimp = varimp,
                           question = question)
  } else {
    # Not Classification
    rt <- rtModCV$new(mod = mods,
                      mod.name = mod.name,
                      type = type,
                      y.train = y,
                      x.name = x.name,
                      y.name = y.name,
                      xnames = xnames,
                      parameters = list(preprocess = .preprocess,
                                        decompose = .decompose,
                                        mod.params = mod.params,
                                        best.tune = best.tune),
                      n.repeats = n.repeats,
                      resampler.params = list(resampler = resampler,
                                              n.resamples = n.resamples,
                                              stratify.var = stratify.var,
                                              train.p = train.p,
                                              strat.n.bins = strat.n.bins,
                                              target.length = target.length,
                                              seed = seed),
                      resamples = res,
                      y.train.res = y.train.res,
                      y.train.res.aggr = y.train.res.aggr,
                      fitted.res = fitted.res,
                      fitted.res.aggr = fitted.res.aggr,
                      error.train.res = error.train.res,
                      error.train.res.mean = error.train.res.mean,
                      error.train.res.aggr = error.train.res.aggr,
                      error.train.repeats = error.train.repeats,
                      error.train.repeats.mean = error.train.repeats.mean,
                      error.train.repeats.sd = error.train.repeats.sd,
                      y.test.res = y.test.res,
                      y.test.res.aggr = y.test.res.aggr,
                      predicted.res = predicted.res,
                      predicted.res.aggr = predicted.res.aggr,
                      error.test.res = error.test.res,
                      error.test.res.mean = error.test.res.mean,
                      error.test.res.aggr = error.test.res.aggr,
                      error.test.repeats = error.test.repeats,
                      error.test.repeats.mean = error.test.repeats.mean,
                      error.test.repeats.sd = error.test.repeats.sd,
                      varimp = varimp,
                      question = question)
  }

  if (!save.mod) rt$mod <- NA
  if (save.rt) rtSave(rt, outdir, file.prefix = "elevate.", verbose = verbose)
  if (print.plot) {
    if (plot.fitted) rt$plotFitted()
    if (plot.predicted) rt$plotPredicted()
  }
  if (!is.null(outdir)) {
    rt$plotFitted(filename = paste0(outdir, "elevate.", mod.name,"_fitted.pdf"))
    rt$plotPredicted(filename = paste0(outdir, "elevate.", mod.name, "_predicted.pdf"))
  }
  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  return(rt)

} # rtemis::elevate
