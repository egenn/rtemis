# train_cv.R
# ::rtemis::
# 2023 E.D. Gennatas rtemis.org

#' Tune, Train, and Test an \pkg{rtemis} Learner by Nested Resampling
#'
#' `train` is a high-level function to tune, train, and test an
#' \pkg{rtemis} model by nested resampling, with optional preprocessing and
#' decomposition of input features
#'
#' - Note on resampling: You should never use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and testing sets of the inner resamples, leading to underestimated
#' testing error.
#'
#' - If there is an error while running either the outer or inner resamples in
#' parallel, the error message returned by R will likely be unhelpful. Repeat
#' the command after setting both inner and outer resample run to use a single
#' core, which should provide an informative message.
#'
#' The `train` command is replacing `elevate`.
#' Note: specifying id.strat for the inner resampling is not yet supported.
#'
#' @inheritParams s_GLM
#' @inheritParams resample
#' @param alg Character: Learner to use. Options: [select_learn]
#' @param train.params Optional named list of parameters to be passed to
#' `alg`. All parameters can be passed as part of `...` as well
#' @param .preprocess Optional named list of parameters to be passed to
#' [preprocess]. Set using [setup.preprocess],
#' e.g. `decom = setup.preprocess(impute = TRUE)`
#' @param .decompose Optional named list of parameters to be used for
#' decomposition / dimensionality reduction. Set using [setup.decompose],
#' e.g. `decom = setup.decompose("ica", 12)`
#' @param n.repeats Integer: Number of times to repeat the outer resampling.
#' This was added for completeness, but in practice we use either k-fold
#' crossvalidation, e.g. 10-fold, especially in large samples, or a higher number of
#' stratified subsamples, e.g. 25, for smaller samples
#' @param outer.resampling List: Output of [setup.resample] to define outer
#' resampling scheme
#' @param inner.resampling List: Output of [setup.resample] to define inner
#' resampling scheme
#' @param x.name Character: Name of predictor dataset
#' @param y.name Character: Name of outcome
# #' @param save.data Logical: Save train, test, fitted, and predicted data for
# #'   each resample. Defaults to TRUE
#' @param outer.n.workers Integer: Number of cores to use for the outer i.e.
#' testing resamples. You are likely parallelizing either in the inner
#' (tuning) or the learner itself is parallelized. Don't parallelize the
#' parallelization
#' @param print.res.plot Logical: Print model performance plot for each
#' resample.
# @param yhat.plots Logical: Print aggregate plots for fitted vs. true and predicted vs. true
#'   from all resamples. Defaults to TRUE
# @param plot.mean Logical: Print plot of type \code{plot.type} plot of models' error.
# @param plot.type Character: Type of plot to draw for all models' errors: "density" (Default), "histogram"
#' @param headless Logical: If TRUE, turn off all plotting.
#' @param outdir Character: Path where output should be saved
#' @param save.mods Logical: If TRUE, retain trained models in object,
#' otherwise discard (save space if running many resamples).
#' @param save.tune Logical: If TRUE, save the best.tune data frame for each
#' resample (output of gridSearchLearn)
#' @param save.rt Logical: If TRUE and `outdir` is set, save all models to
#' `outdir`
#' @param save.plots Logical: If TRUE, save plots to outdir
#' @param bag.fitted Logical: If TRUE, use all models to also get a bagged
#' prediction on the full sample. To get a bagged prediction on new data using
#' the same models, use [predict.rtModCV]
#' @param bag.fn Function to use to average prediction if
#' `bag.fitted = TRUE`. Default = `median`
#' @param trace Integer: (Not really used) Print additional information if > 0.
#' @param res.verbose Logical: Passed to each individual learner's `verbose` argument
#' @param save.res Logical: If TRUE, save the full output of each model trained
#' on differents resamples under subdirectories of `outdir`
#' @param debug Logical: If TRUE, sets `outer.n.workers` to 1, `options(error=recover)`,
#' and options(warn = 2)
#' @param ... Additional train.params to be passed to learner. Will be concatenated
#' with `train.params`
#'
#' @return Object of class `rtModCV` (Regression) or
#' `rtModCVClass` (Classification)
#' \item{error.test.repeats}{the mean or aggregate error, as appropriate, for each repeat}
#' \item{error.test.repeats.mean}{the mean error of all repeats, i.e. the mean of `error.test.repeats`}
#' \item{error.test.repeats.sd}{if `n.repeats` > 1, the standard deviation of `error.test.repeats`}
#' \item{error.test.res}{the error for each resample, for each repeat}
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' # Regression
#'
#' x <- rnormmat(100, 50)
#' w <- rnorm(50)
#' y <- x %*% w + rnorm(50)
#' mod <- train(x, y)
#'
#' # Classification
#'
#' data(Sonar, package = "mlbench")
#' mod <- train(Sonar)
#' }
#' @export

train_cv <- function(
  x,
  y = NULL,
  alg = "ranger",
  train.params = list(),
  .preprocess = NULL,
  .decompose = NULL,
  weights = NULL,
  n.repeats = 1,
  outer.resampling = setup.resample(
    resampler = "strat.sub",
    n.resamples = 10
  ),
  inner.resampling = setup.resample(
    resampler = "kfold",
    n.resamples = 5
  ),
  bag.fn = median,
  x.name = NULL,
  y.name = NULL,
  save.mods = TRUE,
  save.tune = TRUE,
  bag.fitted = FALSE,
  # parallelize outer (testing) resamples
  outer.n.workers = 1,
  print.plot = FALSE,
  plot.fitted = FALSE,
  plot.predicted = TRUE,
  plot.theme = rtTheme,
  print.res.plot = FALSE,
  question = NULL,
  verbose = TRUE,
  res.verbose = FALSE,
  trace = 0,
  headless = FALSE,
  outdir = NULL,
  save.plots = FALSE,
  save.rt = ifelse(!is.null(outdir), TRUE, FALSE),
  save.mod = TRUE,
  save.res = FALSE,
  debug = FALSE,
  ...
) {
  # Intro ----
  .call <- match.call()
  # Make sure data is not substituted by list with entire raw data
  .call[2] <- list(str2lang("dat"))
  if (missing(x)) {
    cat("Usage:\n")
    print(args(train_cv))
    stop("x is missing: Please provide data")
  }

  # Support for rtemis_config file
  if (is.character(x) && length(x) == 1) {
    config_path <- path.expand(x)
    if (!file.exists(config_path)) {
      stop(
        "The 'x' you input was a character. ",
        "Assuming this is the path to a config file, but file not found: ",
        config_path
      )
    }
    config <- read_config(config_path)
    x <- read(config$data_path, character2factor = TRUE)
    if (length(config$target) > 0) {
      y <- x[[config$target]]
      x <- x[setdiff(names(x), config$target)]
    }
    alg <- config$alg
    train.params <- config$train.params
    inner.resampling <- config$inner.resampling
    outer.resampling <- config$outer.resampling
    outdir <- dirname(config$outdir)
  }

  if (!is.null(outer.resampling$id.strat)) {
    stopifnot(length(outer.resampling$id.strat) == NROW(x))
  }
  alg <- select_learn(alg, name = TRUE)
  if (toupper(alg) == "KNN") stop("KNN is not supported by train_cv")
  if (debug) {
    outer.n.workers <- 1
    train.params$n.cores <- 1
    error_og <- getOption("error")
    warn_og <- getOption("warn")
    options(error = recover)
    options(warn = 2)
    on.exit({
      options(error = error_og)
      options(warn = warn_og)
    })
  }
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (verbose) cat("Output directory set to", outdir, "\n")
  }

  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      sys.calls()[[1]][[1]],
      "_",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start_time <- intro(verbose = verbose, logFile = logFile)

  # Dependencies ----
  dependency_check(c("future", "future.apply", "progressr", "plyr"))

  # Arguments ----
  # Allow train_cv(df, "mod")
  # i.e single df with features and outcome followed by mod name
  if (is.character(y) && length(y) == 1) {
    mod <- y
    y <- NULL
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")

  if (headless) {
    print.res.plot <- print.plot <- plot.mean <- yhat.plots <- FALSE
  }

  # If learner is multicore, run CV in series
  # Note: for alg "XGB" and "XGBLIN", only set n.cores > 1 if not using OpenMP

  # Combine train.params with (...)
  train.params <- c(train.params, list(...))

  if (
    alg %in%
      c(
        "AddTree",
        "CART",
        "DN",
        "GBM",
        "GBM0",
        "GBM3",
        "GLMNET",
        "GLMTree",
        "H2OGBM",
        "LIHAD",
        "LINAD",
        "LINOA",
        "MARS",
        "POLYMARS",
        "PPR",
        "Ranger",
        "RF",
        "SPLS",
        "SVM",
        "XGBoost",
        "XRF"
      )
  ) {
    train.params <- c(
      train.params,
      list(grid.resample.params = inner.resampling)
    )
  }

  if (
    outer.n.workers > 1 &&
      alg %in%
        c(
          "H2OGBM",
          "H2ORF",
          "H2OGLM",
          "H2ODL",
          "XRF",
          "XGBoost",
          "XGBLIN",
          "LightGBM",
          "LightRF"
        )
  ) {
    if (verbose) msg2("Using", alg, "- outer.n.workers set to 1")
    outer.n.workers <- 1
  }
  if (!verbose) res.verbose <- FALSE
  if (save.rt && is.null(outdir)) outdir <- paste0("./CV_", alg)

  # Data ----
  dt <- prepare_data(x, y, NULL, NULL)
  x <- dt$x
  y <- dt$y
  xnames <- dt$xnames
  type <- dt$type
  if (verbose) dataSummary(x, y, type = type, testSet = FALSE)
  if (type == "Classification") {
    y <- droplevels(y)
    nclasses <- length(levels(y))
  }

  # Decompose ----
  if (!is.null(.decompose)) {
    decomposer <- select_decom(.decompose$decom)
    x <- do.call(
      decomposer,
      c(list(x = x), .decompose[-1], verbose = verbose)
    )$projections.train
    if (verbose) dataSummary(x, y, type = type, testSet = FALSE)
  }

  # resLearn: Outer resamples ----
  if (outer.n.workers > 1) print.res.plot <- FALSE
  if (!is.null(logFile) && trace < 2) sink() # pause writing to file
  res.outdir <- if (save.res) outdir else NULL
  res.run <- mods <- res <- list()
  if (save.tune) best.tune <- list()

  if (verbose) {
    desc <- switch(
      outer.resampling$resampler,
      kfold = "independent folds",
      strat.sub = "stratified subsamples",
      strat.boot = "stratified bootstraps",
      bootstrap = "bootstrap resamples",
      loocv = "independent folds (LOOCV)",
      "custom resamples"
    )
    msg2(
      hilite(
        paste0(
          "Training ",
          bold(select_learn(alg, desc = TRUE)),
          " on ",
          outer.resampling$n.resamples,
          " ",
          desc,
          "..."
        ),
        bold = FALSE
      ),
      newline.pre = TRUE
    )
  }

  # Loop through repeats (this is often set to one)
  for (i in seq_len(n.repeats)) {
    res.run[[i]] <- resLearn(
      x = x,
      y = y,
      alg = alg,
      resample.params = outer.resampling,
      weights = weights,
      params = train.params,
      .preprocess = .preprocess,
      verbose = verbose,
      res.verbose = res.verbose,
      trace = trace,
      save.mods = save.mods,
      outdir = res.outdir,
      n.workers = outer.n.workers
    )
    mods[[i]] <- res.run[[i]]$mods
    res[[i]] <- res.run[[i]]$res
    if (save.tune) {
      best.tune[[i]] <- plyr::ldply(
        mods[[i]],
        function(r) r$mod1$gridsearch$best.tune
      )
      if (NROW(best.tune[[i]]) > 0) colnames(best.tune[[i]])[1] <- "Resample"
    }
  }

  # Get resample parameters (added for res.groups and res.index)
  n.resamples <- attr(res.run[[1]]$res, "N")
  resampler <- attr(res.run[[1]]$res, "resampler")

  if (!is.null(logFile) && trace < 2)
    sink(logFile, append = TRUE, split = verbose) # Resume writing to log
  names(mods) <- paste0("CV_", alg, "_repeat", seq(mods))

  # Res fitted  ----
  # The fitted values and training error from each resample
  y.train.res <- lapply(seq(n.repeats), function(n) {
    lapply(mods[[n]], function(m) m$mod1$y.train)
  })
  fitted.res <- lapply(seq(n.repeats), function(n) {
    lapply(mods[[n]], function(m) m$mod1$fitted)
  })
  names(y.train.res) <- names(fitted.res) <- paste0(
    "CV_",
    alg,
    "_repeat",
    seq(mods)
  )

  if (resampler != "loocv") {
    # Only if not LOOCV
    if (type == "Classification") {
      error.train.res <- lapply(seq(n.repeats), function(n) {
        plyr::ldply(
          mods[[n]],
          function(m) as.data.frame(m$mod1$error.train$Overall),
          .id = NULL
        )
      })
      names(error.train.res) <- paste0("CV_", alg, "_repeat", seq(mods))
    } else {
      error.train.res <- lapply(seq(n.repeats), function(n) {
        plyr::ldply(mods[[n]], function(m) m$mod1$error.train, .id = NULL)
      })
      names(error.train.res) <- paste0("CV_", alg, "_repeat", seq(mods))
    }
  } else {
    # LOOCV
    error.train.res <- error.train.res.mean <- NULL
  }

  if (!is.null(error.train.res)) {
    error.train.res.mean <- lapply(seq(n.repeats), function(n) {
      as.data.frame(t(colMeans(error.train.res[[n]])))
    })
    names(error.train.res.mean) <- paste0("CV_", alg, "_repeat", seq(mods))
  }
  if (type == "Classification") {
    error.train.res.aggr <- lapply(seq(n.repeats), function(n) {
      mod_error(unlist(y.train.res[[n]]), unlist(fitted.res[[n]]))$Overall
    })
  } else {
    error.train.res.aggr <- lapply(seq(n.repeats), function(n) {
      mod_error(unlist(y.train.res[[n]]), unlist(fitted.res[[n]]))
    })
  }
  names(error.train.res.aggr) <- paste0("CV_", alg, "_repeat", seq(mods))

  # Res predicted ----
  # The predicted values and testing error from each resample
  y.test.res <- lapply(seq(n.repeats), function(n) {
    lapply(mods[[n]], function(m) m$mod1$y.test)
  })
  names(y.test.res) <- paste0("CV_", alg, "_repeat", seq(mods))
  predicted.res <- lapply(seq(n.repeats), function(n) {
    lapply(mods[[n]], function(m) m$mod1$predicted)
  })
  names(predicted.res) <- paste0("CV_", alg, "_repeat", seq(mods))

  # Only if not LOOCV
  if (resampler != "loocv") {
    if (type == "Classification") {
      error.test.res <- lapply(seq(n.repeats), function(n) {
        plyr::ldply(
          mods[[n]],
          function(m) as.data.frame(m$mod1$error.test$Overall),
          .id = NULL
        )
      })
      names(error.test.res) <- paste0("CV_", alg, "_repeat", seq(mods))
    } else {
      error.test.res <- lapply(seq(n.repeats), function(n) {
        plyr::ldply(mods[[n]], function(m) m$mod1$error.test, .id = NULL)
      })
      names(error.test.res) <- paste0("CV_", alg, "_repeat", seq(mods))
    }
  } else {
    # LOOCV
    error.test.res <- error.test.res.mean <- NULL
  }

  if (!is.null(error.test.res)) {
    error.test.res.mean <- lapply(seq(n.repeats), function(n) {
      data.frame(t(colMeans(error.test.res[[n]])))
    })
    names(error.test.res.mean) <- paste0("CV_", alg, "_repeat", seq(mods))
  }

  if (type == "Classification") {
    error.test.res.aggr <- lapply(seq(n.repeats), function(n) {
      mod_error(unlist(y.test.res[[n]]), unlist(predicted.res[[n]]))$Overall
    })
  } else {
    error.test.res.aggr <- lapply(seq(n.repeats), function(n) {
      mod_error(unlist(y.test.res[[n]]), unlist(predicted.res[[n]]))
    })
  }
  names(error.test.res.aggr) <- paste0("CV_", alg, "_repeat", seq(mods))

  # Mean repeats error ----
  if (resampler == "loocv" || resampler == "kfold") {
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

  # Bag fitted ----
  # mod.res used for bagging
  fitted.bag <- error.bag <- NULL
  if (bag.fitted) {
    fitted.bag <- lapply(mods, function(i) {
      sapply(i, function(j) as.numeric(predict(j$mod1, x)))
    })
    if (type == "Classification") {
      fitted.bag <- lapply(fitted.bag, function(i) {
        apply(i, 1, function(j) round(bag.fn(j)))
      })
      fitted.bag <- lapply(fitted.bag, function(i) {
        levels(y)[i]
      })
    } else {
      fitted.bag <- lapply(fitted.bag, function(i) {
        apply(i, 1, bag.fn)
      })
    }
    error.bag <- lapply(fitted.bag, function(i) mod_error(y, i))
  }

  # Summary ----
  if (verbose) {
    boxcat(paste("Cross-validated", hilite(alg)), newline.pre = TRUE, pad = 0)
    # cat("N repeats =", hilite(n.repeats), "\n")
    # cat("N resamples =", hilite(n.resamples), "\n")
    # cat("Resampler =", hilite(resampler), "\n")
    desc <- switch(
      resampler,
      kfold = "independent folds",
      strat.sub = "stratified subsamples",
      strat.boot = "stratified bootstraps",
      bootstrap = "bootstrap resamples",
      loocv = "independent folds (LOOCV)",
      "custom resamples"
    )

    # If using LOOCV or KFOLD, report error of aggregate left-out sets,
    # otherwise mean error across test sets
    if (resampler == "loocv" || resampler == "kfold") {
      if (type == "Regression") {
        cat(
          "MSE of ",
          n.resamples,
          " aggregated test sets",
          if (n.repeats > 1) " in each repeat ",
          ": ",
          hilite(paste(
            ddSci(plyr::laply(
              error.test.res.aggr,
              function(x) x$MSE
            )),
            collapse = ", "
          )),
          "\n",
          sep = ""
        )
        cat(
          "MSE reduction",
          if (n.repeats > 1) " in each repeat",
          ": ",
          hilite(
            paste(
              ddSci(
                plyr::laply(
                  error.test.res.aggr,
                  function(x) x$Rsq
                ) *
                  100
              ),
              collapse = "%, "
            ),
            "%\n",
            sep = ""
          ),
          sep = ""
        )
      } else {
        cat(
          "Balanced Accuracy of ",
          n.resamples,
          " aggregated test sets",
          if (n.repeats > 1) " in each repeat",
          ": ",
          hilite(paste(
            ddSci(plyr::laply(
              error.test.res.aggr,
              function(x) x$`Balanced Accuracy`
            )),
            collapse = ", "
          )),
          "\n",
          sep = ""
        )
      }
    } else {
      # strat.sub, strat.boot, bootstrap
      if (type == "Regression") {
        cat(
          "Mean MSE of ",
          n.resamples,
          " ",
          desc,
          if (n.repeats > 1) " in each repeat",
          ": ",
          hilite(paste(
            ddSci(plyr::laply(
              error.test.res.mean,
              function(x) x$MSE
            )),
            collapse = ", "
          )),
          "\n",
          sep = ""
        )
        cat(
          "Mean MSE reduction",
          if (n.repeats > 1) " in each repeat",
          ": ",
          hilite(
            paste(
              ddSci(plyr::laply(
                error.test.res.mean,
                function(x) x$Rsq * 100
              )),
              collapse = "%, "
            ),
            "%\n",
            sep = ""
          ),
          sep = ""
        )
      } else {
        cat(
          "Mean Balanced Accuracy of ",
          n.resamples,
          " ",
          desc,
          if (n.repeats > 1) " in each repeat ",
          ": ",
          hilite(paste(
            ddSci(plyr::laply(
              error.test.res.mean,
              function(x) x$`Balanced.Accuracy`
            )),
            collapse = ", "
          )),
          "\n",
          sep = ""
        )
      }
    }

    # n.repeats mean
    if (n.repeats > 1) {
      if (type == "Regression") {
        cat(
          "Mean MSE across",
          n.repeats,
          "repeats =",
          hilite(
            ddSci(error.test.repeats.mean$MSE)
          ),
          "\n"
        )
        cat(
          "Mean R-squared was",
          hilite(
            ddSci(error.test.repeats.mean$Rsq)
          ),
          "\n"
        )
      } else if (type == "Classification") {
        cat(
          "Mean Balanced Accuracy across",
          n.repeats,
          "repeats =",
          hilite(
            ddSci(error.test.repeats.mean$`Balanced.Accuracy`)
          ),
          "\n"
        )
      }
    }
  } # verbose

  # Outro ----
  if (type == "Classification") {
    y.train.res.aggr <- lapply(seq(n.repeats), function(i) {
      factor(c(y.train.res[[i]], recursive = TRUE, use.names = FALSE))
    })
    y.test.res.aggr <- lapply(seq(n.repeats), function(i) {
      factor(c(y.test.res, recursive = TRUE, use.names = FALSE))
    })
    fitted.res.aggr <- lapply(seq(n.repeats), function(i) {
      factor(c(fitted.res, recursive = TRUE, use.names = FALSE))
    })
    predicted.res.aggr <- lapply(seq(n.repeats), function(i) {
      factor(c(predicted.res, recursive = TRUE, use.names = FALSE))
    })
    for (i in seq(n.repeats)) {
      levels(y.train.res.aggr[[i]]) <- levels(y.test.res.aggr[[i]]) <-
        levels(predicted.res.aggr[[i]]) <- levels(y)
    }

    fitted.prob.aggr <- plyr::llply(mods, function(i) {
      unlist(
        plyr::llply(i, function(j) {
          j$mod1$fitted.prob
        }),
        use.names = FALSE
      )
    })
    predicted.prob.res <- plyr::llply(mods, function(i) {
      plyr::llply(i, function(j) {
        j$mod1$predicted.prob
      })
    })
    predicted.prob.aggr <- plyr::llply(mods, function(i) {
      unlist(
        plyr::llply(i, function(j) {
          j$mod1$predicted.prob
        }),
        use.names = FALSE
      )
    })
    names(y.train.res.aggr) <- names(y.test.res.aggr) <-
      names(predicted.res.aggr) <- names(fitted.res.aggr) <-
        names(fitted.prob.aggr) <- names(predicted.prob.aggr) <-
          paste0("CV_", alg, "_repeat", seq(mods))
  } else {
    y.train.res.aggr <- lapply(seq(n.repeats), function(i) {
      c(y.train.res[[i]], recursive = TRUE, use.names = FALSE)
    })
    y.test.res.aggr <- lapply(seq(n.repeats), function(i) {
      c(y.test.res[[i]], recursive = TRUE, use.names = FALSE)
    })
    fitted.res.aggr <- lapply(seq(n.repeats), function(i) {
      c(fitted.res[[i]], recursive = TRUE, use.names = FALSE)
    })
    predicted.res.aggr <- lapply(seq(n.repeats), function(i) {
      c(predicted.res[[i]], recursive = TRUE, use.names = FALSE)
    })
    fitted.prob.aggr <- predicted.prob.aggr <- NULL
    names(y.train.res.aggr) <- names(y.test.res.aggr) <-
      names(predicted.res.aggr) <- paste0(
        "CV_",
        alg,
        "_repeat",
        seq(mods)
      )
  }

  if (!save.tune) {
    best.tune <- NULL
  } else {
    names(best.tune) <- paste0("CV_", alg, "_repeat", seq(mods))
  }

  # Variable importance ----
  varimp <- plyr::llply(mods, function(r) {
    .vi <- plyr::ldply(r, function(n) t(n$mod1$varimp), .id = NULL)
    rownames(.vi) <- names(r)
    .vi
  })

  resampler.params <- list(
    resampler = outer.resampling$resampler,
    n.resamples = outer.resampling$n.resamples,
    stratify.var = outer.resampling$stratify.var,
    train.p = outer.resampling$train.p,
    strat.n.bins = outer.resampling$strat.n.bins,
    target.length = outer.resampling$target.length,
    seed = outer.resampling$seed
  )

  if (type == "Classification") {
    rt <- rtModCVClass$new(
      mod = mods,
      mod.name = alg,
      type = type,
      y.train = y,
      x.name = x.name,
      y.name = y.name,
      xnames = xnames,
      parameters = list(
        preprocess = .preprocess,
        decompose = .decompose,
        mod.params = train.params,
        best.tune = best.tune
      ),
      n.repeats = n.repeats,
      resampler.params = resampler.params,
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
      predicted.prob.res = predicted.prob.res,
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
      call = .call,
      question = question
    )
  } else {
    # Not Classification
    rt <- rtModCV$new(
      mod = mods,
      mod.name = alg,
      type = type,
      y.train = y,
      x.name = x.name,
      y.name = y.name,
      xnames = xnames,
      parameters = list(
        preprocess = .preprocess,
        decompose = .decompose,
        mod.params = train.params,
        best.tune = best.tune
      ),
      n.repeats = n.repeats,
      resampler.params = resampler.params,
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
      call = .call,
      question = question
    )
  }

  if (!save.mod) rt$mod <- NA
  if (save.rt) {
    rt_save(rt, outdir, file.prefix = "CV_", verbose = verbose)
  }
  if (print.plot) {
    if (plot.fitted) rt$plotFitted()
    if (plot.predicted) rt$plotPredicted()
  }
  if (!is.null(outdir)) {
    rt$plotFitted(
      filename = paste0(
        outdir,
        "CV_",
        alg,
        "_fitted.pdf"
      )
    )
    rt$plotPredicted(
      filename = paste0(
        outdir,
        "CV_",
        alg,
        "_predicted.pdf"
      )
    )
    if (alg %in% c("LightGBM", "LightRF")) {
      # LightGBM models need to be saved separately with
      # saveRDS.lgb.Booster
      # Loop repeats
      for (i in seq_along(mods)) {
        # Loop resamples
        for (j in seq_along(mods[[i]])) {
          # lightgbm::saveRDS.lgb.Booster(
          mods[[i]][[j]]$mod1$mod$save_model(
            file.path(
              outdir,
              paste0(alg, "_", i, "_", j, ".txt")
            )
          )
        }
      }
    }
  }
  outro(
    start_time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::train_cv
