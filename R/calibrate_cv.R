# calibrate_cv.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Calibrate cross-validated model
#'
#' Calibrate cross-validated model trained using [train_cv]
#'
#' This is a work in progress to be potentially incorporated into [train_cv]
#' You start by training a cross-validated model using [train_cv], then this function
#' can be used to calibrate the model. In order to use all available data, each outer
#' resample from the input `mod` is resampled (using 5-fold CV by default) to train
#' and test calibration models. This allows using the original label-based metrics
#' of `mod` and also extract calibration metrics based on the same data, after
#' aggregating the test set predictions of the calibration models.
#'
#' @param mod `rtModCV` object returned by [train_cv]
#' @param alg Character: Algorithm to use for calibration
#' @param learn.params List: List of parameters to pass to the learning algorithm
#' @param resample.params List of parameters to pass to the resampling algorithm.
#' Build using [setup.resample]
#' @param which.repeat Integer: Which repeat to use for calibration
#' @param verbosity Integer: 0: silent, > 0: print messages
#' @param debug Logical: If TRUE, run without parallel processing, to allow better
#' debugging.
#'
#' @return List: Calibrated models, test-set labels, test set performance metrics,
#' estimated probabilities (uncalibrated), calibrated probabilities,
#'
#' @author E.D. Gennatas
#' @export

calibrate_cv <- function(
  mod,
  alg = "isotonic",
  learn.params = list(),
  resample.params = setup.resample(
    resampler = "kfold",
    n.resamples = 5,
    seed = NULL
  ),
  which.repeat = 1,
  verbosity = 1,
  debug = FALSE
) {
  stopifnot(inherits(mod, "rtModCV"))
  # For each resample in mod, train calibration models using all other resamples'
  # predicted probabilities, then predict on the held-out resample
  # Args ----
  # If IFW is set, check that it is not set to TRUE
  if ("ifw" %in% names(learn.params)) {
    if (learn.params$ifw == TRUE) {
      stop(
        "\nIFW must be set to FALSE for proper calibration.\n",
        "You do not need to set it at all in learn.params in this call:",
        " it will be set to FALSE internally.\n",
        "This message is to ensure you are aware of this."
      )
    }
  } else {
    learn.params$ifw <- FALSE
  }

  # resLearn calibration models ----
  # Train a CV-calibration model on each of outer resample test sets.
  # `calmods` is length `n.resamples` of `mod` outer resamples
  calmods <- lapply(
    seq_along(mod$mod[[which.repeat]]),
    \(i) {
      if (verbosity > 0) {
        msg20("Training CV-calibration models on outer resample ", i, "...")
      }
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      # We are resampling rtModCV's outer resamples' test sets to train calibration models
      resLearn(
        x = data.frame(
          est_prob = mod1$predicted.prob
        ),
        y = mod1$y.test,
        alg = alg,
        resample.params = resample.params,
        weights = NULL,
        params = learn.params,
        mtry = NULL,
        .preprocess = NULL,
        verbose = verbosity > 0,
        trace = verbosity > 0,
        use.future = !debug
      )
    }
  )
  names(calmods) <- names(mod$resamples[[which.repeat]])

  # y_train: the og train set labels: not used
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  # y_train <- lapply(
  #   seq_along(calmods), \(i) {
  #     lapply(
  #       seq_along(calmods[[i]]$mods), \(j) {
  #         calmods[[i]]$mods[[j]]$mod1$y.train
  #       }
  #     )
  #   }
  # )
  # names(y_train) <- names(mod$resamples[[which.repeat]])

  # prob_fitted: the og train set fitted probabilities: not used
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  # prob_fitted <- lapply(
  #   seq_along(calmods), \(i) {
  #     lapply(
  #       seq_along(calmods[[i]]$mods), \(j) {
  #         calmods[[i]]$mods[[j]]$mod1$fitted.prob
  #       }
  #     )
  #   }
  # )
  # names(prob_fitted) <- names(mod$resamples[[which.repeat]])

  ## y_caltrain ----
  # -> the calibration training set labels
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
  y_caltrain <- lapply(
    seq_along(calmods),
    \(i) {
      lapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$y.train
        }
      )
    }
  )

  ## y_caltest ----
  # -> the calibration test set labels
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
  y_caltest <- lapply(
    seq_along(calmods),
    \(i) {
      lapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$y.test
        }
      )
    }
  )
  names(y_caltest) <- names(mod$resamples[[which.repeat]])

  ## prob_caltrain ----
  # -> the calibration training set fitted probabilities
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
  prob_caltrain <- lapply(
    seq_along(calmods),
    \(i) {
      lapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$fitted.prob
        }
      )
    }
  )
  names(prob_caltrain) <- names(mod$resamples[[which.repeat]])

  ## prob_caltest ----
  # -> the calibration test set predicted probabilities
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
  prob_caltest <- lapply(
    seq_along(calmods),
    \(i) {
      lapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$predicted.prob
        }
      )
    }
  )
  names(prob_caltest) <- names(mod$resamples[[which.repeat]])

  # Get CV performance ----
  # Calibration training error
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  error_caltrain <- lapply(
    seq_along(calmods),
    \(i) {
      lapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$error.train
        }
      )
    }
  )
  names(error_caltrain) <- names(mod$resamples[[which.repeat]])

  # Calibration test error
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  error_caltest <- lapply(
    seq_along(calmods),
    \(i) {
      lapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$error.test
        }
      )
    }
  )
  names(error_caltest) <- names(mod$resamples[[which.repeat]])

  # Get all Brier scores
  # brier_train: All training set Brier scores
  # list (length n.resamples outer)
  # brier_train <- sapply(
  #   mod$mod[[which.repeat]], \(m) {
  #     m$mod1$error.train$Overall$`Brier Score`
  #   }
  # )

  ## brier_test_raw ----
  # -> All uncalibrated test set Brier scores
  # list (length n.resamples outer)
  brier_test_raw <- sapply(
    mod$mod[[which.repeat]],
    \(m) {
      m$mod1$error.test$Overall$`Brier Score`
    }
  )

  ## brier_caltrain ----
  # -> All calibration training set Brier scores
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  brier_caltrain <- lapply(
    seq_along(calmods),
    \(i) {
      sapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$error.train$Overall$`Brier Score`
        }
      )
    }
  )
  names(brier_caltrain) <- names(mod$resamples[[which.repeat]])

  ## brier_caltest ----
  # -> All calibration test set Brier scores
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  brier_caltest <- lapply(
    seq_along(calmods),
    \(i) {
      sapply(
        seq_along(calmods[[i]]$mods),
        \(j) {
          calmods[[i]]$mods[[j]]$mod1$error.test$Overall$`Brier Score`
        }
      )
    }
  )
  names(brier_caltest) <- names(mod$resamples[[which.repeat]])

  # Get mean and sd of Brier scores per outer resample
  brier_caltrain_mean <- sapply(brier_caltrain, mean)
  brier_caltrain_sd <- sapply(brier_caltrain, sd)
  brier_caltest_mean <- sapply(brier_caltest, mean)
  brier_caltest_sd <- sapply(brier_caltest, sd)

  # Get mean and sd of Brier scores across all outer resamples
  brier_allcal_mean <- mean(brier_caltrain_mean)
  brier_allcal_sd <- sd(brier_caltrain_sd)
  brier_alltest_mean <- mean(brier_caltest_mean)
  brier_alltest_sd <- sd(brier_caltest_sd)

  # Output ----
  out <- list(
    y_test = lapply(mod$mod[[which.repeat]], \(x) x$mod1$y.test), # The outer test set labels
    predicted_prob = lapply(
      mod$mod[[which.repeat]],
      \(x) x$mod1$predicted.prob
    ), # The raw outer test set predicted probabilities
    mod_cal = calmods, # The calibration models, use for prediction on probabilities output by rtModCV
    y_caltrain = y_caltrain, # Calibration training set labels
    prob_caltrain = prob_caltrain, # Calibration training set probabilities
    y_caltest = y_caltest, # Calibration test set labels
    prob_caltest = prob_caltest, # Calibration test set probabilities
    error_caltrain = error_caltrain,
    error_caltest = error_caltest,
    brier_test_raw = brier_test_raw,
    brier_caltrain = brier_caltrain,
    brier_caltest = brier_caltest,
    brier_caltrain_mean = brier_caltrain_mean,
    brier_caltrain_sd = brier_caltrain_sd,
    brier_caltest_mean = brier_caltest_mean,
    brier_caltest_sd = brier_caltest_sd,
    brier_allcal_mean = brier_allcal_mean,
    brier_allcal_sd = brier_allcal_sd,
    brier_alltest_mean = brier_alltest_mean,
    brier_alltest_sd = brier_alltest_sd
  )
  class(out) <- "rtModCVCalibration"
  out
} # rtemis::calibrate_cv


#' Predict using calibrated model
#'
#' Predict using a calibrated model returned by [calibrate_cv]
#'
#' @param object `rtModCVCalibration` object returned by [calibrate_cv]
#' @param mod `rtModCV` object returned by [train_cv]
#' @param newdata Data frame: New data to predict on
#' @param ... Additional arguments - Use to define `which.repeat`, which should be an integer
#' defining which repeat to use for prediction. Defaults to 1 if not specified.
#'
#' @author EDG
#' @export
predict.rtModCVCalibration <- function(object, mod, newdata, ...) {
  stopifnot(inherits(object, "rtModCVCalibration"))
  stopifnot(inherits(mod, "rtModCV"))
  stopifnot(is.data.frame(newdata))

  # Read extra arguments
  args <- list(...)
  which.repeat <- args$which.repeat
  if (is.null(which.repeat)) {
    which.repeat <- 1
  }

  # Predict using the original cross-validated model
  pred <- predict(mod, newdata = newdata, which.repeat = which.repeat)

  # Predict using the calibration models
  # There are n.resamples outer resamples, each with n.resamples calibration resamples
  # total calibration models.
  # Get a prediction from each.
  prec_cal_res <- sapply(object$mod_cal, \(m) {
    lapply(m$mods, \(m1) {
      predict(m1, newdata = data.frame(est_prob = pred$predicted.prob))
    })
  })

  # Average predictions across calibration resamples

  # Output ----
  out <- list(
    pred = pred,
    prec_cal_res = prec_cal_res
    # pred_cal = pred_cal
  )
  out
} # rtemis::predict.rtModCVCalibration

#' Plot `rtModCVCalibration` object
#'
#' Plot Calibration plots and Brier score boxplots for `rtModCVCalibration` object
#'
#' For calibration plots, `type = "aggregate.all"` is likely the more informative one.
#' It shows calibrations curves before and after calibration by aggregating across all
#' outer test sets. The `type = "aggregate.by.resample"` option shows the calibration
#' curves after calibration for each outer resample.
#'
#' For Brier boxplots, `type = "aggregate.all"` shows 1 score per outer resample prior
#' to calibration and multiple (equal n.resamples used in [calibrate_cv]) brier scores
#' per outer resample after calibration. This is for diagnostic purposes mainly.
#' For presentation, the `type = "aggregate.by.resample"` option shows the mean Brier
#' score per outer resample prior to calibration and after calibration and makes more
#' sense. The uncalibrated estimates could be resampled using the calibration model
#' resamples to produce a more comparable boxplot of Brier scores when using the
#' `type = "aggregate.all"` option, but that seems artifactual.
#'
#' More options are certainly possibly, e.g. an "aggregate.none" that would show all
#' calibration resamples for all outer ersamples, and can be added in the future if
#' needed.
#'
#' @param x `rtModCVCalibration` object
#' @param what Character: "calibration" or "brier"
#' @param type Character: "aggregate.all" or "aggregate.by.resample"
#' @param bin.method Character: "quantile" or "equidistant"
#' @param filename Character: Path to save plot as pdf
#' @param ... Additional arguments
#'
#' @return plotly object
#'
#' @author E.D. Gennatas
#' @export

plot.rtModCVCalibration <- function(
  x,
  what = c("calibration", "brier"),
  type = c("aggregate.all", "aggregate.by.resample"),
  bin.method = c("quantile", "equidistant"),
  filename = NULL,
  ...
) {
  stopifnot(inherits(x, "rtModCVCalibration"))
  what <- match.arg(what)
  type <- match.arg(type)
  bin.method <- match.arg(bin.method)

  if (what == "calibration") {
    # Calibration plot ----
    # Calibration plot using dplot3_calibration()
    if (type == "aggregate.all") {
      ## Aggregate all ----
      # Aggregate all y_caltest for all outer resamples
      y_caltest_agg <- unlist(x$y_caltest)
      prob_caltest_agg <- unlist(x$prob_caltest)

      # Raw vs. Calibrated
      est.prob <- list(
        Raw = unlist(x$predicted_prob),
        Calibrated = prob_caltest_agg
      )
      names(est.prob) <- paste0(x$mod.name, " ", names(est.prob))
      dplot3_calibration(
        true.labels = list(
          Raw = unlist(x$y_test),
          Calibrated = y_caltest_agg
        ),
        predicted.prob = est.prob,
        bin.method = bin.method,
        subtitle = paste(
          "Aggregating across",
          length(x$y_caltest),
          "outer *<br>",
          length(x$y_caltest[[1]]),
          "calibration resamples"
        ),
        filename = filename,
        ...
      )
    } else if (type == "aggregate.by.resample") {
      ## Aggregate by resample ----
      # Aggregate all y_caltest for each outer resample
      y_caltest_agg <- lapply(x$y_caltest, unlist)
      prob_caltest_agg <- lapply(x$prob_caltest, unlist)
      dplot3_calibration(
        true.labels = y_caltest_agg,
        predicted.prob = prob_caltest_agg,
        bin.method = bin.method,
        subtitle = paste(
          "Aggregating across",
          length(x$y_caltest[[1]]),
          "calibration resamples",
          "for each outer resample"
        ),
        filename = filename,
        ...
      )
    }
  } else if (what == "brier") {
    # Brier boxplot ----
    if (type == "aggregate.all") {
      ## Aggregate all ----
      # Aggregate all brier_caltest for all outer resamples
      brier_caltrain_agg <- unlist(x$brier_caltrain)
      brier_caltest_agg <- unlist(x$brier_caltest)
      dplot3_box(
        list(
          Calibration_train = brier_caltrain_agg,
          Calibrated_test = brier_caltest_agg
        ),
        boxpoints = "all",
        annotate_meansd = TRUE,
        filename = filename,
        ...
      )
    } else if (type == "aggregate.by.resample") {
      ## Aggregate by resample ----
      brier_caltrain_resmean <- sapply(x$brier_caltrain, mean)
      brier_caltest_resmean <- sapply(x$brier_caltest, mean)
      dplot3_box(
        list(
          Raw = x$brier_test_raw,
          Calibration_train = brier_caltrain_resmean,
          Calibrated_test = brier_caltest_resmean
        ),
        boxpoints = "all",
        annotate_meansd = TRUE,
        filename = filename,
        ...
      )
    }
  }
} # rtemis::plot.rtModCVCalibration

present.rtModCVCalibration <- function(x, ...) {
  stopifnot(inherits(x, "rtModCVCalibration"))
  # Check that average Brier scores of calibration test sets is lower than raw test set
  nresamples <- length(x$brier_test_raw)
  calbetter <- sum((x$brier_test_raw - x$brier_caltest_mean) > 0)

  plot.rtModCVCalibration(x, ...)
} # rtemis::present.rtModCVCalibration
