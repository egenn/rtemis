# calibrate_cv.R
# ::rtemis::
# 2024 EDG lambdamd.org

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
#' @param alg Character: "gam" or "glm", algorithm to use for calibration
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
    alg = "gam",
    learn.params = list(),
    resample.params = setup.resample(resampler = "kfold", n.resamples = 5),
    which.repeat = 1,
    verbosity = 1,
    debug = FALSE) {
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
  # `mods` is length `n.resamples` of `mod` outer resamples
  mods <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      if (verbosity > 0) {
        msg20("Training CV-calibration models on outer resample ", i, "...")
      }
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
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
  names(mods) <- names(mod$resamples[[which.repeat]])

  # y_cal: the calibration set labels
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  y_cal <- lapply(
    seq_along(mods), \(i) {
      lapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$y.train
        }
      )
    }
  )
  names(y_cal) <- names(mod$resamples[[which.repeat]])

  # prob_cal: the calibration set fitted probabilities
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  prob_cal <- lapply(
    seq_along(mods), \(i) {
      lapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$fitted.prob
        }
      )
    }
  )
  names(prob_cal) <- names(mod$resamples[[which.repeat]])

  # y_test: the test set labels
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
  y_test <- lapply(
    seq_along(mods), \(i) {
      lapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$y.test
        }
      )
    }
  )
  names(y_test) <- names(mod$resamples[[which.repeat]])

  # prob_test: the test set predicted probabilities
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
  prob_test <- lapply(
    seq_along(mods), \(i) {
      lapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$predicted.prob
        }
      )
    }
  )
  names(prob_test) <- names(mod$resamples[[which.repeat]])

  # Get CV performance ----
  # Calibration training error
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  cal_train_error <- lapply(
    seq_along(mods), \(i) {
      lapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$error.train
        }
      )
    }
  )
  names(cal_train_error) <- names(mod$resamples[[which.repeat]])

  # Calibration test error
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  cal_test_error <- lapply(
    seq_along(mods), \(i) {
      lapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$error.test
        }
      )
    }
  )
  names(cal_test_error) <- names(mod$resamples[[which.repeat]])

  # Get all Brier scores
  # brier_train: All training set Brier scores
  # list (length n.resamples outer)
  # brier_train <- sapply(
  #   mod$mod[[which.repeat]], \(m) {
  #     m$mod1$error.train$Overall$`Brier Score`
  #   }
  # )

  # brier_test_raw: All uncalibrated test set Brier scores
  # list (length n.resamples outer)
  brier_test_raw <- sapply(
    mod$mod[[which.repeat]], \(m) {
      m$mod1$error.test$Overall$`Brier Score`
    }
  )

  # brier_cal: All calibration set Brier scores
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  brier_cal <- lapply(
    seq_along(mods), \(i) {
      sapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$error.train$Overall$`Brier Score`
        }
      )
    }
  )
  names(brier_cal) <- names(mod$resamples[[which.repeat]])

  # brier_test: All test set Brier scores
  # list (length n.resamples outer) of lists (length n.resamples calibration) of vectors
  brier_test <- lapply(
    seq_along(mods), \(i) {
      sapply(
        seq_along(mods[[i]]$mods), \(j) {
          mods[[i]]$mods[[j]]$mod1$error.test$Overall$`Brier Score`
        }
      )
    }
  )
  names(brier_test) <- names(mod$resamples[[which.repeat]])

  # Get mean and sd of Brier scores per outer resample
  brier_cal_mean <- lapply(brier_cal, mean)
  brier_cal_sd <- lapply(brier_cal, sd)
  brier_test_mean <- lapply(brier_test, mean)
  brier_test_sd <- lapply(brier_test, sd)

  # Get mean and sd of Brier scores across all outer resamples
  brier_allcal_mean <- mean(unlist(brier_cal_mean))
  brier_allcal_sd <- sd(unlist(brier_cal_sd))
  brier_alltest_mean <- mean(unlist(brier_test_mean))
  brier_alltest_sd <- sd(unlist(brier_test_sd))

  # Output ----
  out <- list(
    true_labels = lapply(mod$mod[[which.repeat]], \(x) x$mod1$y.test),
    predicted_prob = lapply(mod$mod[[which.repeat]], \(x) x$mod1$predicted.prob),
    mod_cal = mods,
    y_cal = y_cal,
    prob_cal = prob_cal,
    y_test = y_test,
    prob_test = prob_test,
    cal_train_error = cal_train_error,
    cal_test_error = cal_test_error,
    brier_test_raw = brier_test_raw,
    brier_cal = brier_cal,
    brier_test = brier_test,
    brier_cal_mean = brier_cal_mean,
    brier_cal_sd = brier_cal_sd,
    brier_test_mean = brier_test_mean,
    brier_test_sd = brier_test_sd,
    brier_allcal_mean = brier_allcal_mean,
    brier_allcal_sd = brier_allcal_sd,
    brier_alltest_mean = brier_alltest_mean,
    brier_alltest_sd = brier_alltest_sd
  )
  class(out) <- "rtModCVCalibration"
  out
} # rtemis::calibrate_cv


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
    which.resample = 1,
    filename = NULL, ...) {
  stopifnot(inherits(x, "rtModCVCalibration"))
  what <- match.arg(what)
  type <- match.arg(type)
  bin.method <- match.arg(bin.method)

  if (what == "calibration") {
    # Calibration plot ----
    # Calibration plot using dplot3_calibration()
    if (type == "aggregate.all") {
      ## Aggregate all ----
      # Aggregate all y_test for all outer resamples
      y_train_agg <- unlist(x$y_cal)
      prob_train_agg <- unlist(x$prob_cal)
      y_test_agg <- unlist(x$y_test)
      prob_test_agg <- unlist(x$prob_test)
      # Raw vs. Calibrated
      est.prob <- list(
        Raw = unlist(x$predicted_prob),
        Calibrated = prob_test_agg
      )
      names(est.prob) <- paste0(mod$mod.name, " ", names(est.prob))
      dplot3_calibration(
        true.labels = list(
          Raw = unlist(x$true_labels),
          Calibrated = y_test_agg
        ),
        est.prob = est.prob,
        bin.method = bin.method,
        subtitle = paste(
          "Aggregating across", length(x$y_cal), "outer &",
          length(x$y_cal[[1]]), "calibration resamples"
        ),
        filename = filename, ...
      )
    } else if (type == "aggregate.by.resample") {
      ## Aggregate by resample ----
      # Aggregate all y_test for each outer resample
      y_test_agg <- lapply(x$y_test, unlist)
      prob_test_agg <- lapply(x$prob_test, unlist)
      dplot3_calibration(
        true.labels = y_test_agg,
        est.prob = prob_test_agg,
        bin.method = bin.method,
        subtitle = paste(
          "Aggregating across", length(x$y_cal[[1]]), "calibration resamples",
          "for each outer resample"
        ),
        filename = filename, ...
      )
    }
  } else if (what == "brier") {
    # Brier boxplot ----
    if (type == "aggregate.all") {
      ## Aggregate all ----
      # Aggregate all brier_test for all outer resamples
      brier_cal_agg <- unlist(x$brier_cal)
      brier_test_agg <- unlist(x$brier_test)
      dplot3_box(
        list(
          Raw = x$brier_test_raw,
          Calibration_train = brier_cal_agg,
          Calibrated_test = brier_test_agg
        ),
        boxpoints = "all",
        annotate_meansd = TRUE,
        filename = filename, ...
      )
    } else if (type == "aggregate.by.resample") {
      ## Aggregate by resample ----
      brier_cal_resmean <- sapply(x$brier_cal, mean)
      brier_test_resmean <- sapply(x$brier_test, mean)
      dplot3_box(
        list(
          Raw = x$brier_test_raw,
          Calibration_train = brier_cal_resmean,
          Calibrated_test = brier_test_resmean
        ),
        boxpoints = "all",
        annotate_meansd = TRUE,
        filename = filename, ...
      )
    }
  }
} # rtemis::plot.rtModCVCalibration

