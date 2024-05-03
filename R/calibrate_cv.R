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
    verbosity = 1) {
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
        trace = verbosity > 0
      )
    }
  )
  names(mods) <- names(mod$resamples[[which.repeat]])

  # Get y_cal: the calibration set labels
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

  # Get prob_cal: the calibration set fitted probabilities
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
  # list (length n.resamples outer) of lists (length n.resamples test) of vectors
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

  # Output ----
  out <- list(
    mod_cal = mods,
    y_cal = y_cal,
    prob_cal = prob_cal,
    y_test = y_test,
    prob_test = prob_test,
    cal_train_error = cal_train_error,
    cal_test_error = cal_test_error,
    brier_cal = brier_cal,
    brier_test = brier_test,
    brier_cal_mean = brier_cal_mean,
    brier_cal_sd = brier_cal_sd,
    brier_test_mean = brier_test_mean,
    brier_test_sd = brier_test_sd
  )
  class(out) <- "rtModCVCalibration"
} # rtemis::calibrate_cv


#' Plot `rtModCVCalibration` object
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
    which.resample = 1, ...) {
  stopifnot(inherits(x, "rtModCVCalibration"))
  what <- match.arg(what)
  type <- match.arg(type)

  if (what == "calibration") {
    if (type == "aggregate.all") {
      # Aggregate all y_test for all outer resamples
      y_train_agg <- unlist(x$y_cal)
      prob_train_agg <- unlist(x$prob_cal)
      y_test_agg <- unlist(x$y_test)
      prob_test_agg <- unlist(x$prob_test)
      # Update dplot3_calibration to allow list for true.labels and est.prob
      # dplot3_calibration(
      #   true.labels = list(y_train_agg, y_test_agg),
      #   est.prob = list(prob_train_agg, prob_test_agg)
      # )
      dplot3_calibration(
        true.labels = y_train_agg,
        est.prob = prob_train_agg,
        main = "Train Set Calibration"
      )
      dplot3_calibration(
        true.labels = y_test_agg,
        est.prob = prob_test_agg,
        main = "Test Set Calibration"
      )
    } else if (type == "aggregate.by.resample") {
      # Aggregate all y_test for each outer resample
      y_test_agg <- lapply(x$y_test, unlist)
      prob_test_agg <- lapply(x$prob_test, unlist)
      dplot3_calibration(
        true.labels = y_test_agg,
        est.prob = prob_test_agg
      )
    }
  } else if (what == "brier") {
    if (type == "aggregate.all") {
      # Aggregate all brier_test for all outer resamples
      brier_cal_agg <- unlist(x$brier_cal)
      brier_test_agg <- unlist(x$brier_test)
      dplot3_box(
        list(Train = brier_cal_agg, Test = brier_test_agg),
        boxpoints = "all"
      )
    } else if (type == "aggregate.by.resample") {
      brier_cal <- as.data.frame(x$brier_cal)
      brier_test <- as.data.frame(x$brier_test)
      group <- factor(
        c(rep("Train", nrow(brier_cal)), rep("Test", nrow(brier_test))),
        levels = c("Train", "Test")
      )
      dplot3_box(
        x = rbind(brier_cal, brier_test),
        group = group,
        boxpoints = "all"
      )
    }
  }

} # rtemis::plot.rtModCVCalibration
