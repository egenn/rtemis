# calibrate_cv.R
# ::rtemis::
# 2024 EDG lambdamd.org

#' Calibrate cross-validated model
#'
#' Calibrate cross-validated model trained using [train_cv]
#'
#' This is a work in progress to be potentially incorporated into [train_cv]
#'
#' @param mod `rtModCV` object returned by [train_cv]
#' @param alg Character: "gam" or "glm", algorithm to use for calibration
#' @param learn_params List: List of parameters to pass to the learning algorithm
#' @param which.repeat Integer: Which repeat to use for calibration
#' @param seed Integer: seed for the resampling
#' @param verbosity Integer: 0: silent, > 0: print messages
#'
#' @return List: Calibrated models, test-set labels, test set performance metrics,
#' estimated probabilities (uncalibrated), calibrated probabilities,
#'
#'
#' @author E.D. Gennatas
#' @export

calibrate_cv <- function(
    mod,
    alg = "gam",
    learn_params = list(),
    which.repeat = 1,
    seed = NULL,
    verbosity = 1) {
  stopifnot(inherits(mod, "rtModCV"))
  # For each resample in mod, train calibration models using all other resamples'
  # predicted probabilities, then predict on the held-out resample

  # Args ----
  learn <- if (alg == "isoreg") "isotone::isotonic.regression" else select_learn(alg)

  # If IFW is set, check that it is not set to TRUE
  if ("ifw" %in% names(learn_params)) {
    if (learn_params$ifw == TRUE) {
      stop(
        "\nIFW must be set to FALSE for proper calibration.\n",
        "You do not need to set it at all in learn_params in this call:",
        " it will be set to FALSE internally.\n",
        "This message is to ensure you are aware of this."
      )
    }
  } else {
    learn_params$ifw <- FALSE
  }

  # Split each test set in half: calibration & test
  if (verbosity > 0) {
    msg2("Splitting original CV test sets further into calibration & test sets...")
  }
  res_cal <- lapply(
    mod$mod[[which.repeat]],
    \(m) {
      resample(m$mod1$y.test,
        n.resamples = 2, train.p = .5,
        seed = seed, verbosity = verbosity
      )
    }
  )

  # Train calibration models ----
  # Train on calibration set, predict on test set
  # y_test_cal: the calibration set labels
  y_test_cal <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod1$y.test[res_cal[[i]][[1]]]
    }
  )
  # predicted_prob_cal: the calibration set predicted probabilities
  predicted_prob_cal <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod1$predicted.prob[res_cal[[i]][[1]]]
    }
  )
  # y_test2: the test set labels
  y_test2 <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod1$y.test[-res_cal[[i]][[1]]]
    }
  )
  
  # predicted_prob_test: the test set predicted probabilities
  predicted_prob_test <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod1$predicted.prob[-res_cal[[i]][[1]]]
    }
  )

  if (verbosity > 0) msg2("Training calibration models...")
  mod_cal <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      do.call(
        what = learn,
        args = c(
          list(
            x = data.frame(
              # prob = mod1$predicted.prob[res_cal[[i]][[1]]],
              # labels = mod1$y.test[res_cal[[i]][[1]]]
              prob = predicted_prob_cal[[i]],
              labels = y_test_cal[[i]]
            )
          ),
          learn_params
        )
      )
    }
  )

  # Get calibrated probabilities ----
  # Predict on calibration sets
  if (verbosity > 0) msg2("Getting calibrated probabilities on calibration set...")
  calibrated_predicted_prob_cal <- if (rtenv$binclasspos == 1) {
    lapply(
      seq_along(mod_cal), \(i) {
        1 - predict(
          mod_cal[[i]]$mod,
          newdata = data.frame(
            prob = predicted_prob_cal[[i]]
          ),
          type = "response"
        )
      }
    )
  } else {
    lapply(
      seq_along(mod_cal), \(i) {
        1 - predict(
          mod_cal[[i]]$mod,
          newdata = data.frame(
            prob = predicted_prob_cal[[i]]
          ),
          type = "response"
        )
      }
    )
  }

  # Predict on test sets
  if (verbosity > 0) msg2("Getting calibrated probabilities on test set...")

  calibrated_predicted_prob_test <- if (rtenv$binclasspos == 1) {
    lapply(
      seq_along(mod_cal), \(i) {
        1 - predict(
          mod_cal[[i]]$mod,
          newdata = data.frame(
            prob = predicted_prob_test[[i]]
          ),
          type = "response"
        )
      }
    )
  } else {
    lapply(
      seq_along(mod_cal), \(i) {
        1 - predict(
          mod_cal[[i]]$mod,
          newdata = data.frame(
            prob = predicted_prob_test[[i]]
          ),
          type = "response"
        )
      }
    )
  }

  # Get CV performance ----
  # Model performance on test set not used for training or calibration
  # res_test_error: the test set performance metrics
  if (verbosity > 0) msg2("Calculating test set performance...")
  res_test_error <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod_error(
        true = mod1$y.test[-res_cal[[i]][[1]]],
        estimated = mod1$predicted[-res_cal[[i]][[1]]],
        estimated.prob = mod1$predicted.prob[-res_cal[[i]][[1]]]
      )
    }
  )

  # Output ----
  list(
    mod_cal = mod_cal,
    y_test_cal = y_test_cal,
    predicted_prob_cal = predicted_prob_cal,
    calibrated_predicted_prob_cal = calibrated_predicted_prob_cal,
    y_test_test = y_test2,
    predicted_prob_test = predicted_prob_test,
    calibrated_predicted_prob_test = calibrated_predicted_prob_test,
    res_test_error = res_test_error,
    res_test_error_mean = mean(sapply(res_test_error, \(e) e$Overall$AUC))
  )
} # rtemis::calibrate_cv
