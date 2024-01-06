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
    verbosity = 1) {

  stopifnot(inherits(mod, "rtModCV"))
  # For each resample in mod, train calibration models using all other resamples'
  # predicted probabilities, then predict on the held-out resample

  # Args ----
  learn <- select_learn(alg)

  # Split each test set in half: calibration & test
  if (verbosity > 0) msg2("Splitting test sets into calibration & test sets...")
  res_cal <- lapply(
    mod$mod[[which.repeat]],
    \(m) resample(m$mod1$y.test, n.resamples = 2, train.p = .5)
  )

  # Train calibration models ----
  # Train on calibration set, predict on test set
  predicted_prob_cal <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod1$predicted.prob[res_cal[[i]][[1]]]
    }
  )
  y_testcal <- lapply(
    seq_along(mod$mod[[which.repeat]]), \(i) {
      mod1 <- mod$mod[[which.repeat]][[i]]$mod1
      mod1$y.test[res_cal[[i]][[1]]]
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
              prob = mod1$predicted.prob[res_cal[[i]][[1]]],
              labels = mod1$y.test[res_cal[[i]][[1]]]
            )
          ),
          learn_params
        )
      )
    }
  )

  # Get calibrated probabilities ----
  # Predict on test sets
  if (verbosity > 0) msg2("Getting calibrated probabilities...")
  calibrated_prob <- lapply(
    seq_along(mod_cal), \(i) {
      predict(
        mod_cal[[i]],
        newdata = data.frame(
          prob = mod$mod[[which.repeat]][[i]]$mod1$predicted.prob[-res_cal[[i]][[1]]]
        ),
        type = "response"
      )
    }
  )

  # Get CV performance ----
  # Model performance on test set not used for training or calibration
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
    y_test = y_testcal,
    res_test_error = res_test_error,
    estimated_prob = predicted_prob_cal,
    calibrated_prob = calibrated_prob
  )

} # rtemis::calibrate_cv
