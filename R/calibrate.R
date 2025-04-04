# calibrate.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Calibrate Binary Classification Models
#'
#' The goal of calibration is to adjust the predicted probabilities of a binary classification
#' model so that they better reflect the true probabilities (i.e. empirical risk) of the positive
#' class.
#'
#' Important: The calibration model's training data should be different from the classification 
#' model's training data.
#'
#' @param x Classification or ClassificationCV object: The model will be returned with the calibration model and
#' calibration metrics added.
#' @param predicted_probabilities Numeric vector: Predicted probabilities.
#' @param true_labels Factor: True class labels.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters Hyperparameters object: Setup using one of `setup_*` functions.
#'
#' @author EDG
#' @export

calibrate <- new_generic(
  "calibrate", ("x"),
  function(x, predicted_probabilities, true_labels, algorithm = "isotonic", hyperparameters = NULL) {
    S7_dispatch()
  }
)

method(calibrate, Classification) <- function(x,
                                              predicted_probabilities,
                                              true_labels,
                                              algorithm = "isotonic",
                                              hyperparameters = NULL) {
  # Check inputs
  check_is_S7(x, Classification)
  check_float01inc(predicted_probabilities)
  check_inherits(true_labels, "factor")

  # Training data is whatever is passed by user
  dat <- data.table(predicted_probabilities, true_labels)
  # Test data is taken from mod, if available
  if (!is.null(x$y_test) && !is.null(x$predicted_prob_test)) {
    dat_test <- data.table(
      predicted_probabilities = x$predicted_prob_test,
      true_labels = x$y_test
    )
  } else {
    dat_test <- NULL
  }
  # Calibration model
  cal_model <- train(
    dat,
    dat_test = dat_test,
    algorithm = algorithm,
    hyperparameters = hyperparameters
  )

  CalibratedClassification(x, cal_model)
} # /rtemis::calibrate
