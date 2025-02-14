# train_SVM.R
# ::rtemis::
# 2025 EDG rtemis.org

# [ ] Rename SVM
# [ ] Check if weights are supported, if not, are class weights?

#' Train a SVM model
#'
#' Train a SVM model using `SVM`.
#'
#' SVM does not work in the presence of missing values.
#'
#' @param x data.frame or similar: Training set.
#' @param dat_validation data.frame or similar: Validation set.
#' @param dat_testing data.frame or similar: Testing set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `SVMHyperparameters` object: make using [setup_RadialSVM].
#' @param tuner_parameters `TunerParameters` object: make using [setup_GridSearch].
#' @param verbosity Integer: Verbosity level.
#'
#' @return Object of class `svm`.
#'
#' @author EDG
#' @keywords internal

train_SVM <- function(
    x,
    dat_validation = NULL,
    dat_testing = NULL,
    weights = NULL,
    hyperparameters = NULL,
    tuner_parameters = NULL,
    verbosity = 1L) {
  # Dependencies ----
  check_dependencies("e1071")

  # Checks ----
  check_is_S7(hyperparameters, SVMHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised_data(
    x = x,
    dat_validation = dat_validation,
    dat_testing = dat_testing,
    allow_missing = FALSE,
    verbosity = verbosity
  )

  type <- supervised_type(x)
  n_classes <- if (type == "Classification") {
    length(levels(x[, ncol(x)]))
  } else {
    NA
  }

  # One-hot encode ----
  y <- x[, ncol(x)]
  x <- preprocess(
    x[, -ncol(x), drop = FALSE],
    parameters = setup_Preprocessor(one_hot = TRUE)
  )@preprocessed

  # Can use class_weights or set class.weights = "inverse" in svm()
  # if (is.null(weights)) {
  #   weights <- rep(1, NROW(x))
  # }

  # Train ----
  class_weights <-
    if (type == "Classification" && n_classes == 2 && hyperparameters[["ifw"]]) "inverse" else NULL
  model <- e1071::svm(
    x = x,
    y = y, # factor or numeric
    kernel = hyperparameters[["kernel"]],
    cost = hyperparameters[["cost"]],
    gamma = hyperparameters[["gamma"]],
    class.weights = class_weights,
    probability = TRUE
  )
  check_inherits(model, "svm")
  model
} # /rtemis::train_SVM

#' Predict from SVM model
#'
#' @param model SVM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_SVM <- function(model, newdata, type, verbosity = 0L) {
  newdata <- preprocess(
    newdata,
    parameters = setup_Preprocessor(one_hot = TRUE),
    verbosity = verbosity - 1L
  )@preprocessed
  if (type == "Classification") {
    predicted_prob <- attr(
      predict(model, newdata = newdata, probability = TRUE),
      "probabilities"
    )
    if (length(model$levels) == 2) {
      predicted_prob[, 2]
    } else {
      predicted_prob
    }
  } else {
    predict(model, newdata = newdata)
  }
} # /rtemis::predict_SVM

#' Get coefficients from SVM model
#'
#' @param model SVM model.
#'
#' @keywords internal
varimp_SVM <- function(model) {
  if (model[["kernel"]] == "linear") {
    coef(model)
  } else {
    NULL
  }
} # /rtemis::varimp_SVM
