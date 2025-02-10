# train_CART.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a CART decision tree
#'
#' Train a CART decision tree using `rpart`.
#'
#' CART does not need any special preprocessing.
#' It works with numeric and factor variables and handles missing values.
#' The "train_*" functions train a single model.
#' Use [train] for tuning and testing using nested cross-validation.
#'
#' @inheritParams train_GLMNET
#' @param hyperparameters `CARTHyperparameters` object: make using [setup_CART].
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_CART <- function(
    x,
    weights = NULL,
    hyperparameters = setup_CART(),
    tuner_parameters = setup_tuner(),
    verbosity = 1L) {
  # Dependencies ----
  check_dependencies("rpart")

  # Arguments ----
  # Hyperparameters must be either untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised_data(
    x = x,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }
  type <- supervised_type(x)

  # Train ----
  # weights can't be NULL.
  # !If formula is character, the input to weights must be the unquoted column name in the data.frame
  # that contains weights, e.g. by doing cbind(x, weights = weights)
  model <- rpart::rpart(
    as.formula(make_formula(x)),
    data = x,
    weights = weights,
    control = rpart::rpart.control(
      minsplit = hyperparameters$minsplit,
      minbucket = hyperparameters$minbucket,
      cp = hyperparameters$cp,
      maxcompete = hyperparameters$maxcompete,
      maxsurrogate = hyperparameters$maxsurrogate,
      usesurrogate = hyperparameters$usesurrogate,
      surrogatestyle = hyperparameters$surrogatestyle,
      maxdepth = hyperparameters$maxdepth,
      xval = hyperparameters$xval
    )
  )

  # Cost-Complexity Pruning ----
  if (!is.null(hyperparameters$prune.cp)) {
    model <- rpart::prune(model, cp = hyperparameters$prune.cp)
  }
  check_inherits(model, "rpart")
  model
} # /rtemis::train_CART

#' Predict from rpart model
#'
#' @param model rpart model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_CART <- function(model, newdata, type) {
  if (type == "Classification") {
    # Classification
    # predict.rpart returns a matrix n_cases x n_classes,
    # with classes are ordered the same as factor levels
    predicted_prob <- predict(model, newdata = newdata, type = "prob") # binclasspos = 2L
    if (NCOL(predicted_prob) == 2L) {
      # In binary classification, rpart returns matrix with 2 columns
      predicted_prob <- predicted_prob[, 2L]
    }
    predicted_prob
  } else {
    predict(model, newdata = newdata)
  }
} # /rtemis::predict_CART

#' Get variable importance from rpart model
#'
#' @param model rpart model.
#'
#' @keywords internal
#' @noRd
varimp_CART <- function(model) {
  model[["variable.importance"]]
} # /rtemis::varimp_CART
