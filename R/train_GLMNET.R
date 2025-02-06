# train_GLMNET.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a GLMNET model
#'
#' Train a GLMNET model using `glmnet`.
#'
#' GLMNET does not work in the presence of missing values.
#'
#' @param x data.frame or similar: Training set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `GLMNETHyperparameters` object: make using [setup_GLMNET].
#' @param tuner `Tuner` object: make using [setup_tuner].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal

train_GLMNET <- function(
    x,
    weights = NULL,
    hyperparameters = setup_GLMNET(),
    tuner_parameters = setup_tuner(),
    verbosity = 1L) {
  # Dependencies ----
  check_dependencies("glmnet")

  # Checks ----
  check_is_S7(hyperparameters, GLMNETHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }
  # Check data-specific hyperparameter values
  # penalty.factor must be of length = N features.
  if (is.null(hyperparameters$penalty.factor)) {
    hyperparameters@hyperparameters$penalty.factor <- rep(1, NCOL(x) - 1)
    if (verbosity > 1) {
      info("Updated hyperparameters$penalty.factor to all 1s.")
    }
  } else {
    if (length(hyperparameters$penalty.factor) != NCOL(x) - 1) {
      stop("Length of penalty.factor must be equal to the number of predictors.")
    }
  }

  # Convert "null" lambda to NULL
  if (hyperparameters$lambda == "null") {
    hyperparameters@hyperparameters$lambda <- NULL
  }

  # Data ----
  check_supervised_data(
    x = x,
    allow_missing = FALSE,
    verbosity = verbosity
  )

  # weights can't be NULL.
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }
  type <- supervised_type(x)
  n_classes <- if (type == "Classification") {
    length(levels(x[, ncol(x)]))
  } else {
    NA
  }
  family <- if (is.null(hyperparameters$family)) {
    if (type == "Regression") {
      "gaussian"
    } else if (type == "Classification") {
      if (n_classes == 2) {
        "binomial"
      } else {
        "multinomial"
      }
    }
  }

  # Train ----
  # if lambda is NULL, use cv.glmnet to find optimal lambda
  if (is.null(hyperparameters$lambda)) {
    mod <- glmnet::cv.glmnet(
      x = as.matrix(
        model.matrix(~., x[, -ncol(x)])[, -1]
      ),
      y = x[, ncol(x)],
      family = family,
      weights = weights,
      offset = hyperparameters$offset,
      alpha = hyperparameters$alpha,
      nlambda = hyperparameters$nlambda,
      standardize = hyperparameters$standardize,
      intercept = hyperparameters$intercept, # can't be NULL
      penalty.factor = hyperparameters$penalty.factor
    )
    check_inherits(mod, "cv.glmnet")
  } else {
    mod <- glmnet::glmnet(
      x = as.matrix(
        model.matrix(~., x[, -ncol(x)])[, -1]
      ),
      y = x[, ncol(x)],
      family = family,
      weights = weights,
      offset = hyperparameters$offset,
      alpha = hyperparameters$alpha,
      nlambda = hyperparameters$nlambda,
      lambda = hyperparameters$lambda,
      standardize = hyperparameters$standardize,
      intercept = hyperparameters$intercept, # can't be NULL
      penalty.factor = hyperparameters$penalty.factor
    )
    check_inherits(mod, "glmnet")
  }
  mod
} # /rtemis::train_GLMNET

#' Predict from GLMNET model
#'
#' @param model glmnet model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_GLMNET <- function(model, newdata, type) {
  newdata <- as.matrix(
    model.matrix(~., newdata)[, -1, drop = FALSE]
  )
  if (type == "Regression") {
    predict(model, newx = newdata, type = "response")
  } else if (type == "Classification") {
    predicted_prob <- predict(model, newx = newdata, type = "response")
    if (NCOL(predicted_prob) == 1) {
      # In binary classification, glmnet returns matrix with 1 column
      # with probabilities of second level.
      predicted_prob <- as.numeric(predicted_prob)
    }
    predicted_prob
  }
} # /rtemis::predict_GLMNET

#' Get coefficients from GLMNET model
#'
#' @param model glmnet model.
#'
#' @keywords internal
varimp_GLMNET <- function(model) {
  coef(model)
} # /rtemis::varimp_CART
