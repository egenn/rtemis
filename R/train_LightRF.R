# train_LightRF.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# LightGBM parameters: https://lightgbm.readthedocs.io/en/latest/Parameters.html

#' Random Forest using LightGBM
#'
#' @inheritParams train_GLMNET
#'
#' @author EDG
#' @keywords internal
#' @noRd
train_LightRF <- function(
  x,
  dat_validation = NULL,
  weights = NULL,
  hyperparameters = setup_LightRF(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Checks ----
  check_is_S7(hyperparameters, LightRFHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised_data(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  if (type == "Classification") {
    nclasses <- length(levels(outcome(x)))
  } else {
    nclasses <- NA
  }
  if (is.null(hyperparameters[["objective"]])) {
    hyperparameters@hyperparameters[["objective"]] <- if (
      type == "Regression"
    ) {
      "regression"
    } else {
      if (nclasses == 2) {
        "binary"
      } else {
        "multiclass"
      }
    }
  }
  factor_index <- names(x)[which(sapply(features(x), is.factor))]
  if (length(factor_index) > 0) {
    prp <- preprocess(
      x,
      parameters = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      dat_validation = dat_validation,
      verbosity = verbosity - 1L
    )
    if (is.null(dat_validation)) {
      x <- prp@preprocessed
    } else {
      x <- prp@preprocessed[["training"]]
      dat_validation <- prp@preprocessed[["validation"]]
    }
  } else {
    factor_index <- NULL
  }
  x <- lightgbm::lgb.Dataset(
    data = as.matrix(features(x)),
    categorical_feature = factor_index,
    label = if (type == "Classification") {
      as.integer(outcome(x)) - 1
    } else {
      outcome(x)
    },
    weight = weights
  )

  if (!is.null(dat_validation)) {
    dat_validation <- lightgbm::lgb.Dataset(
      data = as.matrix(features(dat_validation)),
      categorical_feature = factor_index,
      label = if (type == "Classification") {
        as.integer(outcome(dat_validation)) - 1
      } else {
        outcome(dat_validation)
      }
    )
  }

  # Train ----
  model <- lightgbm::lgb.train(
    params = hyperparameters@hyperparameters, # ?need get_lgb.train_params
    data = x,
    nrounds = hyperparameters[["nrounds"]],
    valids = if (!is.null(dat_validation)) {
      list(training = x, validation = dat_validation)
    } else {
      list(training = x)
    },
    early_stopping_rounds = hyperparameters[["early_stopping_rounds"]],
    verbose = verbosity - 2L
  )
  check_inherits(model, "lgb.Booster")
  model
} # /rtemis::train_LightRF

#' Predict from LightRF LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightRF`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_LightRF <- function(model, newdata, type, verbosity = 0L) {
  check_inherits(model, "lgb.Booster")
  check_inherits(newdata, "data.frame")

  # Preprocess ----
  newdata <- as.matrix(
    preprocess(
      newdata,
      parameters = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      verbosity = 0L
    )@preprocessed
  )

  # Predict ----
  predict(model, newdata = newdata)
} # /rtemis::predict_LightRF

#' Get variable importance from LightRF model
#'
#' @param model lgb.Booster object trained using `train_LightRF`.
#'
#' @keywords internal
#' @noRd
varimp_LightRF <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi[["Gain"]]))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_LightRF

#' Explain LightRF model
#'
#' Get SHAP values for a LightRF model.
#'
#' @param model Supervised model trained with [train] (`algorithm="LightRF"`).
#' @param x data.frame or similar: Data to explain.
#' @param dat_training data.frame or similar: Training data.
#' @param dat_validation data.frame or similar: Validation data.
#' @param method Character: Method to use.
#' @param ... Not used.
#'
#' @keywords internal
#' @noRd
explain_LightRF <- function(
  model,
  x,
  verbosity = 0L,
  ...
) {
  explain_LightGBM(
    model = model,
    x = x,
    verbosity = verbosity
  )
} # /rtemis::explain_LightRF
