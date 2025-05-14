# train_LightGBM.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Gradient Boosting with LightGBM
#'
#' @param x data.frame or similar: Training set.
#' @param dat_validation data.frame or similar: Validation set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `GLMNETHyperparameters` object: make using [setup_GLMNET].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_LightGBM <- function(
  x,
  dat_validation = NULL,
  weights = NULL,
  hyperparameters = setup_LightGBM(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Checks ----
  check_is_S7(hyperparameters, LightGBMHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }

  # Convert "null" nrounds to max_nrounds
  if (hyperparameters[["nrounds"]] == "null") {
    hyperparameters@hyperparameters[["nrounds"]] <- hyperparameters[[
      "max_nrounds"
    ]]
  }

  # Data ----
  check_supervised_data(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  ## Objective ----
  if (type == "Classification") {
    nclasses <- length(levels(x[[ncol(x)]]))
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

  ## Preprocess ----
  factor_index <- names(x)[which(sapply(exc(x, ncol(x)), is.factor))]
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
    data = as.matrix(exc(x, ncol(x))),
    categorical_feature = factor_index,
    label = if (type == "Classification") {
      as.integer(x[[ncol(x)]]) - 1
    } else {
      x[[ncol(x)]]
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
} # /rtemis::train_LightGBM

#' Predict from LightGBM LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightGBM`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_LightGBM <- function(model, newdata, type, verbosity = 0L) {
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
} # /rtemis::predict_LightGBM

#' Get variable importance from LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightGBM`.
#'
#' @keywords internal
#' @noRd
varimp_LightGBM <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi[["Gain"]]))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_LightGBM

#' Explain LightGBM model
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
explain_LightGBM <- function(model, x, verbosity = 0L, ...) {
  # x should include only features
  check_inherits(model@model, "lgb.Booster")
  check_inherits(x, "data.frame")
  factor_index <- names(x)[which(sapply(x, is.factor))]
  # Preprocess ----
  x <- preprocess(
    x,
    parameters = setup_Preprocessor(
      factor2integer = TRUE,
      factor2integer_startat0 = TRUE
    ),
    verbosity = verbosity - 1L
  )@preprocessed
  lightgbm::lgb.interprete(
    model = model@model,
    data = as.matrix(x),
    idxset = seq_len(nrow(x))
  )
} # /rtemis::explain_LightGBM
