# train_LightCART.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Decision Tree using LightGBM
#'
#' @inheritParams train_LightGBM
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_LightCART <- function(
    x,
    weights = NULL,
    hyperparameters = setup_LightCART(),
    verbosity = 1L) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Checks ----
  check_is_S7(hyperparameters, LightCARTHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised_data(
    x = x,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  if (type == "Classification") {
    nclasses <- length(levels(x[[ncol(x)]]))
  } else {
    nclasses <- NA
  }
  if (is.null(hyperparameters[["objective"]])) {
    hyperparameters@hyperparameters[["objective"]] <- if (type == "Regression") {
      "regression"
    } else {
      if (nclasses == 2) {
        "binary"
      } else {
        "multiclass"
      }
    }
  }
  factor_index <- names(x)[which(sapply(x[, -ncol(x)], is.factor))]
  if (length(factor_index) > 0) {
    prp <- preprocess(
      x,
      parameters = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      verbosity = verbosity - 1L
    )
    x <- prp@preprocessed
  } else {
    factor_index <- NULL
  }

  x <- lightgbm::lgb.Dataset(
    data = as.matrix(x[, -ncol(x)]),
    categorical_feature = factor_index,
    label = if (type == "Classification") {
      as.integer(x[[ncol(x)]]) - 1
    } else {
      x[[ncol(x)]]
    },
    weight = weights
  )

  # Train ----
  model <- lightgbm::lgb.train(
    params = hyperparameters@hyperparameters,
    data = x,
    nrounds = 1L,
    valids = list(training = x),
    early_stopping_rounds = NULL,
    verbose = verbosity - 2L
  )
  check_inherits(model, "lgb.Booster")
  model
} # /rtemis::train_LightCART

#' Predict from LightCART LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightCART`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_LightCART <- function(model, newdata, type) {
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
} # /rtemis::predict_LightCART

#' Get variable importance from LightCART model
#'
#' @param model lgb.Booster object trained using `train_LightCART`.
#'
#' @keywords internal
#' @noRd
varimp_LightCART <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi[["Gain"]]))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_LightCART
