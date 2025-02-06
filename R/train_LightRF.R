# train_LightRF.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Random Forest using LightGBM
#'
#' @inheritParams train_GLMNET
#'
#' @author EDG
#' @export

train_LightRF <- function(
    x,
    dat_validation = NULL,
    weights = NULL,
    hyperparameters = setup_LightRF(),
    tuner_parameters = setup_tuner(),
    verbosity = 1L) {
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
  factor_index <- names(x)[which(sapply(x[, -ncol(x)], is.factor))]
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
      x <- prp@preprocessed$training
      dat_validation <- prp@preprocessed$validation
    }
  } else {
    factor_index <- NULL
  }
  type <- supervised_type(x)
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

  if (!is.null(dat_validation)) {
    dat_validation <- lightgbm::lgb.Dataset(
      data = as.matrix(dat_validation[, -ncol(dat_validation)]),
      categorical_feature = factor_index,
      label = if (type == "Classification") {
        as.integer(dat_validation[[ncol(dat_validation)]]) - 1
      } else {
        dat_validation[[ncol(dat_validation)]]
      }
    )
  }

  # Train ----
  mod <- lightgbm::lgb.train(
    params = hyperparameters@hyperparameters, # ?need get_lgb.train_params
    data = x,
    nrounds = hyperparameters$nrounds,
    valids = if (!is.null(dat_validation)) {
      list(training = x, validation = dat_validation)
    } else {
      list(training = x)
    },
    early_stopping_rounds = hyperparameters$early_stopping_rounds,
    verbose = verbosity - 2L
  )
  check_inherits(mod, "lgb.Booster")
  mod
} # /rtemis::train_LightRF

#' Predict from LightRF LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightRF`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_LightRF <- function(model, newdata, type) {
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
varimp_LightRF <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi$Gain))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_LightRF
