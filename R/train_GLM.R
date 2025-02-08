# train_GLM.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a GLM model
#'
#' Train a GLM model.
#'
#' GLM does not work in the presence of missing values.
#'
#' @param x data.frame or similar: Training set.
#' @param dat_validation data.frame or similar: Validation set.
#' @param dat_testing data.frame or similar: Testing set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters No hyperparameters available for `glm`.
#' @param tuner_parameters NULL.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal

train_GLM <- function(
    x,
    dat_validation = NULL,
    dat_testing = NULL,
    weights = NULL,
    hyperparameters = NULL,
    tuner_parameters = NULL,
    verbosity = 1L) {
  # Checks ----
  check_is_S7(hyperparameters, GLMHyperparameters)

  # Data ----
  check_supervised_data(
    x = x,
    dat_validation = dat_validation,
    dat_testing = dat_testing,
    allow_missing = FALSE,
    verbosity = verbosity
  )

  # ?Any params that may be NULL by setup_ but aren't allowed to be NULL by training fn

  # ? Can weights be NULL
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }

  type <- supervised_type(x)
  if (type == "Classification") {
    n_classes <- length(levels(x[, ncol(x)]))
    if (n_classes > 2L) {
      stop("GLM does not support multiclass classification")
    }
  } else {
    n_classes <- NA_integer_
  }

  # Formula ----
  formula <- as.formula(
    paste(
      names(x)[ncol(x)], "~",
      paste(names(x)[-ncol(x)], collapse = " + ")
    )
  )

  # Train ----
  family <- if (type == "Regression") {
    gaussian()
  } else if (type == "Classification") {
    binomial()
  }
  mod <- glm(
    formula = formula,
    family = family,
    data = x,
    weights = weights
  )
  check_inherits(mod, "glm")
  mod
} # /rtemis::train_GLM

#' Predict from GLM model
#'
#' @param model GLM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
predict_GLM <- function(model, newdata, type) {
  predict(model, newdata = newdata, type = "response")
} # /rtemis::predict_GLM

#' Get coefficients from GLM model
#'
#' @param model GLM model.
#'
#' @keywords internal
varimp_GLM <- function(model) {
  coef(model)
} # /rtemis::varimp_GLM

#' Get Standard Errors from GLM model
#'
#' @param model GLM model.
#' @param newdata data.frame or similar: Data to predict on.
se_GLM <- function(model, newdata) {
  predict(model, newdata = newdata, se.fit = TRUE)$se.fit
}
