# train_GAM.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a GAM model
#'
#' Train a GAM model using `GAM`.
#'
#' GAM does not work in the presence of missing values.
#'
#' @param x data.frame or similar: Training set.
#' @param dat_validation data.frame or similar: Validation set.
#' @param dat_testing data.frame or similar: Testing set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `GAMHyperparameters` object: make using [setup_GAM].
#' @param tuner `Tuner` object: make using [setup_tuner].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_GAM <- function(
    x,
    dat_validation = NULL,
    dat_testing = NULL,
    weights = NULL,
    hyperparameters = NULL,
    tuner_parameters = NULL,
    verbosity = 1L) {
  # Dependencies ----
  check_dependencies("mgcv")

  # Checks ----
  check_is_S7(hyperparameters, GAMHyperparameters)

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

  # Formula ----
  # use s(x, k = k) for all numeric predictors
  index_numeric <- which(sapply(x[, -ncol(x), drop = FALSE], is.numeric))
  spline_features <- if (length(index_numeric) > 0) {
    paste0(
      "s(", colnames(x)[index_numeric], ", k = ", hyperparameters$k, ")",
      collapse = " + "
    )
  } else {
    ""
  }
  index_factor <- which(sapply(x[, -ncol(x), drop = FALSE], is.factor))
  categorical_features <- if (length(index_factor) > 0) {
    paste0(
      colnames(x)[index_factor],
      collapse = " + "
    )
  } else {
    ""
  }
  outcome_name <- colnames(x)[ncol(x)]
  formula <- as.formula(
    gsub(
      "^ \\+ | \\+ $",
      "",
      paste(outcome_name, "~", paste(spline_features, categorical_features, sep = " + "))
    )
  )

  # Train ----
  family <- if (type == "Regression") {
    gaussian()
  } else if (type == "Classification") {
    if (n_classes == 2) {
      binomial()
    } else {
      mgcv::multinom()
    }
  }
  mod <- mgcv::gam(
    formula = formula,
    family = family,
    data = x,
    weights = weights
  )
  check_inherits(mod, "gam")
  mod
} # /rtemis::train_GAM

#' Predict from GAM model
#'
#' @param model GAM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_GAM <- function(model, newdata, type) {
  predict(object = model, newdata = newdata, type = "response")
} # /rtemis::predict_GAM

#' Get coefficients from GAM model
#'
#' @param model mgcv gam model.
#'
#' @keywords internal
#' @noRd
varimp_GAM <- function(model, type = c("p-value", "coefficients", "edf")) {
  type <- match.arg(type)
  if (type == "p-value") {
    # Get parametric and smooth term p-values
    summary_ <- summary(model)
    # Exclude intercept
    -log10(c(summary_$s.table[, "p-value"], summary_$p.table[, ncol(summary_$p.table)][-1]))
  } else if (type == "coefficients") {
    coef(model)
  } else if (type == "edf") {
    summary(model)$s.table[, "edf"]
  }
} # /rtemis::varimp_GAM

#' Get Standard Errors from GAM model
#'
#' @param model mgcv gam model.
#' 
#' @keywords internal
#' @noRd
se_GAM <- function(model, newdata) {
  predict(model, newdata = newdata, se.fit = TRUE)$se.fit
}
