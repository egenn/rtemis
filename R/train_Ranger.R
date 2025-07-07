# train_Ranger.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# https://imbs-hl.github.io/ranger/reference/ranger.html

#' Random Forest using Ranger
#'
#' @inheritParams train_GLMNET
#'
#' @return `ranger` model object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
train_Ranger <- function(
  x,
  dat_validation = NULL,
  weights = NULL,
  hyperparameters = setup_Ranger(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("ranger")

  # Checks ----
  check_is_S7(hyperparameters, RangerHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }
  # mtry cannot be larger than number of features
  if (any(hyperparameters@hyperparameters[["mtry"]] > NCOL(features(x)))) {
    cli::cli_abort(
      "mtry cannot be greater than number of features: {ncol(features(x))}."
    )
  }

  # Data ----
  check_supervised_data(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)

  # Train ----
  model <- ranger::ranger(
    formula = NULL,
    x = features(x),
    y = outcome(x),
    num.trees = hyperparameters@hyperparameters[["num_trees"]],
    mtry = hyperparameters@hyperparameters[["mtry"]],
    importance = hyperparameters@hyperparameters[["importance"]],
    write.forest = hyperparameters@hyperparameters[["write_forest"]],
    probability = type == "Classification",
    min.node.size = hyperparameters@hyperparameters[["min_node_size"]],
    min.bucket = hyperparameters@hyperparameters[["min_bucket"]],
    max.depth = hyperparameters@hyperparameters[["max_depth"]],
    replace = hyperparameters@hyperparameters[["replace"]],
    sample.fraction = hyperparameters@hyperparameters[["sample_fraction"]],
    case.weights = weights,
    splitrule = hyperparameters@hyperparameters[["splitrule"]],
    num.random.splits = hyperparameters@hyperparameters[["num.random.splits"]],
    alpha = hyperparameters@hyperparameters[["alpha"]],
    minprop = hyperparameters@hyperparameters[["minprop"]],
    poisson.tau = hyperparameters@hyperparameters[["poisson.tau"]],
    split.select.weights = hyperparameters@hyperparameters[[
      "split.select.weights"
    ]],
    always.split.variables = hyperparameters@hyperparameters[[
      "always.split.variables"
    ]],
    respect.unordered.factors = hyperparameters@hyperparameters[[
      "respect.unordered.factors"
    ]],
    scale.permutation.importance = hyperparameters@hyperparameters[[
      "scale.permutation.importance"
    ]],
    local.importance = hyperparameters@hyperparameters[["local.importance"]],
    regularization.factor = hyperparameters@hyperparameters[[
      "regularization.factor"
    ]],
    regularization.usedepth = hyperparameters@hyperparameters[[
      "regularization.usedepth"
    ]],
    keep.inbag = hyperparameters@hyperparameters[["keep.inbag"]],
    inbag = hyperparameters@hyperparameters[["inbag"]],
    holdout = hyperparameters@hyperparameters[["holdout"]],
    quantreg = hyperparameters@hyperparameters[["quantreg"]],
    time.interest = hyperparameters@hyperparameters[["time.interest"]],
    oob.error = hyperparameters@hyperparameters[["oob.error"]],
    num.threads = hyperparameters@hyperparameters[["num.threads"]],
    save.memory = hyperparameters@hyperparameters[["save.memory"]],
    verbose = verbosity > 0L,
    node.stats = hyperparameters@hyperparameters[["node.stats"]],
    seed = hyperparameters@hyperparameters[["seed"]],
    na.action = hyperparameters@hyperparameters[["na.action"]]
  )
  check_inherits(model, "ranger")
  model
} # /rtemis::train_Ranger

#' Predict from Ranger model
#'
#' @param model
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_Ranger <- function(
  model,
  newdata,
  type,
  verbosity = 0L,
  ranger_type = "response",
  ...
) {
  check_inherits(model, "ranger")
  check_inherits(newdata, "data.frame")

  # Predict ----
  predicted <- predict(
    model,
    data = newdata,
    type = ranger_type,
    verbose = verbosity > 0L,
    ...
  )[["predictions"]]
  if (type == "Classification" && NCOL(predicted) == 2L) {
    # In binary classification, ranger returns matrix with 2 columns
    # with probabilities for each class
    predicted <- predicted[, 2L]
  }
  predicted
} # /rtemis::predict_Ranger


#' Get variable importance from Ranger model
#'
#' @param model `ranger` model object.
#'
#' @keywords internal
#' @noRd
varimp_Ranger <- function(model) {
  check_inherits(model, "ranger")
  varimp <- ranger::importance(model)
} # /rtemis::varimp_Ranger


#' Validate Ranger Hyperparameters
#'
#' Validate Ranger Hyperparameters given training data.
#'
#' @param x data.frame or similar: Training data.
#' @param hyperparameters RangerHyperparameters: Hyperparameters to check.
#'
#' @return NULL. Will throw error if hyperparameters are invalid.
#'
#' @keywords internal
#' @noRd
method(validate_hyperparameters, RangerHyperparameters) <- function(
  x,
  hyperparameters
) {
  check_is_S7(x, class_data.frame)
  check_is_S7(hyperparameters, RangerHyperparameters)

  # Check mtry
  if (any(hyperparameters@hyperparameters[["mtry"]] > NCOL(features(x)))) {
    cli::cli_abort(
      "mtry cannot be greater than number of features: {ncol(features(x))}."
    )
  }

  hyperparameters
} # /rtemis::validate_hyperparameters.RangerHyperparameters
