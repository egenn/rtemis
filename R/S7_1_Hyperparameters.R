# S7_Hyperparameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# References ----
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/

# `tuned` values ----
# -9: Set by Tuner: Actively being tuned (Values fixed by Tuner).
# -2: Set by constructor: Not tunable (No tunable_hyperparameters).
# -1: Set by constructor: Not tunable (tunable_hyperparameters exist, but none of them have more than one value).
#  0: Set by constructor: Untuned but tunable (at least one of tunable_hyperparameters has more than one value).
#  1: Set by Tuner: Tuned (Started as 0, set to 1 when tuned).

# `crossvalidated` values ----
# 0: Running on single training set.
# 1: Running on cross-validated training sets.

#' @title Hyperparameters
#'
#' @description
#' Superclass for hyperparameters.
#'
#' @field algorithm Character: Algorithm name.
#' @field hyperparameters Named list of algorithm hyperparameter values.
#' @field tuned Integer: Tuning status.
#' @field crossvalidated Integer: Cross-validation status.
#' @field tunable_hyperparameters Character: Names of tunable hyperparameters.
#' @field fixed_hyperparameters Character: Names of fixed hyperparameters.
#'
#' @author EDG
Hyperparameters <- new_class(
  name = "Hyperparameters",
  properties = list(
    algorithm = class_character,
    hyperparameters = class_list,
    tunable_hyperparameters = class_character,
    fixed_hyperparameters = class_character,
    tuned = class_integer,
    crossvalidated = class_integer
  ),
  constructor = function(algorithm, hyperparameters, tunable_hyperparameters, fixed_hyperparameters) {
    # Test if any tunable_hyperparameters have more than one value
    if (length(tunable_hyperparameters) > 0) {
      if (any(sapply(hyperparameters[tunable_hyperparameters], length) > 1)) {
        tuned <- 0L # Search values defined for tunable hyperparameters.
      } else {
        tuned <- -1L # No search values defined for tunable hyperparameters.
      }
    } else {
      tuned <- -2L # No tunable hyperparameters
    }
    # GLMNET
    if (algorithm == "GLMNET") {
      if (is.null(hyperparameters$lambda)) {
        tuned <- 0L
      }
    }
    new_object(
      S7_object(),
      algorithm = algorithm,
      hyperparameters = hyperparameters,
      tunable_hyperparameters = tunable_hyperparameters,
      fixed_hyperparameters = fixed_hyperparameters,
      tuned = tuned,
      crossvalidated = 0L
    )
  }
) # /Hyperparameters

# Print Hyperparameters ----
#' Print Hyperparameters
#'
#' @description
#' Print Hyperparameters object.
#'
#' @param x Hyperparameters object.
#' @param ... Not used.
#'
#' @author EDG
#' @export
print.Hyperparameters <- function(x, ...) {
  objcat(paste(x@algorithm, "Hyperparameters"))
  printls(props(x))
  if (x@tuned == -9L) {
    cat(hilite2("\n  Hyperparameters are being tuned.\n"))
  } else if (x@tuned == -2L) {
    cat(hilite2("\n  No hyperparameters are tunable.\n"))
  } else if (x@tuned == 0L) {
    need_tuning <- names(get_params_need_tuning(x))
    cat(hilite2(
      "\n  ",
      ngettext(length(need_tuning), "Hyperparameter", "Hyperparameters"),
      oxfordcomma(
        need_tuning,
        format_fn = underline
      ), ngettext(length(need_tuning), "needs", "need"), "tuning.\n"
    ))
  } else if (x@tuned == -1L) {
    cat(hilite2("\n  No search values defined for tunable hyperparameters.\n"))
  } else if (x@tuned == 1L) {
    cat(hilite2("\n  Hyperparameters are tuned.\n"))
  }
  invisible(x)
}
method(print, Hyperparameters) <- function(x) {
  print.Hyperparameters(x)
} # rtemis::print.Hyperparameters

# Get tuned status ----
get_tuned_status <- new_generic("get_tuned_status", "x")
method(get_tuned_status, Hyperparameters) <- function(x) {
  if (length(x@tunable_hyperparameters) > 0) {
    if (any(sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1)) {
      0L
    } else {
      -1L
    }
  } else {
    -2L
  }
} # /get_tuned_status.Hyperparameters

# Update Hyperparameters ----
#' Update Hyperparameters
#'
#' @param x Hyperparameters object.
#' @param hyperparameters Named list of algorithm hyperparameter values.
#'
#' @author EDG
#' @keywords internal
update <- new_generic("update", "x")
method(update, Hyperparameters) <- function(x, hyperparameters, tuned = NULL) {
  for (hp in names(hyperparameters)) {
    x@hyperparameters[[hp]] <- hyperparameters[[hp]]
  }
  # Update tuned status
  if (is.null(tuned)) {
    x@tuned <- get_tuned_status(x)
  } else {
    x@tuned <- tuned
  }
  x
} # /update.Hyperparameters

# Freeze Hyperparameters ----
#' Freeze Hyperparameters
#'
#' @param x Hyperparameters object.
#'
#' @author EDG
freeze <- new_generic("freeze", "x")
method(freeze, Hyperparameters) <- function(x) {
  x@tuned <- -1
}

# Lock Hyperparameters ----
#' Lock Hyperparameters
#'
#' @param x Hyperparameters object.
#'
#' @author EDG
lock <- new_generic("lock", "x")
method(lock, Hyperparameters) <- function(x) {
  x@tuned <- 1
}

# Make Hyperparameters@hyperparameters@name `$`-accessible
method(`$`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}

# Make Hyperparameters@hyperparameters@name `[[`-accessible
method(`[[`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}

# `$`-autocomplete Hyperparameters@hyperparameters ----
.DollarNames.Hyperparameters <- function(x, pattern = "") {
  all_names <- names(x@hyperparameters)
  grep(pattern, all_names, value = TRUE)
}
method(`.DollarNames`, Hyperparameters) <- function(x, pattern = "") {
  .DollarNames.Hyperparameters(x, pattern)
}

# needs_tuning ----
needs_tuning <- new_generic("needs_tuning", "x")
method(needs_tuning, Hyperparameters) <- function(x) {
  x@tuned == 0
} # /needs_tuning.Hyperparameters

# get_params_need_tuning ----
get_params_need_tuning <- new_generic("get_params_need_tuning", "x")
method(get_params_need_tuning, Hyperparameters) <- function(x) { # -> list
  # Get tunable hyperparameters with more than one value
  x@hyperparameters[x@tunable_hyperparameters[sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1]]
} # /get_params_need_tuning.Hyperparameters

# CARTHyperparameters ----
CART_tunable <- c("cp", "maxdepth", "minsplit", "minbucket", "prune.cp")
CART_fixed <- c(
  "method", "model", "maxcompete", "maxsurrogate", "usesurrogate", "surrogatestyle",
  "xval", "cost"
)

# CART_tunable <- list(
#   cp = class_numeric,
#   maxdepth = class_integer,
#   minsplit = class_integer,
#   minbucket = class_integer,
#   prune.cp = class_numeric
# )

# CART_fixed <- list(
#   method = class_character,
#   model = class_logical,
#   maxcompete = class_integer,
#   maxsurrogate = class_integer,
#   usesurrogate = class_integer,
#   surrogatestyle = class_integer,
#   xval = class_integer,
#   cost = class_numeric
# )

#' @title CARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for CART.
#'
#' @author EDG
#' @export
CARTHyperparameters <- new_class(
  name = "CARTHyperparameters",
  parent = Hyperparameters,
  constructor = function(cp = NULL,
                         maxdepth = NULL,
                         minsplit = NULL,
                         minbucket = NULL,
                         prune.cp = NULL,
                         method = NULL,
                         model = NULL,
                         maxcompete = NULL,
                         maxsurrogate = NULL,
                         usesurrogate = NULL,
                         surrogatestyle = NULL,
                         xval = NULL,
                         cost = NULL) {
    new_object(
      Hyperparameters(
        algorithm = "CART",
        hyperparameters = list(
          cp = cp,
          maxdepth = maxdepth,
          minsplit = minsplit,
          minbucket = minbucket,
          prune.cp = prune.cp,
          method = method,
          model = model,
          maxcompete = maxcompete,
          maxsurrogate = maxsurrogate,
          usesurrogate = usesurrogate,
          surrogatestyle = surrogatestyle,
          xval = xval,
          cost = cost
        ),
        tunable_hyperparameters = CART_tunable,
        fixed_hyperparameters = CART_fixed
      )
    )
  } # /constructor
) # /rtemis::CARTHyperparameters

#' Setup CART Hyperparameters
#'
#' Setup hyperparameters for CART training.
#'
#' Get more information from [rpart::rpart] and [rpart::rpart.control].
#'
#' @param cp (Tunable) Numeric: Complexity parameter.
#' @param maxdepth (Tunable) Integer: Maximum depth of tree.
#' @param minsplit (Tunable) Integer: Minimum number of observations in a node to split.
#' @param minbucket (Tunable) Integer: Minimum number of observations in a terminal node.
#' @param prune.cp (Tunable) Numeric: Complexity for cost-complexity pruning after tree is built
#' @param method String: Splitting method.
#' @param model Logical: If TRUE, return a model.
#' @param maxcompete Integer: Maximum number of competitive splits.
#' @param maxsurrogate Integer: Maximum number of surrogate splits.
#' @param usesurrogate Integer: Number of surrogate splits to use.
#' @param surrogatestyle Integer: Type of surrogate splits.
#' @param xval Integer: Number of cross-validation folds.
#' @param cost Numeric (>=0): One for each feature.
#'
#' @author EDG
#' @export
setup_CART <- function(
    # tunable
    cp = 0.01,
    maxdepth = 20L,
    minsplit = 2L,
    minbucket = 1L, # round(minsplit / 3),
    prune.cp = NULL,
    # fixed
    method = "auto",
    model = TRUE,
    maxcompete = 4L,
    maxsurrogate = 5L,
    usesurrogate = 2L,
    surrogatestyle = 0L,
    xval = 0L,
    cost = NULL) {
  check_inherits(cp, "numeric")
  maxdepth <- clean_integer(maxdepth)
  minsplit <- clean_integer(minsplit)
  minbucket <- clean_integer(minbucket)
  check_inherits(prune.cp, "numeric")
  check_inherits(method, "character")
  check_inherits(model, "logical")
  maxcompete <- clean_integer(maxcompete)
  maxsurrogate <- clean_integer(maxsurrogate)
  usesurrogate <- clean_integer(usesurrogate)
  surrogatestyle <- clean_integer(surrogatestyle)
  xval <- clean_integer(xval)
  check_inherits(cost, "numeric")
  CARTHyperparameters(
    cp = cp,
    maxdepth = maxdepth,
    minsplit = minsplit,
    minbucket = minbucket,
    prune.cp = prune.cp,
    method = method,
    model = model,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate,
    usesurrogate = usesurrogate,
    surrogatestyle = surrogatestyle,
    xval = xval,
    cost = cost
  )
} # /setup_CART

# Test that all CART hyperparameters are set by setup_CART
stopifnot(all(c(CART_tunable, CART_fixed) %in% names(formals(setup_CART))))

# GLMNETHyperparameters ----
GLMNET_tunable <- "alpha"
GLMNET_fixed <- c(
  "family", "offset", "which.cv.lambda", "nlambda", "penalty.factor", "standardize", "intercept"
)

#' @title GLMNETHyperparameters
#'
#' @description
#' Hyperparameters subclass for GLMNET.
#'
#' @author EDG
#' @export
GLMNETHyperparameters <- new_class(
  name = "GLMNETHyperparameters",
  parent = Hyperparameters,
  constructor = function(alpha = NULL,
                         family = NULL,
                         offset = NULL,
                         which.cv.lambda = NULL,
                         nlambda = NULL,
                         lambda = NULL,
                         penalty.factor = NULL,
                         standardize = NULL,
                         intercept = TRUE) {
    check_float01inc(alpha)
    check_inherits(which.cv.lambda, "character")
    nlambda <- clean_posint(nlambda)
    check_inherits(penalty.factor, "numeric")
    check_inherits(standardize, "logical")
    new_object(
      Hyperparameters(
        algorithm = "GLMNET",
        hyperparameters = list(
          alpha = alpha,
          family = family,
          offset = offset,
          which.cv.lambda = which.cv.lambda,
          nlambda = nlambda,
          lambda = lambda,
          penalty.factor = penalty.factor,
          standardize = standardize,
          intercept = intercept
        ),
        tunable_hyperparameters = GLMNET_tunable,
        fixed_hyperparameters = GLMNET_fixed
      )
    )
  } # /constructor
) # /rtemis::GLMNETHyperparameters

#' Setup GLMNET Hyperparameters
#'
#' Setup hyperparameters for GLMNET training.
#'
#' Get more information from [glmnet::glmnet].
#'
#' @param alpha (Tunable) Numeric: Mixing parameter.
#' @param which.cv.lambda Character: Which lambda to use for prediction:
#' "lambda.1se" or "lambda.min"
#' @param nlambda Positive integer: Number of lambda values.
#' @param penalty.factor Numeric: Penalty factor for each feature.
#' @param standardize Logical: If TRUE, standardize features.
#'
#' @author EDG
#' @export
setup_GLMNET <- function(
    # tunable
    alpha = 1,
    # fixed
    family = NULL,
    offset = NULL,
    which.cv.lambda = "lambda.1se",
    nlambda = 100L,
    lambda = NULL,
    penalty.factor = NULL,
    standardize = TRUE,
    intercept = TRUE) {
  check_float01inc(alpha)
  check_inherits(which.cv.lambda, "character")
  nlambda <- clean_posint(nlambda)
  check_inherits(penalty.factor, "numeric")
  check_inherits(standardize, "logical")
  GLMNETHyperparameters(
    family = family,
    offset = offset,
    alpha = alpha,
    which.cv.lambda = which.cv.lambda,
    nlambda = nlambda,
    lambda = lambda,
    penalty.factor = penalty.factor,
    standardize = standardize,
    intercept = intercept
  )
} # /setup_GLMNET

# Test that all GLMNET hyperparameters are set by setup_GLMNET
stopifnot(all(c(GLMNET_tunable, GLMNET_fixed) %in% names(formals(setup_GLMNET))))

method(get_params_need_tuning, GLMNETHyperparameters) <- function(x) {
  # Get tunable hyperparameters with more than one value
  out <- x@hyperparameters[x@tunable_hyperparameters[sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1]]
  if (is.null(x$lambda)) {
    out <- c(out, list(lambda = NULL))
  }
  out
} # /get_params_need_tuning.GLMNETHyperparameters


# LightRFHyperparameters ----
LightRF_tunable <- c(
  "nrounds", "num_leaves", "maxdepth", "feature_fraction", "subsample",
  "lambda_l1", "lambda_l2", "max_cat_threshold", "min_data_per_group"
)
LightRF_fixed <- character(0)

#' @title LightRFHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRF
#'
#' @author EDG
#' @export
LightRFHyperparameters <- new_class(
  name = "LightRFHyperparameters",
  parent = Hyperparameters,
  constructor = function(nrounds = NULL,
                         num_leaves = NULL,
                         maxdepth = NULL,
                         feature_fraction = NULL,
                         subsample = NULL,
                         lambda_l1 = NULL,
                         lambda_l2 = NULL,
                         max_cat_threshold = NULL,
                         min_data_per_group = NULL,
                         linear_tree = NULL) {
    new_object(
      Hyperparameters(
        algorithm = "LightRF",
        hyperparameters = list(
          nrounds = nrounds,
          num_leaves = num_leaves,
          maxdepth = maxdepth,
          feature_fraction = feature_fraction,
          subsample = subsample,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          max_cat_threshold = max_cat_threshold,
          min_data_per_group = min_data_per_group,
          linear_tree = linear_tree
        ),
        tunable_hyperparameters = LightRF_tunable,
        fixed_hyperparameters = LightRF_fixed
      )
    )
  }
) # /rtemis::LightRFHyperparameters

#' Setup LightRF Hyperparameters
#'
#' Setup hyperparameters for LightRF training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param nrounds (Tunable) Positive integer: Number of boosting rounds.
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param maxdepth (Tunable) Integer: Maximum depth of trees.
#' @param feature_fraction (Tunable) Numeric: Fraction of features to use.
#' @param subsample (Tunable) Numeric: Fraction of data to use.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param max_cat_threshold (Tunable) Positive integer: Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Positive integer: Minimum number of data per categorical group.
#' @param linear_tree Logical: If TRUE, use linear trees.
#'
#' @author EDG
#' @export
setup_LightRF <- function(
    nrounds = 500L,
    num_leaves = 4096L,
    maxdepth = -1L,
    feature_fraction = 1.0,
    subsample = .623,
    lambda_l1 = 0,
    lambda_l2 = 0,
    max_cat_threshold = 32L,
    min_data_per_group = 32L,
    linear_tree = FALSE) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  maxdepth <- clean_integer(maxdepth)
  check_float01inc(feature_fraction)
  check_float01inc(subsample)
  check_float01inc(lambda_l1)
  check_float01inc(lambda_l2)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  check_logical(linear_tree)
  LightRFHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    maxdepth = maxdepth,
    feature_fraction = feature_fraction,
    subsample = subsample,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree
  )
} # /rtemis::setupLightRF
