# S7_Hyperparameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# References ----
# S7
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/
# LightGBM
# https://lightgbm.readthedocs.io/en/latest/Parameters.html


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
#' @noRd
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
      if (is.null(hyperparameters[["lambda"]])) {
        tuned <- 0L
      }
    }
    # LightGBM
    if (algorithm == "LightGBM") {
      if (is.null(hyperparameters[["nrounds"]])) {
        tuned <- 0L
      }
    }
    # SVM
    # Check kernel-specific hyperparameters
    if (algorithm == "SVM") {
      # linear => cost
      if (hyperparameters[["kernel"]] == "linear") {
        if (length(hyperparameters[["cost"]]) > 1) {
          tuned <- 0L
        }
      } else if (hyperparameters[["kernel"]] == "polynomial") {
        if (length(hyperparameters[["degree"]]) > 1) {
          tuned <- 0L
        }
      } else if (hyperparameters[["kernel"]] == "radial") {
        if (length(hyperparameters[["sigma"]]) > 1) {
          tuned <- 0L
        }
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
#' Print method for Hyperparameters object.
#'
#' @param x Hyperparameters object.
#' @param ... Not used.
#'
#' @author EDG
#' @export
print.Hyperparameters <- function(x, ...) {
  objcat(paste(x@algorithm, "Hyperparameters"))
  printls(props(x)[-1])
  if (x@tuned == -9L) {
    cat(hilite2("\n  Hyperparameters are being tuned.\n"))
  } else if (x@tuned == -2L) {
    cat(hilite2("\n  No hyperparameters are tunable.\n"))
  } else if (x@tuned == 0L) {
    need_tuning <- names(get_params_need_tuning(x))
    cat(hilite2(
      "\n  ",
      ngettext(length(need_tuning), "Hyperparameter ", "Hyperparameters "),
      oxfordcomma(
        need_tuning,
        format_fn = underline
      ), ngettext(length(need_tuning), " needs ", " need "), "tuning.\n"
    ))
  } else if (x@tuned == -1L) {
    cat(hilite2("\n  No search values defined for tunable hyperparameters.\n"))
  } else if (x@tuned == 1L) {
    cat(hilite2("\n  Hyperparameters are tuned.\n"))
  }
  invisible(x)
}
method(print, Hyperparameters) <- function(x, ...) {
  print.Hyperparameters(x)
} # rtemis::print.Hyperparameters

# is_tuned() ----
is_tuned <- new_generic("is_tuned", "x")
method(is_tuned, Hyperparameters) <- function(x) {
  x@tuned == 1L
} # /is_tuned.Hyperparameters

# get_tuned_status() ----
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
#' @noRd
# update <- new_generic("update", "x")
method(update, Hyperparameters) <- function(object, hyperparameters, tuned = NULL, ...) {
  for (hp in names(hyperparameters)) {
    object@hyperparameters[[hp]] <- hyperparameters[[hp]]
  }
  # Update tuned status
  if (is.null(tuned)) {
    object@tuned <- get_tuned_status(object)
  } else {
    object@tuned <- tuned
  }
  object
} # /update.Hyperparameters

# Freeze Hyperparameters ----
#' Freeze Hyperparameters
#'
#' @param x Hyperparameters object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
lock <- new_generic("lock", "x")
method(lock, Hyperparameters) <- function(x) {
  x@tuned <- 1
}

# Make Hyperparameters@hyperparameters@name `$`-accessible
method(`$`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}

# `$`-autocomplete Hyperparameters@hyperparameters ----
method(`.DollarNames`, Hyperparameters) <- function(x, pattern = "") {
  all_names <- names(x@hyperparameters)
  grep(pattern, all_names, value = TRUE)
}

# Make Hyperparameters@hyperparameters@name `[[`-accessible
method(`[[`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}

#' needs_tuning ----
#'
#' @keywords internal
#' @noRd
needs_tuning <- new_generic("needs_tuning", "x")
method(needs_tuning, Hyperparameters) <- function(x) {
  x@tuned == 0
} # /needs_tuning.Hyperparameters

# get_params_need_tuning ----
#' Get hyperparameters that need tuning in an algorithm-specific way.
#' 
#' @keywords internal
#' @noRd
method(get_params_need_tuning, Hyperparameters) <- function(x) { # -> list
  # Get tunable hyperparameters with more than one value
  x@hyperparameters[x@tunable_hyperparameters[sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1]]
} # /get_params_need_tuning.Hyperparameters

# get_params.(Hyperparameters, character) ----
method(get_params, list(Hyperparameters, class_character)) <- function(x, param_names) {
  sapply(param_names, function(p) x@hyperparameters[p], USE.NAMES = FALSE)
}


# GLMHyperparameters ----
#' @author EDG
#'
#' @keywords internal
#' @noRd
GLMHyperparameters <- new_class(
  name = "GLMHyperparameters",
  parent = Hyperparameters,
  constructor = function(ifw = NULL) {
    new_object(
      Hyperparameters(
        algorithm = "GLM",
        hyperparameters = list(
          ifw = ifw
        ),
        tunable_hyperparameters = "ifw",
        fixed_hyperparameters = character()
      )
    )
  } # /constructor
) # /rtemis::GLMHyperparameters

#' Setup GLM Hyperparameters
#'
#' Setup hyperparameters for GLM training.
#'
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @author EDG
#' @export
setup_GLM <- function(ifw = FALSE) {
  GLMHyperparameters(ifw = ifw)
}

# GAMHyperparameters ----
GAM_tunable <- c("k", "ifw")
GAM_fixed <- character()

#' @author EDG
#' @keywords internal
#' @noRd
GAMHyperparameters <- new_class(
  name = "GAMHyperparameters",
  parent = Hyperparameters,
  constructor = function(k = NULL, ifw = FALSE) {
    new_object(
      Hyperparameters(
        algorithm = "GAM",
        hyperparameters = list(
          k = k,
          ifw = ifw
        ),
        tunable_hyperparameters = GAM_tunable,
        fixed_hyperparameters = GAM_fixed
      )
    )
  } # /constructor
) # /rtemis::GAMHyperparameters

#' Setup GAM Hyperparameters
#'
#' Setup hyperparameters for GAM training.
#'
#' Get more information from [mgcv::gam].
#'
#' @param k (Tunable) Integer: Number of knots.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @author EDG
#' @export
setup_GAM <- function(k = 5L, ifw = FALSE) {
  k <- clean_posint(k)
  GAMHyperparameters(k = k)
} # /rtemis::setup_GAM


# CARTHyperparameters ----
CART_tunable <- c("cp", "maxdepth", "minsplit", "minbucket", "prune.cp", "ifw")
CART_fixed <- c(
  "method", "model", "maxcompete", "maxsurrogate", "usesurrogate", "surrogatestyle",
  "xval", "cost"
)

#' @title CARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for CART.
#'
#' @author EDG
#' @keywords internal
#' @noRd
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
                         cost = NULL,
                         ifw = NULL) {
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
          cost = cost,
          ifw = ifw
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
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return CARTHyperparameters object.
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
    cost = NULL,
    ifw = FALSE) {
  check_inherits(cp, "numeric")
  maxdepth <- clean_int(maxdepth)
  minsplit <- clean_int(minsplit)
  minbucket <- clean_int(minbucket)
  check_inherits(prune.cp, "numeric")
  check_inherits(method, "character")
  check_inherits(model, "logical")
  maxcompete <- clean_int(maxcompete)
  maxsurrogate <- clean_int(maxsurrogate)
  usesurrogate <- clean_int(usesurrogate)
  surrogatestyle <- clean_int(surrogatestyle)
  xval <- clean_int(xval)
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
    cost = cost,
    ifw = ifw
  )
} # /setup_CART

# Test that all CART hyperparameters are set by setup_CART
stopifnot(all(c(CART_tunable, CART_fixed) %in% names(formals(setup_CART))))

# GLMNETHyperparameters ----
GLMNET_tunable <- c("alpha", "ifw")
GLMNET_fixed <- c(
  "family", "offset", "which.cv.lambda", "nlambda", "penalty.factor", "standardize", "intercept"
)

#' @title GLMNETHyperparameters
#'
#' @description
#' Hyperparameters subclass for GLMNET.
#'
#' @author EDG
#' @keywords internal
#' @noRd
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
                         intercept = TRUE,
                         ifw = NULL) {
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
          intercept = intercept,
          ifw = ifw
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
#' @param family Character: Family for GLMNET.
#' @param offset Numeric: Offset for GLMNET.
#' @param which.cv.lambda Character: Which lambda to use for prediction:
#' "lambda.1se" or "lambda.min"
#' @param nlambda Positive integer: Number of lambda values.
#' @param lambda Numeric: Lambda values.
#' @param penalty.factor Numeric: Penalty factor for each feature.
#' @param standardize Logical: If TRUE, standardize features.
#' @param intercept Logical: If TRUE, include intercept.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
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
    intercept = TRUE,
    ifw = FALSE) {
  check_float01inc(alpha)
  check_inherits(which.cv.lambda, "character")
  nlambda <- clean_posint(nlambda)
  check_inherits(penalty.factor, "numeric")
  check_logical(standardize)
  check_logical(ifw)
  GLMNETHyperparameters(
    family = family,
    offset = offset,
    alpha = alpha,
    which.cv.lambda = which.cv.lambda,
    nlambda = nlambda,
    lambda = lambda,
    penalty.factor = penalty.factor,
    standardize = standardize,
    intercept = intercept,
    ifw = ifw
  )
} # /setup_GLMNET

# Test that all GLMNET hyperparameters are set by setup_GLMNET
stopifnot(all(c(GLMNET_tunable, GLMNET_fixed) %in% names(formals(setup_GLMNET))))

method(get_params_need_tuning, GLMNETHyperparameters) <- function(x) {
  # Get tunable hyperparameters with more than one value
  out <- x@hyperparameters[x@tunable_hyperparameters[sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1]]
  if (is.null(x[["lambda"]])) {
    out <- c(out, list(lambda = NULL))
  }
  out
} # /get_params_need_tuning.GLMNETHyperparameters

# LightCARTHyperparameters ----
LightCART_tunable <- c(
  "num_leaves", "max_depth", "lambda_l1", "lambda_l2", "max_cat_threshold",
  "min_data_per_group", "linear_tree", "ifw"
)
LightCART_fixed <- c("objective")

#' @title LightCARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightCART
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightCARTHyperparameters <- new_class(
  name = "LightCARTHyperparameters",
  parent = Hyperparameters,
  constructor = function(num_leaves = NULL,
                         max_depth = NULL,
                         lambda_l1 = NULL,
                         lambda_l2 = NULL,
                         max_cat_threshold = NULL,
                         min_data_per_group = NULL,
                         linear_tree = NULL,
                         objective = NULL,
                         ifw = FALSE) {
    new_object(
      Hyperparameters(
        algorithm = "LightCART",
        hyperparameters = list(
          num_leaves = num_leaves,
          max_depth = max_depth,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          max_cat_threshold = max_cat_threshold,
          min_data_per_group = min_data_per_group,
          linear_tree = linear_tree,
          objective = objective,
          ifw = ifw
        ),
        tunable_hyperparameters = LightCART_tunable,
        fixed_hyperparameters = LightCART_fixed
      )
    )
  } # /constructor
) # /rtemis::LightCARTHyperparameters

#' Setup LightCART Hyperparameters
#'
#' Setup hyperparameters for LightCART training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param max_cat_threshold (Tunable) Positive integer: Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Positive integer: Minimum number of data per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Character: Objective function.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return LightCARTHyperparameters object.
#'
#' @author EDG
#' @export
setup_LightCART <- function(
    num_leaves = 32L,
    max_depth = -1L,
    lambda_l1 = 0,
    lambda_l2 = 0,
    max_cat_threshold = 32L,
    min_data_per_group = 100L,
    linear_tree = FALSE,
    objective = NULL,
    ifw = FALSE) {
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_float01inc(lambda_l1)
  check_float01inc(lambda_l2)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  check_logical(linear_tree)
  LightCARTHyperparameters(
    num_leaves = num_leaves,
    max_depth = max_depth,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    objective = objective,
    ifw = ifw
  )
} # /rtemis::setup_LightCART


# LightRFHyperparameters ----
LightRF_tunable <- c(
  "nrounds", "num_leaves", "maxdepth", "feature_fraction", "subsample",
  "lambda_l1", "lambda_l2", "max_cat_threshold", "min_data_per_group", "ifw"
)
LightRF_fixed <- c("subsample_freq", "early_stopping_rounds", "tree_learner", "objective")

#' @title LightRFHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRF
#'
#' @author EDG
#' @keywords internal
#' @noRd
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
                         linear_tree = NULL,
                         ifw = NULL,
                         # fixed LightGBM params for RF
                         subsample_freq = 1L,
                         early_stopping_rounds = -1L,
                         tree_learner = "data_parallel",
                         objective = NULL) {
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
          linear_tree = linear_tree,
          ifw = ifw,
          subsample_freq = subsample_freq,
          early_stopping_rounds = early_stopping_rounds,
          tree_learner = tree_learner,
          objective = objective
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
#' Note that hyperparameters subsample_freq, early_stopping_rounds, and tree_learner are fixed,
#' and cannot be set because they are what makes `lightgbm` train a random forest.
#' These can all be set when training gradient boosting with LightGBM.
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
#' @param objective Character: Objective function.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
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
    linear_tree = FALSE,
    objective = NULL,
    ifw = FALSE) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  maxdepth <- clean_int(maxdepth)
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
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective
  )
} # /rtemis::setupLightRF

# Test that all LightRF hyperparameters are set by setup_LightRF
# LightRF fixed hyperparameters are not editable.
stopifnot(all(LightRF_tunable %in% names(formals(setup_LightRF))))


# LightGBMHyperparameters ----
LightGBM_tunable <- c(
  "num_leaves", "max_depth", "learning_rate", "feature_fraction", "subsample", "subsample_freq",
  "lambda_l1", "lambda_l2", "max_cat_threshold", "min_data_per_group", "linear_tree", "ifw"
)
LightGBM_fixed <- c("max_nrounds", "force_nrounds", "early_stopping_rounds", "objective")

#' @title LightGBMHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightGBM
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightGBMHyperparameters <- new_class(
  name = "LightGBMHyperparameters",
  parent = Hyperparameters,
  constructor = function(max_nrounds = NULL,
                         force_nrounds = NULL,
                         early_stopping_rounds = NULL,
                         # tunable
                         num_leaves = NULL,
                         max_depth = NULL,
                         learning_rate = NULL,
                         feature_fraction = NULL,
                         subsample = NULL,
                         subsample_freq = NULL,
                         lambda_l1 = NULL,
                         lambda_l2 = NULL,
                         max_cat_threshold = NULL,
                         min_data_per_group = NULL,
                         linear_tree = NULL,
                         ifw = NULL,
                         objective = NULL) {
    nrounds <- if (!is.null(force_nrounds)) {
      force_nrounds
    } else {
      NULL
    }
    new_object(
      Hyperparameters(
        algorithm = "LightGBM",
        hyperparameters = list(
          nrounds = nrounds,
          max_nrounds = max_nrounds,
          force_nrounds = force_nrounds,
          early_stopping_rounds = early_stopping_rounds,
          num_leaves = num_leaves,
          max_depth = max_depth,
          learning_rate = learning_rate,
          subsample = subsample,
          subsample_freq = subsample_freq,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          max_cat_threshold = max_cat_threshold,
          min_data_per_group = min_data_per_group,
          linear_tree = linear_tree,
          ifw = ifw,
          objective = objective
        ),
        tunable_hyperparameters = LightGBM_tunable,
        fixed_hyperparameters = LightGBM_fixed
      )
    )
  }
) # /rtemis::LightGBMHyperparameters

method(update, LightGBMHyperparameters) <- function(object, hyperparameters, tuned = NULL, ...) {
  for (hp in names(hyperparameters)) {
    object@hyperparameters[[hp]] <- hyperparameters[[hp]]
  }
  # Update tuned status
  if (is.null(tuned)) {
    object@tuned <- get_tuned_status(object)
  } else {
    object@tuned <- tuned
  }
  # Update nrounds (e.g. in LightRuleFit)
  if (is.null(object@hyperparameters[["nrounds"]]) && !is.null(object@hyperparameters[["force_nrounds"]])) {
    object@hyperparameters[["nrounds"]] <- object@hyperparameters[["force_nrounds"]]
  }
  object
} # /update.Hyperparameters

# References:
# LightGBM parameters: https://lightgbm.readthedocs.io/en/latest/Parameters.html

#' Setup LightGBM Hyperparameters
#'
#' Setup hyperparameters for LightGBM training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param max_nrounds Positive integer: Maximum number of boosting rounds.
#' @param force_nrounds Positive integer: Use this many boosting rounds. Disable search for nrounds.
#' @param early_stopping_rounds Positive integer: Number of rounds without improvement to stop training.
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param learning_rate (Tunable) Numeric: Learning rate.
#' @param feature_fraction (Tunable) Numeric: Fraction of features to use.
#' @param subsample (Tunable) Numeric: Fraction of data to use.
#' @param subsample_freq (Tunable) Positive integer: Frequency of subsample.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param max_cat_threshold (Tunable) Positive integer: Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Positive integer: Minimum number of data per categorical group.
#' @param linear_tree Logical: If TRUE, use linear trees.
#' @param objective Character: Objective function.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @author EDG
#' @export
setup_LightGBM <- function(
    # nrounds will be auto-tuned if force_nrounds is NULL with a value up to max_nrounds and
    # using early_stopping_rounds.
    max_nrounds = 1000L,
    force_nrounds = NULL,
    early_stopping_rounds = 10L,
    # tunable
    num_leaves = 4096L,
    max_depth = -1L,
    learning_rate = 0.01,
    feature_fraction = 1.0,
    subsample = .623,
    subsample_freq = 1L,
    lambda_l1 = 0,
    lambda_l2 = 0,
    max_cat_threshold = 32L,
    min_data_per_group = 32L,
    linear_tree = FALSE,
    objective = NULL,
    ifw = FALSE) {
  max_nrounds <- clean_posint(max_nrounds)
  force_nrounds <- clean_posint(force_nrounds)
  early_stopping_rounds <- clean_posint(early_stopping_rounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_floatpos1(learning_rate)
  check_floatpos1(feature_fraction)
  check_floatpos1(subsample)
  subsample_freq <- clean_posint(subsample_freq)
  check_inherits(lambda_l1, "numeric")
  check_inherits(lambda_l2, "numeric")
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  check_logical(linear_tree)
  LightGBMHyperparameters(
    max_nrounds = max_nrounds,
    force_nrounds = force_nrounds,
    early_stopping_rounds = early_stopping_rounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective
  )
} # /rtemis::setupLightGBM

# Test that all LightGBM hyperparameters are set by setup_LightGBM
stopifnot(all(c(LightGBM_tunable, LightGBM_fixed) %in% names(formals(setup_LightGBM))))

method(get_params_need_tuning, LightGBMHyperparameters) <- function(x) {
  # Get tunable hyperparameters with more than one value
  out <- x@hyperparameters[x@tunable_hyperparameters[sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1]]
  if (is.null(x[["nrounds"]])) {
    out <- c(out, list(nrounds = NULL))
  }
  out
} # /get_params_need_tuning.LightGBMHyperparameters


# LightRuleFitHyperparameters ----
LightRuleFit_tunable <- c(
  "nrounds", "num_leaves", "max_depth", "learning_rate", "subsample", "subsample_freq",
  "lambda_l1", "lambda_l2", "alpha", "ifw_lightgbm", "ifw_glmnet"
)
LightRuleFit_fixed <- c("lambda", "objective")
LightRuleFit_lightgbm_params <- c(
  "nrounds", "num_leaves", "max_depth", "learning_rate", "subsample", "subsample_freq",
  "lambda_l1", "lambda_l2", "objective"
)
LightRuleFit_glmnet_params <- c("alpha", "lambda")

#' @title LightRuleFitHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRuleFit.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightRuleFitHyperparameters <- new_class(
  name = "LightRuleFitHyperparameters",
  parent = Hyperparameters,
  constructor = function(nrounds = NULL,
                         num_leaves = NULL,
                         max_depth = NULL,
                         learning_rate = NULL,
                         subsample = NULL,
                         subsample_freq = NULL,
                         lambda_l1 = NULL,
                         lambda_l2 = NULL,
                         objective = NULL,
                         ifw_lightgbm = NULL,
                         # GLMNET
                         alpha = NULL,
                         lambda = NULL,
                         ifw_glmnet = NULL,
                         # IFW
                         ifw = NULL) {
    new_object(
      Hyperparameters(
        algorithm = "LightRuleFit",
        hyperparameters = list(
          nrounds = nrounds,
          num_leaves = num_leaves,
          max_depth = max_depth,
          learning_rate = learning_rate,
          subsample = subsample,
          subsample_freq = subsample_freq,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          objective = objective,
          ifw_lightgbm = ifw_lightgbm,
          # GLMNET
          alpha = alpha,
          lambda = lambda,
          ifw_glmnet = ifw_glmnet,
          # IFW
          ifw = ifw
        ),
        tunable_hyperparameters = LightRuleFit_tunable,
        fixed_hyperparameters = LightRuleFit_fixed
      )
    )
  }
) # /rtemis::LightRuleFitHyperparameters

#' Setup LightRuleFit Hyperparameters
#'
#' Setup hyperparameters for LightRuleFit training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param nrounds (Tunable) Positive integer: Number of boosting rounds.
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param learning_rate (Tunable) Numeric: Learning rate.
#' @param subsample (Tunable) Numeric: Fraction of data to use.
#' @param subsample_freq (Tunable) Positive integer: Frequency of subsample.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param objective Character: Objective function.
#' @param ifw_lightgbm (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the LightGBM
#' step.
#' @param objective Character: Objective function.
#' @param alpha (Tunable) Numeric: Alpha for GLMNET.
#' @param lambda Numeric: Lambda for GLMNET.
#' @param ifw_glmnet (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the GLMNET step.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification. This applies IFW
#' to both LightGBM and GLMNET.
#'
#' @return LightRuleFitHyperparameters object.
#'
#' @author EDG
#' @export
setup_LightRuleFit <- function(
    nrounds = 200L,
    num_leaves = 32L,
    max_depth = 4L,
    learning_rate = 0.1,
    subsample = 0.666,
    subsample_freq = 1L,
    lambda_l1 = 0,
    lambda_l2 = 0,
    objective = NULL,
    ifw_lightgbm = FALSE,
    alpha = 1,
    lambda = NULL,
    ifw_glmnet = FALSE,
    ifw = FALSE) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_floatpos1(learning_rate)
  check_floatpos1(subsample)
  subsample_freq <- clean_posint(subsample_freq)
  check_inherits(lambda_l1, "numeric")
  check_inherits(lambda_l2, "numeric")
  check_float01inc(alpha)
  check_inherits(lambda, "numeric")
  check_logical(ifw_lightgbm)
  check_logical(ifw_glmnet)
  check_logical(ifw)
  # If ifw, cannot have ifw_lightgbm or ifw_glmnet
  if (ifw) {
    if (ifw_lightgbm) {
      stop("Cannot set ifw and ifw_lightgbm at the same time.")
    }
    if (ifw_glmnet) {
      stop("Cannot set ifw and ifw_glmnet at the same time.")
    }
  }
  LightRuleFitHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    ifw_lightgbm = ifw_lightgbm,
    alpha = alpha,
    lambda = lambda,
    ifw_glmnet = ifw_glmnet,
    ifw = ifw
  )
} # /rtemis::setup_LightRuleFit


# IsotonicHyperparameters ----
Isotonic_tunable <- character()
Isotonic_fixed <- character()

#' @title IsotonicHyperparameters
#'
#' @description
#' Hyperparameters subclass for Isotonic Regression.
#'
#' @author EDG
#' @keywords internal
#' @noRd
IsotonicHyperparameters <- new_class(
  name = "IsotonicHyperparameters",
  parent = Hyperparameters,
  constructor = function(ifw = NULL) {
    new_object(
      Hyperparameters(
        algorithm = "Isotonic",
        hyperparameters = list(
          ifw = ifw
        ),
        tunable_hyperparameters = "ifw",
        fixed_hyperparameters = Isotonic_fixed
      )
    )
  }
) # /rtemis::IsotonicHyperparameters

# setup_Isotonic ----
#' Setup Isotonic Hyperparameters
#'
#' Setup hyperparameters for Isotonic Regression.
#'
#' There are not hyperparameters for this algorithm at this moment.
#' 
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return IsotonicHyperparameters object.
#'
#' @author EDG
#' @export

setup_Isotonic <- function(ifw = FALSE) {
  IsotonicHyperparameters(ifw = ifw)
} # /rtemis::setup_Isotonic


# SVMHyperparameters ----
SVM_tunable <- c("cost", "degree", "gamma", "coef0", "ifw")
SVM_fixed <- c("kernel")

#' @title SVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM.
#'
#' @author EDG
#' @keywords internal
#' @noRd
SVMHyperparameters <- new_class(
  name = "SVMHyperparameters",
  parent = Hyperparameters,
  constructor = function(hyperparameters = list(),
                         tunable_hyperparameters = character(),
                         fixed_hyperparameters = character()) {
    new_object(
      Hyperparameters(
        algorithm = "SVM",
        hyperparameters = hyperparameters,
        tunable_hyperparameters = tunable_hyperparameters,
        fixed_hyperparameters = fixed_hyperparameters
      )
    )
  } # /constructor
) # /rtemis::SVMHyperparameters


# RadialSVMHyperparameters ----
#' @title RadialSVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM with radial kernel.
#'
#' @author EDG
#' @keywords internal
#' @noRd
RadialSVMHyperparameters <- new_class(
  name = "RadialSVMHyperparameters",
  parent = SVMHyperparameters,
  constructor = function(cost = NULL,
                         gamma = NULL,
                         ifw = NULL) {
    new_object(
      SVMHyperparameters( 
        hyperparameters = list(
          kernel = "radial",
          cost = cost,
          gamma = gamma,
          ifw = ifw
        ),
        tunable_hyperparameters = c("cost", "gamma", "ifw"),
        fixed_hyperparameters = c("kernel")
      )
    )
  } # /constructor
) # /rtemis::RadialSVMHyperparameters

#' Setup RadialSVM Hyperparameters
#'
#' Setup hyperparameters for RadialSVM training.
#'
#' Get more information from [e1071::svm].
#'
#' @param cost (Tunable) Numeric: Cost of constraints violation.
#' @param gamma (Tunable) Numeric: Kernel coefficient.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return RadialSVMHyperparameters object.
#'
#' @author EDG
#' @export
setup_RadialSVM <- function(
    cost = 1,
    gamma = 0.01,
    ifw = FALSE) {
  check_inherits(cost, "numeric")
  check_inherits(gamma, "numeric")
  check_logical(ifw)
  RadialSVMHyperparameters(
    cost = cost,
    gamma = gamma,
    ifw = ifw
  )
} # /setup_RadialSVM

setup_SVM <- setup_RadialSVM