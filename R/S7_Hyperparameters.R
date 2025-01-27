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
#' @export
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
  constructor = function(
      algorithm, hyperparameters, tunable_hyperparameters, fixed_hyperparameters) {
    # Test if any tunable_hyperparameters have more than one value
    if (length(tunable_hyperparameters) > 0) {
      if (any(sapply(hyperparameters[tunable_hyperparameters], length) > 1)) {
        tuned <- 0L
      } else {
        tuned <- -1L
      }
    } else {
      tuned <- -2L
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

#' Print Hyperparameters
#'
#' @description
#' print Hyperparameters object
#'
#' @param x Hyperparameters object
#'
#' @author EDG
#' @export
print.Hyperparameters <- function(x) {
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
      oxfordcomma(need_tuning, format_fn = underline), ngettext(length(need_tuning), "needs", "need"), "tuning.\n"
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
method(get_params_need_tuning, Hyperparameters) <- function(x) {
  # Get tunable hyperparameters with more than one value
  x@hyperparameters[x@tunable_hyperparameters[sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1]]
}

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
#' Hyperparameters subclass for CART hyperparameters.
#' 
#' @author EDG
#' @export
CARTHyperparameters <- new_class(
  name = "CARTHyperparameters",
  parent = Hyperparameters,
  constructor = function(
      cp = NULL,
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
) # /CARTHyperparameters

#' Setup CART hyperparameters
#'
#' Setup hyperparameters for CART training
#'
#' Get more information from [rpart::rpart] and [rpart::rpart.control].
#'
#' @param cp Numeric: Complexity parameter.
#' @param maxdepth Integer: Maximum depth of tree.
#' @param minsplit Integer: Minimum number of observations in a node to split.
#' @param minbucket Integer: Minimum number of observations in a terminal node.
#' @param prune.cp Numeric: Complexity for cost-complexity pruning after tree is built
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
