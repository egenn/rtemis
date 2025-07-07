# S7_init.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# S7 generics: https://rconsortium.github.io/S7/articles/generics-methods.html

# Generics ----
# Standard error of the fit.
se <- new_generic("se", "x")
# Short description for inline printing.
desc <- new_generic("desc", "x")
# Alt description for inline printing.
desc_alt <- new_generic("desc_alt", "x")
# Get metrics
get_metric <- new_generic("get_metric", "x")
# Plot predicted vs. true values.
plot_true_pred <- new_generic("plot_true_pred", "x")
# Manhattan plot
plot_manhattan <- new_generic("plot_manhattan", "x")
# check hyperparameters given training data
validate_hyperparameters <- new_generic("validate_hyperparameters", "x")

#' Describe rtemis object
#'
#' @description
#' This generic is used to provide a description of an rtemis object in plain language.
#'
#' @param x An rtemis object.
#' @param ... Not used.
#'
#' @return A character string describing the object.
#'
#' @author EDG
#' @export
describe <- new_generic("describe", "x")

#' Present rtemis object
#'
#' @description
#' This generic is used to present an rtemis object using plots and text.
#'
#' @param x An rtemis object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A plotly object along with console output.
#'
#' @author EDG
#' @export
present <- new_generic("present", "x")

#' Plot ROC curve
#'
#' @description
#' This generic is used to plot the ROC curve for a model.
#'
#' @param x Classification or ClassificationRes object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A plotly object containing the ROC curve.
#'
#' @author EDG
#' @export
plot_roc <- new_generic("plot_roc", "x")

# Get parameters that need tuning.
get_params_need_tuning <- new_generic("get_params_need_tuning", "x")
# Get parameters.
get_params <- new_generic("get_params", c("x", "param_names"))
# Extract rules from a model.
extract_rules <- new_generic("extract_rules", "x")

# S3 Classes ----
class_data.table <- new_S3_class("data.table")
class_lgb.Booster <- new_S3_class("lgb.Booster")

#' Custom S7 validators
#'
#' @description
#' A collection of custom S7 validators used in rtemis.
#'
#' @keywords internal
#' @noRd
scalar_dbl <- S7::new_property(
  class = S7::class_double | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar double."
      } else if (!is.double(value)) {
        "must be double."
      }
    }
  }
) # /scalar_dbl

#' @keywords internal
#' @noRd
scalar_dbl_01excl <- S7::new_property(
  class = S7::class_double | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar double."
      } else if (value <= 0 || value >= 1) {
        "must be between > 0 and < 1."
      }
    }
  }
) # /scalar_dbl_01excl

#' @keywords internal
#' @noRd
scalar_dbl_01incl <- S7::new_property(
  class = S7::class_double | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar double."
      } else if (value < 0 || value > 1) {
        "must be between >= 0 and <= 1."
      }
    }
  }
) # /scalar_dbl_01incl

#' @keywords internal
#' @noRd
scalar_int <- S7::new_property(
  class = S7::class_integer | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar integer."
      }
    }
  }
) # /scalar_int

#' @keywords internal
#' @noRd
scalar_int_pos <- S7::new_property(
  class = S7::class_integer | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a positive integer scalar."
      } else if (value < 0) {
        "must be >= 0."
      }
    }
  }
) # /scalar_int_pos


#' @keywords internal
#' @noRd
scalar_int_12 <- S7::new_property(
  class = S7::class_integer | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a positive integer scalar."
      } else if (!value %in% 1:2) {
        "must be 1 or 2."
      }
    }
  }
) # /scalar_int_12

# data.frame data.table compatibility ----
#' Select columns by character or numeric vector.
#'
#' @param x data.frame or similar.
#'
#' @return data.frame, tibble, or data.table.
#'
#' @keywords internal
#' @noRd
.. <- identity
inc <- new_generic("inc", "x")
exc <- new_generic("exc", c("x", "idx"))
method(inc, class_data.frame) <- function(x, idx) {
  x[, idx, drop = FALSE]
}
# may cause R CMD check note, consider defining `..` or using the `with = FALSE` approach instead.
method(inc, class_data.table) <- function(x, idx) {
  x[, ..idx]
}

method(exc, list(class_data.frame, class_character)) <- function(x, idx) {
  x[, -which(names(x) %in% idx), drop = FALSE]
}
method(exc, list(class_data.frame, class_integer)) <- function(x, idx) {
  x[, -idx, drop = FALSE]
}
method(exc, list(class_data.frame, class_double)) <- function(x, idx) {
  idx <- clean_int(idx)
  x[, -idx, drop = FALSE]
}
method(
  exc,
  list(class_data.table, class_character | class_integer)
) <- function(x, idx) {
  x[, !..idx]
}
method(exc, list(class_data.table, class_double)) <- function(x, idx) {
  idx <- clean_int(idx)
  x[, !idx]
}

#' Get the name of the last column
#'
#' @param x data.frame or similar.
#'
#' @return Name of the last column.
#'
#' @keywords internal
#' @noRd
outcome_name <- new_generic("outcome_name", "x")
method(outcome_name, class_data.frame) <- function(x) {
  names(x)[NCOL(x)]
}

#' Get last column as a vector
#'
#' @param x data.frame or similar.
#' @param ... Not used.
#'
#' @return Vector containing the last column of `x`.
#'
#' @author EDG
#' @export
outcome <- new_generic("outcome", "x")
method(outcome, class_data.frame) <- function(x) {
  x[[NCOL(x)]]
}

#' Get features (all columns except the last one)
#'
#' @param x data.frame or similar.
#' @param ... Not used.
#'
#' @return object same as input, after removing the last column.
#'
#' @author EDG
#' @export
features <- new_generic("features", "x")
method(features, class_data.frame) <- function(x) {
  stopifnot(NCOL(x) > 1)
  x[, 1:(NCOL(x) - 1), drop = FALSE]
}

feature_names <- new_generic("feature_names", "x")
method(feature_names, class_data.frame) <- function(x) {
  names(x)[1:(NCOL(x) - 1)]
}

#' Get factor names
#'
#' @param x data.frame or similar.
#'
#' @return Character vector of factor names.
#'
#' @keywords internal
#' @noRd
get_factor_names <- new_generic("get_factor_names", "x")
method(get_factor_names, class_data.frame) <- function(x) {
  names(x)[sapply(x, is.factor)]
}
