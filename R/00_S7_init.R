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
# Full description for multi-line printing.
describe <- new_generic("describe", "x")
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
