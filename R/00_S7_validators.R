# S7_validators.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Custom S7 validators
#' 
#' @description
#' A collection of custom S7 validators used in rtemis.
#' 
#' @rdname S7_validators
#' @keywords internal
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

#' @rdname S7_validators
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

#' @rdname S7_validators
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

#' @rdname S7_validators
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

#' @rdname S7_validators
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


#' @rdname S7_validators
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
