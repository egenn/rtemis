# S7_Preprocessor.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# preprocessor_args <- c(
#   "factor2integer",
#   "scale",
#   "center",
#   "ifw",
#   "ifw.type",
#   "upsample",
#   "downsample",
#   "resample.seed"
# )

# Preprocessor ----
#' @title Preprocessor
#' 
#' @description
#' Preprocessor class.
#' 
#' @author EDG
#' @export
Preprocessor <- new_class(
  name = "Preprocessor",
  properties = list(
    factor2integer = class_logical,
    scale = class_logical,
    center = class_logical,
    ifw = class_logical,
    ifw.type = class_integer,
    upsample = class_logical,
    downsample = class_logical,
    resample.seed = class_integer | NULL
  )
) # /Preprocessor

#' Print `Preprocessor` object
#'
#' @param x `Preprocessor` object.
#' @param ... Ignored.
#'
#' @export
print.Preprocessor <- function(x, ...) {
  objcat("Preprocessor")
  printls(props(x))
}
method(print, Preprocessor) <- function(x, ...) {
  print.Preprocessor(x, ...)
}

#' Create a `Preprocessor` object
#'
#' @param factor2integer Logical: If TRUE, convert factors to integers.
#' @param scale Logical: If TRUE, scale numeric features.
#' @param center Logical: If TRUE, center numeric features.
#' @param ifw Logical: If TRUE, return class weights for inverse frequency
#' weighting for Classification.
#' @param ifw.type {1, 2}: Type of inverse frequency weighting for Classification.
#' @param upsample Logical: If TRUE, upsample minority class to match size of majority class.
#' @param downsample Logical: If TRUE, downsample majority class to match size of minority class.
#' @param resample.seed Integer: Seed for resampling.
#'
#' @author EDG
#' @export
setup_preprocessor <- function(
    factor2integer = FALSE,
    scale = FALSE,
    center = FALSE,
    ifw = FALSE,
    ifw.type = 2L,
    upsample = FALSE,
    downsample = FALSE,
    resample.seed = NULL) {
  Preprocessor(
    factor2integer = factor2integer,
    scale = scale,
    center = center,
    ifw = ifw,
    ifw.type = ifw.type,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed
  )
} # /setup_preprocessor
