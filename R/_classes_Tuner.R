# _classes_Tuner.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# R6: https://r6.r-lib.org
# R6 objects containing other R6 objects:
#   https://r6.r-lib.org/articles/Introduction.html#fields-containing-reference-objects
# roxygen2 for R6: https://roxygen2.r-lib.org/articles/rd-other.html#r6

# Tuner ----
Tuner <- R6::R6Class(
  classname = "Tuner",
  public = list(
    type = NULL,
    resample_params = NULL,
    # Initialize Tuner ----
    #' @description
    #' Initialize `Tuner` object
    #'
    #' @param type Character: Type of tuner
    #' @param resample_params List: resampling parameters
    initialize = function(type, resample_params = setup_resample()) {
      self$type <- type
      self$resample_params <- resample_params
    }, # /initialize
    # Print ----
    #' @description
    #' Print `Tuner` object
    #'
    print = function() {
      objcat(c(self$type, "Tuner"))
      printls(self$resample_params, title = "Resampling")
    } # /print
  )
) # /Tuner

#' Setup Tuner
#'
#' @inheritParams Tuner
#' @export
setup_tuner <- function(
    type = NULL,
    resample_params = setup_resample(resampler = "kfold", n_resamples = 5)) {
  Tuner$new(type = type, resample_params = resample_params)
} # rtemis::setup_tuner
