# _classes_Preprocessor.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# -------------------------------------------------------------------------------------
# R6: https://r6.r-lib.org
# R6 objects containing other R6 objects:
#   https://r6.r-lib.org/articles/Introduction.html#fields-containing-reference-objects
# roxygen2 for R6: https://roxygen2.r-lib.org/articles/rd-other.html#r6

preprocessor_args <- c(
  "factor2integer",
  "scale",
  "center",
  "ifw",
  "ifw.type",
  "upsample",
  "downsample",
  "resample.seed"
)

# Preprocessor ----
Preprocessor <- R6::R6Class(
  classname = "Preprocessor",
  public = list(
    # Algorithm-specific
    factor2integer = NULL,
    scale = NULL,
    center = NULL,
    # Classification
    ifw = NULL,
    ifw.type = NULL,
    upsample = NULL,
    downsample = NULL,
    resample.seed = NULL,
    # Initialize ----
    #' @description
    #' Initialize `Preprocessors` object
    #'
    initialize = function(factor2integer = FALSE,
                          scale = FALSE,
                          center = FALSE,
                          ifw = FALSE,
                          ifw.type = 2,
                          upsample = FALSE,
                          downsample = FALSE,
                          resample.seed = NULL) {
      self$factor2integer <- factor2integer
      self$scale <- scale
      self$center <- center
      self$ifw <- ifw
      self$ifw.type <- ifw.type
      self$upsample <- upsample
      self$downsample <- Classification_fixed$downsample
      self$resample.seed <- Classification_fixed$resample.seed
    },
    # Print ----
    #' @description
    #' Print `Preprocessor` object
    #'
    print = function() {
      objcat("Preprocessor")
      # make publ a list with all public attributes
      publ <- lapply(preprocessor_args, function(x) self[[x]])
      names(publ) <- preprocessor_args
      printls(publ)
    }, # /print
    # Preprocess ----
    preprocess = function(x) {
      preprocess(x,
        factor2integer = self$factor2integer,
        scale = self$scale,
        center = self$center,
        ifw = self$ifw,
        ifw.type = self$ifw.type,
        upsample = self$upsample,
        downsample = self$downsample,
        resample.seed = self$resample.seed
      )
    } # /preprocess
  )
) # /Preprocessor

setup_preprocessor <- function(
    factor2integer = FALSE,
    scale = FALSE,
    center = FALSE,
    ifw = FALSE,
    ifw.type = 2,
    upsample = FALSE,
    downsample = FALSE,
    resample.seed = NULL) {
  Preprocessor$new(
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
