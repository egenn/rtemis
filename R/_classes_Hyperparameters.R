# rtemis Hyperparameters Class
# ::rtemis::
# 2025 EDG rtemis.org

# References ----
# R6: https://r6.r-lib.org
# R6 objects containing other R6 objects:
#   https://r6.r-lib.org/articles/Introduction.html#fields-containing-reference-objects
# roxygen2 for R6: https://roxygen2.r-lib.org/articles/rd-other.html#r6

# `tuned` values ----
# -2: init (unknown)
# -1: being tuned
# 0: untuned but tunable
# 1: untuned, not tunable
# 2: tuned

Hyperparameters <- R6::R6Class(
  classname = "Hyperparameters",
  public = list(
    algorithm = NULL,
    tuned = NULL,
    # Initialize ----
    #' @description
    #' Initialize `Hyperparameters` object
    #'
    #' @param algorithm Character: Algorithm name
    #' @param hyperparameters List of algorithm hyperparameters
    initialize = function(algorithm = character(),
                          tuned = -2) {
      self$algorithm <- algorithm
      self$tuned <- tuned
    }, # /initialize
    # Update ----
    #' @description
    #' Update hyperparameter values
    #'
    #' @param hyperparameters Named list of algorithm hyperparameter values
    # Update the hyperparameters in the input
    update = function(hyperparameters = list()) {
      for (hp in names(hyperparameters)) {
        self[[hp]] <- hyperparameters[[hp]]
      }
    }, # /update
    # Freeze ----
    #' @description
    #' Freeze hyperparameter values while tuning (`tuned = -1`).
    #'
    freeze = function() {
      self$tuned <- -1
    }, # /freeze
    # Lock ----
    #' @description
    #' Lock hyperparameter values when untunable (`tuned = 1`).
    #'
    lock = function() {
      self$tuned <- 1
    } # /lock
  ) # /public
) # rtemis::Hyperparameters

Classification_fixed <- list(
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL
)

## CART ----

# CART_tunable <- list(
#   cp = 0.01,
#   maxdepth = 20,
#   minsplit = 2,
#   minbucket = 1, # round(minsplit / 3),
#   prune.cp = NULL
# )

# CART_fixed <- list(
#   method = "auto",
#   model = TRUE,
#   maxcompete = 0,
#   maxsurrogate = 0,
#   usesurrogate = 2,
#   surrogatestyle = 0,
#   xval = 0,
#   cost = NULL
# )

CART_tunable <- c("cp", "maxdepth", "minsplit", "minbucket", "prune.cp")
CART_fixed <- c(
  "method", "model", "maxcompete", "maxsurrogate", "usesurrogate", "surrogatestyle",
  "xval", "cost"
)

CARTHyperparameters <- R6::R6Class(
  classname = "CARTHyperparameters",
  inherit = Hyperparameters,
  public = list(
    # CART - tunable
    cp = NULL,
    maxdepth = NULL,
    minsplit = NULL,
    minbucket = NULL,
    prune.cp = NULL,
    method = NULL,
    # CART - fixed
    model = NULL,
    maxcompete = NULL,
    maxsurrogate = NULL,
    usesurrogate = NULL,
    surrogatestyle = NULL,
    xval = NULL,
    cost = NULL,
    # Initialize ----
    #' @description
    #' Initialize `CARTHyperparameters` object
    #'
    initialize = function(cp = 0.01,
                          maxdepth = 20,
                          minsplit = 2,
                          minbucket = 1, # round(minsplit / 3),
                          prune.cp = NULL,
                          method = "auto",
                          model = TRUE,
                          maxcompete = 0,
                          maxsurrogate = 0,
                          usesurrogate = 2,
                          surrogatestyle = 0,
                          xval = 0,
                          cost = NULL) {
      super$initialize(
        algorithm = "CART",
        tuned = -2
      )
      self$cp <- cp
      self$maxdepth <- maxdepth
      self$minsplit <- minsplit
      self$minbucket <- minbucket
      self$prune.cp <- prune.cp
      self$method <- method
      self$model <- model
      self$maxcompete <- maxcompete
      self$maxsurrogate <- maxsurrogate
      self$usesurrogate <- usesurrogate
      self$surrogatestyle <- surrogatestyle
      self$xval <- xval
      self$cost <- cost
    }, # /initialize
    # Print ----
    #' @description
    #' Print `CARTHyperparameters` object
    #'
    print = function() {
      objcat(paste(self$algorithm, "Hyperparameters"))
      hpl <- lapply(c(CART_tunable, CART_fixed), function(x) self[[x]])
      names(hpl) <- c(CART_tunable, CART_fixed)
      printls(hpl)
    } # /print
  )
) # rtemis::CARTHyperparameters


#' Generic function to check if Hyperparameters need tuning
#' 
#' @param parameters object
#' 
#' @return Logical: TRUE if parameters need tuning
#' 
#' @export
needs_tuning <- function(parameters) UseMethod("needs_tuning")

#' Check if Hyperparameters need tuning
#'
#' @param hyperparameters `Hyperparameters` object
#'
#' @return Logical: TRUE if hyperparameters need tuning
#'
#' @export
needs_tuning.Hyperparameters <- function(hyperparameters) {
  hyperparameters$tuned == 0
} # rtemis::hyperparameters_need_tuning
