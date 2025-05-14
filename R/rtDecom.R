# rtDecom.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' R6 Class for \pkg{rtemis} Decompositions
#'
#' \pkg{rtemis} decomposition R6 object
#'
#' @docType class
#' @name rtDecom-class
#'
#' @field decom.name Character: Name of decomposition algorithm
#' @field xnames Character vector: Column names of x
#' @field decom Decomposition model output
#' @field parameters List of decompotision parameters
#' @field center Numeric vector of column means if centering was applied using `scale()`
#' prior to decomposition
#' @field scale Numeric vector of column scale factor if scaling was applied using `scale()`
#' prior to decomposition
#' @field projections.train Input data projected on new axes / basis
#' @field projections.test Input test data projected on new axes / basis
#' @field extra List: Algorithm-specific output
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

rtDecom <- R6::R6Class(
  "rtDecom",
  public = list(
    ### Attributes
    decom.name = NULL,
    # call = NULL,
    xnames = NULL,
    decom = NULL,
    parameters = NULL,
    center = NULL,
    scale = NULL,
    projections.train = NULL,
    projections.test = NULL,
    extra = NULL,
    ### Initialize
    #' @description
    #' Initialize `rtDecom` object
    #' @param decom.name Character: Decomposition algorithm name
    #' @param xnames Character vector: feature names
    #' @param decom Decomposition object
    #' @param parameters list of decomposition algorithm parameters
    #' @param center Numeric vector of column means if centering was applied using `scale()`
    #' prior to decomposition
    #' @param scale Numeric vector of column scale factor if scaling was applied using `scale()`
    #' prior to decomposition
    #' @param projections.train Training set projections
    #' @param projections.test Testing set projections
    #' @param extra Optional list of algorithm-specific info
    initialize = function(
      decom.name = character(),
      # call = call("NULL"),
      xnames = character(),
      decom = list(),
      parameters = list(),
      center = numeric(),
      scale = numeric(),
      projections.train = numeric(),
      projections.test = numeric(),
      extra = list()
    ) {
      self$decom.name <- decom.name
      # self$call <- call
      self$xnames <- xnames
      self$decom <- decom
      self$parameters <- parameters
      self$center <- center
      self$scale <- scale
      self$projections.train <- projections.train
      self$projections.test <- projections.test
      self$extra <- extra
    },
    ### Methods
    #' @description
    #' Print method for `rtDecom` objects
    print = function() {
      "show / print method for rtDecom"
      objcat("Decomposition object")
      cat(
        hilite(self$decom.name),
        " (",
        select_decom(self$decom.name, desc = TRUE),
        ")\n",
        sep = ""
      )
      if (length(self$parameters) > 0) {
        printls(self$parameters, title = "Parameters", newline.pre = TRUE)
      }
    }
  )
)


# #' R6 class for \pkg{rtemis} decomLearn
# #'
# #' \pkg{rtemis} decomLearn R6 object
# #'
# #' @docType class
# #' @name rtDecomLearn-class
# #' @usage # rtDecom$new(decom.name = ...) # initialize new object
# #' @field decom.name Character: Name of decomposition algorithm
# #' @field call Originating call
# #' @field xnames Character vector: Column names of x
# #' @field decom Decomposition model output
# #' @field projections.train Input data projected on new axes / basis
# #' @field projections.test Input test data projected on new axes / basis
# #' @field extra List: Algorithm-specific output
# #' @author E.D. Gennatas
# #' @export
# rtDecomLearn <- R6::R6Class("rtDecomLearn",
#                             public = list(
#                               ### Attributes
#                               mod.name = NULL,
#                               decom.name = NULL,
#                               decom.params = NULL,
#                               decom.param.grid = NULL,
#                               tuner.name = NULL,
#                               tuner.params = NULL,
#                               best.tune = NULL,
#                               learner.name = NULL,
#                               learner.params = NULL,
#                               xnames = NULL,
#                               decom = NULL,
#                               mod = NULL,
#                               projections.train = NULL,
#                               projections.test = NULL,
#                               extra = NULL,
#                               ### Initialize
#                               initialize = function(mod.name = character(),
#                                                     decom.name = character(),
#                                                     decom.params = list(),
#                                                     decom.param.grid = list(),
#                                                     tuner.name = character(),
#                                                     tuner.params = list(),
#                                                     best.tune = list(),
#                                                     learner.name = character(),
#                                                     learner.params = list(),
#                                                     xnames = character(),
#                                                     decom = list(),
#                                                     mod = list(),
#                                                     projections.train = numeric(),
#                                                     projections.test = numeric(),
#                                                     extra = list()) {
#                                 self$mod.name <- mod.name
#                                 self$decom.name <- decom.name
#                                 self$decom.params <- decom.params
#                                 self$decom.param.grid <- decom.param.grid
#                                 self$tuner.name <- tuner.name
#                                 self$tuner.params <- tuner.params
#                                 self$best.tune <- best.tune
#                                 self$learner.name <- learner.name
#                                 self$learner.params <- learner.params
#                                 self$xnames <- xnames
#                                 self$decom <- decom
#                                 self$mod <- mod
#                                 self$projections.train <- projections.train
#                                 self$projections.test <- projections.test
#                                 self$extra <- extra
#                               }
#                               ### Methods
#                             ))

#' `print.rtDecom`: `print` method for `rtDecom` object
#'
#' @param x `rtDecom` object
#' @param ... Not used
#'
#' @method print rtDecom
#' @rdname rtDecom-methods
#'
#' @export
print.rtDecom <- function(x, ...) {
  x$print()
  invisible(x)
} # rtemis::print.rtDecom
