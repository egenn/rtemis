# rtXDecom.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' R6 class for \pkg{rtemis} cross-decompositions
#'
#' \pkg{rtemis} cross-decomposition R6 object
#'
#' @docType class
#' @name rtXDecom-class
#' @field xdecom.name Character: Name of cross-decomposition algorithm
#' @field k Integer: Number of projections
#' @field xnames Character vector: Column names of x
#' @field xdecom Cross-decomposition model output
#' @field projections.train Input data projected on new axes / basis
#' @field projections.test Input test data projected on new axes / basis
#' @field extra List: Algorithm-specific output
#' @author E.D. Gennatas
#' @export

rtXDecom <- R6::R6Class("rtXDecom",
                        public = list(
                          ### Attributes
                          xdecom.name = NULL,
                          k = NULL,
                          xnames = NULL,
                          znames = NULL,
                          xdecom = NULL,
                          xprojections.train = NULL,
                          xprojections.test = NULL,
                          zprojections.train = NULL,
                          zprojections.test = NULL,
                          parameters = NULL,
                          extra = NULL,
                          ### Initialize
                          initialize = function(xdecom.name = character(),
                                                k = integer(),
                                                xnames = character(),
                                                znames = character(),
                                                xdecom = list(),
                                                xprojections.train = numeric(),
                                                xprojections.test = numeric(),
                                                zprojections.train = numeric(),
                                                zprojections.test = numeric(),
                                                parameters = list(),
                                                extra = list()) {
                            self$xdecom.name <- xdecom.name
                            self$k <- k
                            self$xnames <- xnames
                            self$znames <- znames
                            self$xdecom <- xdecom
                            self$xprojections.train <- xprojections.train
                            self$xprojections.test <- xprojections.test
                            self$zprojections.train <- zprojections.train
                            self$zprojections.test <- zprojections.test
                            self$parameters <- parameters
                            self$extra <- extra
                          },
                          ### Methods
                          print = function() {
                            "show / print method for rtXDecom"
                            objcat("Cross-Decomposition object")
                            cat(rtHighlight$bold(self$xdecom.name), " (", xdecomSelect(self$xdecom.name, desc = TRUE),
                                ")\n", sep = "")
                            if (length(self$parameters) > 0) printls(self$parameters,
                                                                     title = "Parameters",
                                                                     newline.pre = TRUE)
                          }
                        ))
