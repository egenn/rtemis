# rtDecom.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' R6 Class for \pkg{rtemis} Decompositions
#'
#' \pkg{rtemis} decomposition R6 object
#'
#' @docType class
#' @name rtDecom-class
#' @field decom.name String: Name of decomposition algorithm
#' @field call Originating call
#' @field xnames Character vector: Column names of x
#' @field decom Decomposition model output
#' @field projections.train Input data projected on new axes / basis
#' @field projections.test Input test data projected on new axes / basis
#' @field extra List: Algorithm-specific output
#' @author Efstathios D Gennatas
#' @export

rtDecom <- R6::R6Class("rtDecom",
                        public = list(
                          ### Attributes
                          decom.name = NULL,
                          # call = NULL,
                          xnames = NULL,
                          decom = NULL,
                          parameters = NULL,
                          projections.train = NULL,
                          projections.test = NULL,
                          extra = NULL,
                          ### Initialize
                          initialize = function(decom.name = character(),
                                                # call = call("NULL"),
                                                xnames = character(),
                                                decom = list(),
                                                parameters = list(),
                                                projections.train = numeric(),
                                                projections.test = numeric(),
                                                extra = list()) {
                            self$decom.name <- decom.name
                            # self$call <- call
                            self$xnames <- xnames
                            self$decom <- decom
                            self$parameters <- parameters
                            self$projections.train <- projections.train
                            self$projections.test <- projections.test
                            self$extra <- extra
                          },
                          ### Methods
                          print = function() {
                            "show / print method for rtDecom"
                            cat(".:rtemis Decomposition\n------------------------------------------------------\n")
                            cat(self$decom.name, " (", decomSelect(self$decom.name, desc = TRUE),
                                ")\n", sep = "")
                            cat("------------------------------------------------------\n")
                            printls(self$parameters)
                            cat("------------------------------------------------------\n")
                          }
                        ))


#' R6 class for \pkg{rtemis} decomLearn
#'
#' \pkg{rtemis} decomLearn R6 object
#'
#' @docType class
#' @name rtDecomLearn-class
#' @usage # rtDecom$new(decom.name = ...) # initialize new object
#' @field decom.name String: Name of decomposition algorithm
#' @field call Originating call
#' @field xnames Character vector: Column names of x
#' @field decom Decomposition model output
#' @field projections.train Input data projected on new axes / basis
#' @field projections.test Input test data projected on new axes / basis
#' @field extra List: Algorithm-specific output
#' @author Efstathios D Gennatas
#' @export
rtDecomLearn <- R6::R6Class("rtDecomLearn",
                            public = list(
                              ### Attributes
                              mod.name = NULL,
                              decom.name = NULL,
                              decom.params = NULL,
                              decom.param.grid = NULL,
                              tuner.name = NULL,
                              tuner.params = NULL,
                              best.tune = NULL,
                              learner.name = NULL,
                              learner.params = NULL,
                              # call = NULL,
                              xnames = NULL,
                              decom = NULL,
                              mod = NULL,
                              projections.train = NULL,
                              projections.test = NULL,
                              extra = NULL,
                              ### Initialize
                              initialize = function(mod.name = character(),
                                                    decom.name = character(),
                                                    decom.params = list(),
                                                    decom.param.grid = list(),
                                                    tuner.name = character(),
                                                    tuner.params = list(),
                                                    best.tune = list(),
                                                    learner.name = character(),
                                                    learner.params = list(),
                                                    call = call("NULL"),
                                                    xnames = character(),
                                                    decom = list(),
                                                    mod = list(),
                                                    projections.train = numeric(),
                                                    projections.test = numeric(),
                                                    extra = list()) {
                                self$mod.name <- mod.name
                                self$decom.name <- decom.name
                                self$decom.params <- decom.params
                                self$decom.param.grid <- decom.param.grid
                                self$tuner.name <- tuner.name
                                self$tuner.params <- tuner.params
                                self$best.tune <- best.tune
                                self$learner.name <- learner.name
                                self$learner.params <- learner.params
                                # self$call <- call
                                self$xnames <- xnames
                                self$decom <- decom
                                self$mod <- mod
                                self$projections.train <- projections.train
                                self$projections.test <- projections.test
                                self$extra <- extra
                              }
                              ### Methods
                            ))
