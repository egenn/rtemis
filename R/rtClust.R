# rtClust.R
# ::rtemis::
# 2016-8 Efstathios D. Gennatas egenn.github.io

#' R6 class for \pkg{rtemis} clustering
#'
#' \pkg{rtemis} clustering object
#'
#' @docType class
#' @name rtClust-class
#' @field clust.name String: Name of clustering algorithm
#' @field call Originating call
#' @field xnames Column names of x
#' @field clust Clustering algorithm output
#' @field clusters.train Cluster assignment for training set
#' @field clusters.test Cluster assignment for testing set (if supported by algorithm and test set provided)
#' @field extra List: Algorithm-specific output
#' @author Efstathios D. Gennatas
#' @export

rtClust <- R6::R6Class("rtClust",
                        public = list(
                          ### Attributes
                          clust.name = NULL,
                          k = NULL,
                          xnames = NULL,
                          clust = NULL,
                          clusters.train = NULL,
                          clusters.test = NULL,
                          parameters = NULL,
                          extra = NULL,
                          ### Initialize
                          initialize = function(clust.name = character(),
                                                k = NULL,
                                                xnames = character(),
                                                clust = list(),
                                                clusters.train = numeric(),
                                                clusters.test = numeric(),
                                                parameters = list(),
                                                extra = list()) {
                            self$clust.name <- clust.name
                            self$k <- k
                            self$xnames <- xnames
                            self$clust <- clust
                            self$clusters.train <- clusters.train
                            self$clusters.test <- clusters.test
                            self$parameters <- parameters
                            self$extra <- extra
                          },
                          ### Methods
                          print = function() {
                            "show / print method for rtClust"
                            cat(".:rtemis Clustering \n------------------------------------------------------\n")
                            cat(self$clust.name, " (", clustSelect(self$clust.name, desc = TRUE),
                                ")\n", sep = "")
                            cat("------------------------------------------------------\n")
                            printls(self$parameters)
                            cat("------------------------------------------------------\n")
                          }
                        )) # /rtClust

# rtClust S3 methods ====

#' rtClust S3 methods
#'
#' S3 methods for \code{rtClust} class.
#'
#' @name rtClust-methods
NULL


#' \code{print.rtClust}: \code{print} method for \code{rtClust} object
#'
#' @method print rtClust
#' @rdname rtClust-methods
#' @export
print.rtClust <- function(x, ...) {
  
  x$print()
  
} # rtemis::print.rtClust
