# rtClust.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org

#' R6 class for \pkg{rtemis} clustering
#'
#' \pkg{rtemis} clustering object
#'
#' @docType class
#' @name rtClust-class
#' @field clust.name Character: Name of clustering algorithm
#' @field k Integer: Number of clusters
#' @field xnames Column names of x
#' @field clust Clustering algorithm output
#' @field clusters.train Cluster assignment for training set
#' @field clusters.test Cluster assignment for testing set (if supported by algorithm and test set provided)
#' @field parameters List of clustering algorithm parameters
#' @field extra List: Algorithm-specific output
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

rtClust <- R6::R6Class(
  "rtClust",
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
    #' @description
    #' Create a new rtClust object
    #' @param clust.name Character: Clustering algorithm name
    #' @param k Integer: Number of cluster
    #' @param xnames Character vector: feature names
    #' @param clust Clustering object
    #' @param clusters.train Training set clustering results
    #' @param clusters.test Testing set clustering results
    #' @param parameters List of clustering algorithm parameters
    #' @param extra Optional list of algorithm-specific info
    initialize = function(
      clust.name = character(),
      k = NULL,
      xnames = character(),
      clust = list(),
      clusters.train = numeric(),
      clusters.test = numeric(),
      parameters = list(),
      extra = list()
    ) {
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
    #' @description
    #' Print method for `rtClust` objects
    print = function() {
      "show / print method for rtClust"
      objcat("Clustering object")
      cat(
        hilite(self$clust.name),
        " (",
        select_clust(self$clust.name, desc = TRUE),
        ")\n",
        sep = ""
      )
      if (length(self$parameters) > 0) {
        printls(self$parameters, title = "Parameters", newline.pre = TRUE)
      }
    }
  )
) # /rtClust

# rtClust S3 methods ----

#' rtClust S3 methods
#'
#' S3 methods for `rtClust` class.
#'
#' @name rtClust-methods
NULL


#' `print.rtClust`: `print` method for `rtClust` object
#'
#' @param x `rtClust` object
#' @param ... Not used
#'
#' @method print rtClust
#' @rdname rtClust-methods
#'
#' @export
print.rtClust <- function(x, ...) {
  x$print()
  invisible(x)
} # rtemis::print.rtClust
