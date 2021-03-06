# u.KMEANS.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' K-means Clustering
#'
#' Perform K-means clustering using \code{flexclust::cclust}
#'
#' @param x Input matrix / data.frame
#' @param x.test Testing set matrix / data.frame
#' @param k Integer: Number of clusters to get
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'
#' @param verbose Logical: If TRUE, print messages to screen
#' @param ... Additional parameters to be passed to \code{flexclust::cclust}
#' @author E.D. Gennatas
#' @family Clustering
#' @export

u.KMEANS <- function(x, x.test = NULL,
                     k = 2,
                     dist = "euclidean",
                     verbose = TRUE, ...) {

  # [ Intro ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "KMEANS"

  # [ Data ] ====
  .colnames <- if (is.null(colnames(x))) paste0("Feature_", seq(NCOL(x))) else (colnames(x))
  x <- as.data.frame(x)
  xnames <- colnames(x) <- .colnames

  # [ Dependencies ] ====
  if (!depCheck("flexclust", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
  if (missing(x)) {
    print(args(u.KMEANS))
    stop("x is missing")
  }

  # [ KMEANS ] ====
  if (verbose) msg("Performing K-means Clustering with k = ", k, "...", sep = "")
  clust <- flexclust::cclust(x,
                             k = k,
                             dist = dist,
                             method = "kmeans", ...)

  # [ Clusters ] ====
  clusters.train <- flexclust::clusters(clust)
  if (!is.null(x.test)) {
    clusters.test <- flexclust::clusters(clust, x.test)
  } else {
    clusters.test <- NULL
  }

  # [ Outro ] ====
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = clusters.test,
                    parameters = list(k = k,
                                      dist = dist),
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.KMEANS
