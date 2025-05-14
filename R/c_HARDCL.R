# c_HARDCL.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Clustering by Hard Competitive Learning
#'
#' Perform clustering by
#' [Hard Competitive Learning](https://en.wikipedia.org/wiki/Competitive_learning)
#' using `flexclust::cclust`
#'
#' @param x Input matrix / data.frame
#' @param x.test Optional test set data
#' @param k Integer: Number of clusters to get
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional parameters to be passed to `flexclust::cclust`
#'
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_HARDCL <- function(
  x,
  x.test = NULL,
  k = 2,
  dist = "euclidean",
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "HARDCL"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("flexclust")

  # Arguments ----
  if (missing(x)) {
    print(args(c_HARDCL))
    stop("x is missing")
  }

  # CCLUST ----
  if (verbose) msg20("Running Hard Competitive Learning with k = ", k, "...")
  clust <- flexclust::cclust(x, k = k, dist = dist, method = "hardcl", ...)

  # Clusters ----
  clusters.train <- flexclust::clusters(clust)
  if (!is.null(x.test)) {
    clusters.test <- flexclust::clusters(clust, x.test)
  } else {
    clusters.test <- NULL
  }

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = clusters.test,
    parameters = list(k = k, dist = dist),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_HARDCL
