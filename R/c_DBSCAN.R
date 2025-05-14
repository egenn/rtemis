# c_DBSCAN.R
# ::rtemis::
# E.D. Gennatas egenn.github.io

#' Density-based spatial clustering of applications with noise
#'
#' Perform [DBSCAN](https://en.wikipedia.org/wiki/DBSCAN) clustering
#'
#' See `dbscan::dbscan` for info on how to choose `eps` and
#' `minPts`
#'
#' @param x Input matrix / data.frame
#' @param x.test Testing set matrix / data.frame
#' @param eps Numeric: Radius of the epsilon neighborhood
#' @param minPts Integer: Number of minimum points required in the eps
#' neighborhood for core points (including the point itself).
#' @param weights Numeric vector: Data points' weights. Needed for weighted
#' clustering.
#' @param borderPoints Logical: If TRUE, assign border points to clusters,
#' otherwise they are considered noise
#' @param search Character: "kdtree", "linear" or "dist": nearest neighbor
#' search strategy
#' @param verbose Logical: If TRUE, print messages to screen
#' @param ... Additional parameters to be passed to `flexclust::cclust`
#'
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

c_DBSCAN <- function(
  x,
  x.test = NULL,
  eps = 1,
  minPts = NCOL(x) + 1,
  weights = NULL,
  borderPoints = TRUE,
  search = c("kdtree", "linear", "dist"),
  verbose = TRUE,
  ...
) {
  # Dependencies ----
  dependency_check("flexclust")

  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "DBSCAN"

  # Arguments ----
  if (missing(x)) {
    print(args(c_DBSCAN))
    stop("x is missing")
  }
  search <- match.arg(search)

  # Data ----
  .colnames <- if (is.null(colnames(x))) {
    paste0("Feature_", seq_len(NCOL(x)))
  } else {
    (colnames(x))
  }
  x <- as.matrix(x)
  xnames <- colnames(x) <- .colnames

  # DBSCAN ----
  if (verbose) msg2("Performing DBSCAN Clustering...")
  clust <- dbscan::dbscan(
    x,
    eps = eps,
    minPts = minPts,
    weights = weights,
    borderPoints = borderPoints,
    search = search,
    ...
  )

  # Clusters ----
  clusters.train <- clust$cluster
  if (!is.null(x.test)) {
    clusters.test <- predict(clust, newdata = as.matrix(x.test), data = x)
  } else {
    clusters.test <- NULL
  }

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = length(unique(clusters.train)),
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = clusters.test,
    parameters = c(
      eps = eps,
      minPts = minPts,
      weights = weights,
      borderPoints = borderPoints,
      search = search
    ),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_DBSCAN
