# u.NGAS.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Neural Gas Clustering
#'
#' Perform Neural Gas clustering using \code{flexclust::cclust}
#'
#' @inheritParams u.KMEANS
#' @param x Input matrix / data.frame
#' @param k Integer: Number of clusters to get
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'
#' @param ... Additional parameters to be passed to \code{flexclust::cclust}
#' @author E.D. Gennatas
#' @return \link{rtClust} object
#' @family Clustering
#' @export

u.NGAS <- function(x, x.test = NULL,
                   k = 2,
                   dist = "euclidean",
                   verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "NGAS"

  # [ DATA ] ====
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # [ DEPENDENCIES ] ====
  if (!depCheck("flexclust", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(u.NGAS))
    stop("x is missing")
  }

  # [ NGAS ] ====
  if (verbose) msg("Performing Neural Gas clustering with k = ", k, "...", sep = "")
  clust <- flexclust::cclust(x,
                             k = k,
                             dist = dist,
                             method = "neuralgas", ...)

  # [ CLUSTERS ] ====
  clusters.train <- flexclust::clusters(clust)
  if (!is.null(x.test)) {
    clusters.test <- flexclust::clusters(clust, x.test)
  } else {
    clusters.test <- NULL
  }

  # [ OUTRO ] ====
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = clusters.test,
                    parameters = list(k = k, dist = dist),
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.NGAS
