# u.HARDCL.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Clustering by Hard Competitive Learning
#'
#' Perform clustering by Hard Competitive Learning using \code{flexclust::cclust}
#'
#' @inheritParams u.KMEANS
#' @param x Input matrix / data.frame
#' @param k Integer: Number of clusters to get
#' @param dist String: Distance measure to use: 'euclidean' or 'manhattan'
#' @param ... Additional parameters to be passed to \code{flexclust::cclust}
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

u.HARDCL <- function(x, x.test = NULL,
                     k = 2,
                     dist = "euclidean",
                     verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "HARDCL"

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
    print(args(u.HARDCL))
    stop("x is missing")
  }

  # [ CCLUST ] ====
  if (verbose) msg0("Running Hard Competitive Learning with k = ", k, "...")
  clust <- flexclust::cclust(x,
                             k = k,
                             dist = dist,
                             method = "hardcl", ...)

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
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.HARDCL
