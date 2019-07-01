# u.HOPACH.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Hieararchical Ordered Partitioning and Collapsing Hybrid
#'
#' Perform HOPACH clustering using \code{hopach::hopach}
#'
#' @inheritParams u.KMEANS
#' @param x Input matrix / data.frame
#' @param dmat Matrix (numeric, no missing values) or \code{hdist} object of pairwise distances.
#' If NULL
#' @param metric String: Dissimilarity metric to be used. Options: 'euclidean', 'manhattan'
#' @param K Integer, (0:15]: Maximum number of levels
#' @param kmax Integer, [1:9]: Maximum number of children at each node in the tree
#' @param khigh Integer, [1:9]: Maximum number of children at each nod ein the tree when computing the
#' the Mean/Median Split Silhouette. Usually same as \code{kmax}
#' @param ... Additional parameters to be passed to \code{cluster::hopach}
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

u.HOPACH <- function(x,
                     dmat = NULL,
                     metric = c("cosangle", "abscosangle", "euclid", "abseuclid", "cor", "abscor"),
                     K = 15,
                     kmax = 9,
                     khigh = 9,
                     verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = FALSE)
  clust.name <- "HOPACH"

  # [ ARGUMENTS ] ====
  metric <- match.arg(metric)

  # [ DATA ] ====
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # [ DEPENDENCIES ] ====
  if (!depCheck("hopach", verbose = verbose)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ HOPACH ] ====
  if (verbose) msg("Running HOPACH clustering...")
  clust <- hopach::hopach(x,
                          dmat = dmat,
                          d = metric,
                          K = K,
                          kmax = kmax,
                          khigh = khigh,
                          verbose = verbose, ...)
  if (verbose) msg("HOPACH identified ", clust$clustering$k, " clusters (sizes: ",
      paste(clust$clustering$sizes, collapse = ", "), ")", sep = "")

  # [ CLUSTERS ] ====
  clusters.train <- clust$clustering$labels

  # [ OUTRO ] ====
  cl <- rtClust$new(clust.name = clust.name,
                    k = length(unique(clusters.train)),
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = NULL,
                    parameters = list(K = K,
                                      kmax = kmax,
                                      khigh = khigh),
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.HOPACH
