# c_CMEANS.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Fuzzy C-means Clustering
#'
#' Perform [fuzzy C-means clustering](https://en.wikipedia.org/wiki/Fuzzy_clustering) using `e1071::cmeans`
#'
#' @param x Input data
#' @param k Integer: Number of clusters to get. Default = 2
#' @param iter.max Integer: Maximum number of iterations. Default = 100
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'. Default = "euclidean"
#' @param method Character: "cmeans" - fuzzy c-means clustering; "ufcl": on-line update. Default = "cmeans"
#' @param m Float (>1): Degree of fuzzification. Default = 2
#' @param rate.par Float (0, 1): Learning rate for the online variant. (Default = .3)
#' @param weights Float (>0): Case weights
#' @param control List of control parameters. See `e1071::cmeans`
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional parameters to be passed to `e1071::cmeans`
#'
#' @author E.D. Gennatas
#' @return `rtClust` object
#' @family Clustering
#' @export

c_CMeans <- function(
  x,
  k = 2,
  iter.max = 100,
  dist = "euclidean",
  method = "cmeans",
  m = 2,
  rate.par = NULL,
  weights = 1,
  control = list(),
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "CMeans"

  # Data ----
  .colnames <- if (is.null(colnames(x))) {
    paste0("Feature_", seq_len(NCOL(x)))
  } else {
    (colnames(x))
  }
  x <- as.data.frame(x)
  xnames <- colnames(x) <- .colnames

  # Dependencies ----
  dependency_check("e1071")

  # CMeans ----
  if (verbose) msg20("Running Fuzzy C-means clustering with k = ", k, "...")
  clust <- e1071::cmeans(
    x,
    centers = k,
    iter.max = iter.max,
    dist = dist,
    method = "cmeans",
    m = m,
    rate.par = rate.par,
    weights = weights,
    control = control,
    ...
  )

  # Clusters ----
  clusters.train <- clust$cluster
  clusters.test <- NULL

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = clusters.test,
    parameters = list(k = k, m = m),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_CMEANS
