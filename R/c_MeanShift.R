# c_MEANSHIFT.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' Mean Shift Clustering
#'
#' Perform [Mean Shift](https://en.wikipedia.org/wiki/Mean_shift) clustering using `meanShiftR::meanShift`
#'
#' @param x Input matrix
#' @param nNeighbors Integer: Number of neighbors to consider for kernel
#' density estimate
#' @param algorithm Character: "LINEAR" or "KDTREE"
#' @param kernelType Character: "NORMAL", "EPANECHNIKOV", "BIWEIGHT"
#' @param bandwidth Numeric vector, length = ncol(x): Use in kernel density
#' estimation for steepest ascent classification.
#' @param alpha Numeric: A scalar tuning parameter for normal kernels. When
#' this parameter is set to zero, the mean shift algorithm will operate as
#' usual. When this parameter is set to one, the mean shift algorithm will be
#' approximated through Newton's Method. When set to a value between zero and
#' one, a generalization of Newton's Method and mean shift will be used
#' instead providing a means to balance convergence speed with stability.
#' @param iterations Integer: Number of iterations to perform
#' @param epsilon Numeric: used to determine when to terminate the iteration of
#' an individual query point. If the distance between the query point at
#' iteration i and i+1 is less than epsilon, then iteration ceases on this point.
#' @param epsilonCluster Numeric: Used to determine the minimum distance between
#' distinct clusters. This distance is applied after all iterations have
#' finished and in order of the rows of queryData.
#' @param parameters A scalar or vector of paramters used by the specific
#' algorithm. There are no optional parameters for the "LINEAR" method,
#' "KDTREE" supports optional parameters for the maximum number of points to
#' store in a leaf node and the maximum value for the quadratic form in the
#' normal kernel, ignoring the constant value -0.5.
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional parameters to be passed to `flexclust::cclust`
#'
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_MeanShift <- function(
  x,
  # x.test = NULL,
  nNeighbors = NROW(x),
  algorithm = c("LINEAR", "KDTREE"),
  kernelType = c("NORMAL", "EPANECHNIKOV", "BIWEIGHT"),
  bandwidth = rep(1, NCOL(x)),
  alpha = 0,
  iterations = 10,
  epsilon = 1e-08,
  epsilonCluster = 1e-04,
  parameters = NULL,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "MeanShift"

  # Data ----
  .colnames <- if (is.null(colnames(x))) {
    paste0("Feature_", seq_len(NCOL(x)))
  } else {
    colnames(x)
  }
  x <- as.matrix(x)
  xnames <- colnames(x) <- .colnames

  # Dependencies ----
  dependency_check("meanShiftR")

  # Arguments ----
  if (missing(x)) {
    print(args(c_MeanShift))
    stop("x is missing")
  }
  algorithm <- match.arg(algorithm)
  kernelType <- match.arg(kernelType)

  # Mean Shift ----
  if (verbose) msg2("Performing Mean Shift Clustering...", sep = "")
  clust <- meanShiftR::meanShift(
    queryData = x,
    trainData = x,
    nNeighbors = nNeighbors,
    algorithm = algorithm,
    kernelType = kernelType,
    bandwidth = bandwidth,
    alpha = alpha,
    iterations = iterations,
    epsilon = epsilon,
    epsilonCluster = epsilonCluster,
    parameters = parameters
  )

  # Clusters ----
  clusters.train <- as.integer(clust$assignment)

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    # k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    # clusters.test = clusters.test,
    parameters = list(
      nNeighbors = nNeighbors,
      algorithm = algorithm,
      kernelType = kernelType,
      bandwidth = bandwidth,
      alpha = alpha,
      iterations = iterations,
      epsilon = epsilon,
      epsilonCluster = epsilonCluster,
      parameters = parameters
    ),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_MEANSHIFT
