# u.MEANSHIFT.R
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

#' Mean Shift Clustering
#'
#' Perform Mean Shift clustering using \code{meanShiftR::meanShift}
#'
#' @param x Input matrix
#' @param verbose Logical: If TRUE, print messages to screen
#' @param ... Additional parameters to be passed to \code{flexclust::cclust}
#' @author E.D. Gennatas
#' @family Clustering
#' @export

u.MEANSHIFT <- function(x, 
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
                verbose = TRUE, ...) {

  # Intro ====
  start.time <- intro(verbose = verbose)
  clust.name <- "MEANSHIFT"

  # Data ====
  .colnames <- if (is.null(colnames(x))) paste0("Feature_", seq(NCOL(x))) else (colnames(x))
  x <- as.matrix(x)
  xnames <- colnames(x) <- .colnames

  # Dependencies ====
  dependency_check("meanShiftR")

  # Arguments ====
  if (missing(x)) {
    print(args(u.MEANSHIFT))
    stop("x is missing")
  }
  algorithm <- match.arg(algorithm)
  kernelType <- match.arg(kernelType)

  # Mean Shift ====
  if (verbose) msg("Performing Mean Shift Clustering...", sep = "")
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
                parameters = parameters)

  # Clusters ====
  clusters.train <- as.integer(clust$assignment)

  # Outro ====
  cl <- rtClust$new(clust.name = clust.name,
                    # k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    # clusters.test = clusters.test,
                    parameters = list(nNeighbors = nNeighbors,
                                        algorithm = algorithm,
                                        kernelType = kernelType,
                                        bandwidth = bandwidth,
                                        alpha = alpha,
                                        iterations = iterations,
                                        epsilon = epsilon,
                                        epsilonCluster = epsilonCluster,
                                        parameters = parameters),
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.MEANSHIFT
