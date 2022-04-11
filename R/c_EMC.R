# c_EMC.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Expectation Maximization Clustering
#'
#' Perform EM Clustering using \code{EMCluster::emcluster}
#'
#' First, \code{EMCluster::simple.init(x, nclass = k)} is run,
#' followed by \code{EMCluster::emcluster(x, emobj = emobj, assign.class = TRUE, ...)}
#'
#' This can be very slow.
#' @inheritParams c_KMEANS
#' @param lab Vector, length \code{NROW(x)}: Labels for semi-supervised clustering
#' @param EMC List of control parameters for \code{EMCluster::emcluster}. Default = \code{EMCluster::.EMC}
# @param maxiter Integer: Maximum number of iterations. Default = 100
#' @param ... Additional parameters to be passed to \code{EMCluster::emcluster}
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_EMC <- function(x, x.test = NULL,
                  k = 2,
                  lab = NULL,
                  EMC = EMCluster::.EMC,
                  # maxiter = 100,
                  verbose = TRUE, ...) {

  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "EMC"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("EMCluster")

  # Arguments ----
  if (missing(x)) {
    print(args(c_EMC))
    stop("x is missing")
  }

  # EMC ----
  if (verbose) msg("Initializing EM Clustering...")
  emobj <- EMCluster::simple.init(x, nclass = k)
  if (verbose) msg("Performing EM Clustering...")
  clust <- EMCluster::emcluster(x,
                                emobj = emobj,
                                EMC = EMC,
                                assign.class = TRUE, ...)

  # Clusters ----
  clusters.train <- clust$class

  # Outro ----
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = NULL,
                    parameters = list(k = k, EMC = EMC),
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::c_EMC
