# u.EMC.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Expectation Maximization Clustering
#'
#' Perform EM Clustering using \code{EMCluster::emcluster}
#'
#' First, \code{EMCluster::simple.init(x, nclass = k)} is run,
#' followed by \code{EMCluster::emcluster(x, emobj = emobj, assign.class = TRUE, ...)}
#'
#' This can be very slow.
#' @inheritParams u.KMEANS
#' @param lab Vector, length \code{NROW(x)}: Labels for semi-supervised clustering
#' @param EMC List of control parameters for \code{EMCluster::emcluster}. Default = \code{EMCluster::.EMC}
# @param maxiter Integer: Maximum number of iterations. Default = 100
#' @param ... Additional parameters to be passed to \code{EMCluster::emcluster}
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

u.EMC <- function(x, x.test = NULL,
                  k = 2,
                  lab = NULL,
                  EMC = EMCluster::.EMC,
                  # maxiter = 100,
                  verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "EMC"
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature.", 1:NCOL(x))
  xnames <- colnames(x)

  # [ DEPENDENCIES ] ====
  if (!depCheck("EMCluster", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(u.EMC))
    stop("x is missing")
  }

  # [ EMC ] ====
  if (verbose) msg("Initializing EM Clustering...")
  emobj <- EMCluster::simple.init(x, nclass = k)
  if (verbose) msg("Performing EM Clustering...")
  clust <- EMCluster::emcluster(x,
                                emobj = emobj,
                                EMC = EMC,
                                assign.class = TRUE, ...)

  # [ CLUSTERS ] ====
  clusters.train <- clust$class

  # [ OUTRO ] ====
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = NULL,
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.EMC
