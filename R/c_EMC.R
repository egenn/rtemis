# c_EMC.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Expectation Maximization Clustering
#'
#' Perform clustering by
#' [EM](https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm)
#' using `EMCluster::emcluster`
#'
#' First, `EMCluster::simple.init(x, nclass = k)` is run,
#' followed by `EMCluster::emcluster(x, emobj = emobj, assign.class = TRUE, ...)`
#'
#' This can be very slow.
#'
#' @inheritParams c_KMeans
#' @param lab Vector, length `NROW(x)`: Labels for semi-supervised clustering
#' @param EMC List of control parameters for `EMCluster::emcluster`. Default = `EMCluster::.EMC`
# @param maxiter Integer: Maximum number of iterations. Default = 100
#' @param ... Additional parameters to be passed to `EMCluster::emcluster`
#'
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_EMC <- function(
  x,
  x.test = NULL,
  k = 2,
  lab = NULL,
  EMC = EMCluster::.EMC,
  # maxiter = 100,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "EMC"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
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
  if (verbose) msg2("Initializing EM Clustering...")
  emobj <- EMCluster::simple.init(x, nclass = k)
  if (verbose) msg2("Performing EM Clustering...")
  clust <- EMCluster::emcluster(
    x,
    emobj = emobj,
    EMC = EMC,
    assign.class = TRUE,
    ...
  )

  # Clusters ----
  clusters.train <- clust$class

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = NULL,
    parameters = list(k = k, EMC = EMC),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_EMC
