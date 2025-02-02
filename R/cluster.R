# cluster.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Cluster rows of a dataset
#' 
#' @param x Matrix or data.frame: Data to cluster. Rows are cases to be clustered.
#' @param algorithm Character: Clustering algorithm.
#' @param parameters List: Algorithm-specific parameters.
#' @param verbosity Integer: Verbosity level.
#' 
#' @return Clustering object
#' @author EDG
#' @export

cluster <- new_generic("cluster", "x")
method(cluster, class_numeric | class_data.frame) <- function(x,
                                                              algorithm = "KMeans",
                                                              parameters = NULL,
                                                              verbosity = 1L) {
  # Checks ----
  if (is.null(parameters)) {
    parameters <- get_default_clusterparams(algorithm)
  }
  check_is_S7(parameters, ClusteringParameters)

  # Intro ----
  start_time <- intro(verbosity = verbosity)

  # Data ----
  if (verbosity > 0L) {
    cat("\n")
    summarize_unsupervised_data(x)
    cat("\n")
  }

  # Cluster ----
  algorithm <- get_clust_name(algorithm)
  cluster_fn <- get_clust_fn(algorithm)
  if (verbosity > 0L) {
    msg20("Clustering with ", algorithm, "...\n")
  }
  clust <- do.call(
    cluster_fn,
    list(x = x, parameters = parameters)
  )

  # Outro ----
  outro(start_time, verbosity = verbosity)
  Clustering(
    algorithm = algorithm,
    clust = clust,
    k = parameters$k,
    clusters = flexclust::clusters(clust),
    parameters = parameters
  )
} # /rtemis::cluster
