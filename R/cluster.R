# cluster.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Perform Clustering
#'
#' Perform clustering on the rows (usually cases) of a dataset.
#'
#' @param x Matrix or data.frame: Data to cluster. Rows are cases to be clustered.
#' @param algorithm Character: Clustering algorithm.
#' @param parameters List: Algorithm-specific parameters.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Clustering object.
#'
#' @author EDG
#' @export
cluster <- function(
  x,
  algorithm = "KMeans",
  parameters = NULL,
  verbosity = 1L
) {
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
  if (verbosity > 0L) {
    msg20("Clustering with ", algorithm, "...")
  }
  clust <- do_call(
    fn = get_clust_fn(algorithm),
    args = list(x = x, parameters = parameters, verbosity = verbosity)
  )

  # Clusters ----
  clusters <- do_call(
    fn = get_clustpredict_fn(algorithm),
    args = list(clust = clust)
  )

  # Outro ----
  outro(start_time, verbosity = verbosity)
  Clustering(
    algorithm = algorithm,
    clust = clust,
    k = parameters[["k"]],
    clusters = clusters,
    parameters = parameters
  )
} # /rtemis::cluster
