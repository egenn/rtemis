# cluster_DBSCAN.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Density-based spatial clustering of applications with noise (DBSCAN)
#'
#' @keywords internal
#' @noRd
cluster_DBSCAN <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, DBSCANParameters)

  # Dependencies ----
  check_dependencies("dbscan")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  clust <- dbscan::dbscan(
    x = x,
    eps = parameters[["eps"]],
    minPts = parameters[["min_points"]],
    weights = parameters[["weights"]],
    borderPoints = parameters[["border_points"]],
    search = parameters[["search"]],
    bucketSize = parameters[["bucket_size"]],
    splitRule = parameters[["split_rule"]],
    approx = parameters[["approx"]]
  )
  check_inherits(clust, "dbscan")
  clust
} # /rtemis::cluster_DBSCAN


clustpredict_DBSCAN <- function(clust, dat_train = NULL, newdata = NULL) {
  check_inherits(clust, "dbscan")
  if (is.null(newdata)) {
    return(clust[["cluster"]])
  } else {
    predict(clust, newdata = newdata, data = dat_train)
  }
} # /rtemis::clustpredict_DBSCAN
