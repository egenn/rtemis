# cluster_CMeans.R
# ::rtemis::
# 2025 EDG rtemis.org

#' C-means Clustering
#'
#' @keywords internal
#' @noRd
cluster_CMeans <- function(x, parameters) {
  # Checks ----
  check_is_S7(parameters, CMeansParameters)

  # Dependencies ----
  check_dependencies("e1071")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Cluster ----
  clust <- e1071::cmeans(
    x = x,
    centers = parameters$k,
    iter.max = parameters$max_iter,
    verbose = verbosity > 0L,
    dist = parameters$dist,
    method = parameters$method,
    m = parameters$m,
    rate.par = parameters$rate_par,
    weights = parameters$weights,
    control = parameters$control
  )
  check_inherits(clust, "fclust")
  clust
} # /rtemis::cluster_CMeans

clustpredict_CMeans <- function(clust) {
  check_inherits(clust, "fclust")
  clust$cluster
} # /rtemis::clustpredict_CMeans
