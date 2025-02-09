# cluster_KMeans.R
# ::rtemis::
# 2025 EDG rtemis.org

#' K-means Clustering
#'
#' @keywords internal
#' @noRd
cluster_KMeans <- function(x, parameters) {
  # Checks ----
  check_is_S7(parameters, KMeansParameters)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Cluster ----
  clust <- flexclust::cclust(
    x = x,
    k = parameters$k,
    dist = parameters$dist,
    method = "kmeans"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_KMeans


#' Hard Competitive Learning Clustering
#' 
#' @keywords internal
#' @noRd
cluster_HardCL <- function(x, parameters) {
  # Checks ----
  check_is_S7(parameters, HardCLParameters)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Cluster ----
  clust <- flexclust::cclust(
    x = x,
    k = parameters$k,
    dist = parameters$dist,
    method = "hardcl"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_HardCL


#' Neural Gas Clustering
#' 
#' @keywords internal
#' @noRd
cluster_NeuralGas <- function(x, parameters) {
  # Checks ----
  check_is_S7(parameters, NeuralGasParameters)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Cluster ----
  clust <- flexclust::cclust(
    x = x,
    k = parameters$k,
    dist = parameters$dist,
    method = "neuralgas"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_NeuralGas
