# S7_ClusteringParameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# ClusteringParameters ----
#' @title ClusteringParameters
#'
#' @description
#' Clustering parameters class.
#'
#' @field algorithm Character: Algorithm name.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
#' @noRd
ClusteringParameters <- new_class(
  name = "ClusteringParameters",
  properties = list(
    algorithm = class_character,
    parameters = class_list
  )
) # /ClusteringParameters

# Make ClusteringParameters@parameters `$`-accessible
method(`$`, ClusteringParameters) <- function(x, name) {
  x@parameters[[name]]
}

# Make ClusteringParameters@parameters `[[`-accessible
method(`[[`, ClusteringParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete ClusteringParameters@parameters ----
method(`.DollarNames`, ClusteringParameters) <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Print ClusteringParameters ----
#' Print Method for ClusteringParameters
#'
#' @param x ClusteringParameters object.
#' @param pad Integer: Left side padding.
#' 
#' @returns ClusteringParameters object, invisibly.
#'
#' @author EDG
#' @noRd
method(print, ClusteringParameters) <- function(x, pad = 0L) {
  objcat(paste(x@algorithm, "ClusteringParameters"), pad = pad)
  printls(props(x)$parameters, pad = pad)
  invisible(x)
}

# KMeansParameters ----
#' @title KMeansParameters
#'
#' @description
#' ClusteringParameters subclass for K-means Clustering.
#'
#' @author EDG
#' @noRd
KMeansParameters <- new_class(
  name = "KMeansParameters",
  parent = ClusteringParameters,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringParameters(
        algorithm = "KMeans",
        parameters = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /KMeansParameters

#' Setup KMmeansParameters
#'
#' @param k Number of clusters.
#'
#' @return KMeansParameters object.
#'
#' @author EDG
#' @export
setup_KMeans <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  KMeansParameters(k, dist)
} # /rtemis::setup_KMeans

# HardCLParameters ----
#' @title HardCLParameters
#'
#' @description
#' ClusteringParameters subclass for HardCL Clustering.
#'
#' @author EDG
#' @noRd
HardCLParameters <- new_class(
  name = "HardCLParameters",
  parent = ClusteringParameters,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringParameters(
        algorithm = "HardCL",
        parameters = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /HardCLParameters

#' Setup HardCLParameters
#'
#' @param k Number of clusters.
#'
#' @return HardCLParameters object.
#'
#' @author EDG
#' @export
setup_HardCL <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  HardCLParameters(k, dist)
} # /rtemis::setup_HardCL

# NeuralGasParameters ----
#' @title NeuralGasParameters
#'
#' @description
#' ClusteringParameters subclass for Neural Gas Clustering.
#'
#' @author EDG
#' @noRd
NeuralGasParameters <- new_class(
  name = "NeuralGasParameters",
  parent = ClusteringParameters,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringParameters(
        algorithm = "NeuralGas",
        parameters = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /NeuralGasParameters

#' Setup NeuralGasParameters
#'
#' @param k Number of clusters.
#' 
#' @return NeuralGasParameters object.
#' 
#' @author EDG
#' @export
setup_NeuralGas <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  NeuralGasParameters(k, dist)
} # /rtemis::setup_NeuralGas
