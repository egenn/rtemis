# S7_Clustering.R
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
#' @export
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
.DollarNames.ClusteringParameters <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Clustering ----
#' @title Clustering
#'
#' @description
#' Clustering class.
#'
#' @field algorithm Character: Algorithm name.
#' @field clust Any: Clustering object.
#' @field k Integer: Number of clusters.
#' @field xnames Character vector: feature names.
#' @field clusters List: Cluster assignment.
#' @field parameters ClusteringParameters: Algorithm-specific parameters.
#'
#' @author EDG
#' @export
Clustering <- new_class(
  name = "Clustering",
  properties = list(
    algorithm = class_character,
    clust = class_any,
    k = class_integer,
    xnames = class_character,
    clusters = class_list,
    parameters = ClusteringParameters
  )
) # /Clustering

# DecompositionParameters ----
#' @title DecompositionParameters
#' 
#' @description
#' Decomposition parameters class.
#' 
#' @field algorithm Character: Algorithm name.
#' @field parameters List: Algorithm-specific parameters.
#' 
#' @author EDG
#' @export
DecompositionParameters <- new_class(
  name = "DecompositionParameters",
  properties = list(
    algorithm = class_character,
    parameters = class_list
  )
) # /DecompositionParameters

# Make DecompositionParameters@parameters `$`-accessible
method(`$`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# Make DecompositionParameters@parameters `[[`-accessible
method(`[[`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete DecompositionParameters@parameters ----
.DollarNames.DecompositionParameters <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Decomposition ----
#' @title Decomposition
#'
#' @description
#' Decomposition class.
#'
#' @field algorithm Character: Algorithm name.
#' @field decom Any: Decomposition object.
#' @field xnames Character vector: feature names.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
#' @export
Decomposition <- new_class(
  name = "Decomposition",
  properties = list(
    algorithm = class_character,
    decom = class_any,
    xnames = class_character,
    parameters = DecompositionParameters
  )
) # /Decomposition
