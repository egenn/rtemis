# S7_Clustering.R
# ::rtemis::
# 2025 EDG rtemis.org

# Clustering ----
#' @title Clustering
#'
#' @description
#' Clustering class.
#'
#' @field algorithm Character: Algorithm name.
#' @field clust Any: Clustering object.
#' @field k Integer: Number of clusters.
#' @field clusters List: Cluster assignment.
#' @field parameters ClusteringParameters: Algorithm-specific parameters.
#'
#' @author EDG
#' @noRd
Clustering <- new_class(
  name = "Clustering",
  properties = list(
    algorithm = class_character,
    clust = class_any,
    k = class_integer,
    clusters = class_integer | class_list,
    parameters = ClusteringParameters
  )
) # /Clustering

# Make Clustering props `$`-accessible
method(`$`, Clustering) <- function(x, name) {
  prop_names <- names(props(x))
  if (name %in% prop_names) {
    prop(x, name)
  } else {
    stop(paste("No property named", name, "in Clustering object"))
  }
}

# `$`-autocomplete Clustering props
method(`.DollarNames`, Clustering) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}

# Make Clustering@clusters `[[`-accessible
method(`[[`, Clustering) <- function(x, index) {
  props(x, "clusters")[[index]]
}

# Print Clustering ----
method(print, Clustering) <- function(x) {
  cat(gray(".:"))
  objcat(paste(x$algorithm, "Clustering"))
  cat("\n")
  printls(props(x)[-1])
  invisible(x)
} 
