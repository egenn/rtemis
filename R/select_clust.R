# select_clust.R
# ::rtemis::
# 2016-22 E.D. Gennatas rtemis.org

#' Select \pkg{rtemis} Clusterer
#'
#' Accepts clusterer name (supports abbreviations) and returns \pkg{rtemis}
#' function name or the function itself.
#' If run with no parameters, prints list of available algorithms.
#'
#' @param clust Character: Clustering algorithm name. Case insensitive, supports
#' partial matching. e.g. "hop" for HOPACH
#' @param fn Logical: If TRUE, return function, otherwise name of function.
#' @param desc Logical: If TRUE, return full name of algorithm `clust`
#'
#' @return Name of function (Default) or function (`fn=TRUE`) or full name
#' of algorithm (`desc=TRUE`)
#' @author E.D. Gennatas
#' @export

select_clust <- function(clust, fn = FALSE, desc = FALSE) {
  description <- list(
    "CMeans" = "Fuzzy C-means Clustering",
    "DBSCAN" = "Density-based spatial clustering of applications with noise",
    "EMC" = "Expectation Maximization Clustering",
    "HARDCL" = "Hard Competitive Learning",
    "HOPACH" = "Hierarchical Ordered Partitioning And Collapsing Hybrid",
    "H2OKMeans" = "H2O K-Means Clustering",
    "KMeans" = "K-Means Clustering",
    "MeanShift" = "Mean Shift Clustering",
    "NGAS" = "Neural Gas Clustering",
    "PAM" = "Partitioning Around Medoids",
    "PAMK" = "Partitioning Around Medoids with k estimation",
    "SPEC" = "Spectral Clustering"
  )
  description <- t(as.data.frame(description))
  description <- data.frame(
    Name = rownames(description),
    Description = description
  )

  if (missing(clust)) {
    cat(
      ".:select_clust\nrtemis supports the following clustering algorithms:\n\n"
    )
    print(description, quote = FALSE, row.names = FALSE)
    return(invisible(9))
  }

  name <- description[, 1][tolower(clust) == tolower(description[, 1])]
  if (length(name) == 0) {
    stop("Incorrect clusterer specified")
  }

  if (desc) {
    return(as.character(description[description$Name == name, 2]))
  }

  c_algname <- paste0("c_", name)
  if (fn) getFromNamespace(c_algname, "rtemis") else c_algname
} # rtemis::select_clust
