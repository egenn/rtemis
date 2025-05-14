# clust.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Clustering with \pkg{rtemis}
#'
#' Convenience function to perform any \pkg{rtemis} clustering
#'
#' @param x Numeric matrix / data frame: Input data
#' @param clust Character: Decomposition algorithm name, e.g. "nmf"
#' (case-insensitive)
#' @param x.test Numeric matrix / Data frame: Testing set data if supported by
#' `clust`
#' @param verbose Logical: if TRUE, print messages to screen
#' @param ... Additional arguments to be passed to clusterer `clust`
#' @return `rtClust` object
#' @author E.D. Gennatas
#' @export

clust <- function(x, clust = "kmeans", x.test = NULL, verbose = TRUE, ...) {
  if (missing(x)) {
    cat('Usage:\n  clust(x, "nmf", ...)\n\n')
    return(select_clust())
  }

  # Intro ----
  clusterer <- select_clust(clust, fn = FALSE)

  # DECOMPOSER ----
  clust <- R.utils::doCall(
    clusterer,
    x = x,
    x.test = x.test,
    verbose = verbose,
    ...
  )

  # Outro ----
  clust
} # rtemis::clust
