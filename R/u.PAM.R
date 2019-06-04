# u.PAM.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Partitioning Around Medoids
#'
#' Perform PAM clustering using \code{cluster::pam}
#'
#' @inheritParams u.KMEANS
#' @param x Input matrix / data.frame
#' @param diss Logical: If TRUE, \code{x} should be a \code{dist} or dissimilarity matrix.
#' Otherwise, \code{x} should be a matrix of cases by features. Default = FALSE
#' @param metric String: Dissimilarity metric to be used. Options: 'euclidean', 'manhattan'
#' @param do.swap Logical: If TRUE, perform the swap phase (See \code{cluster::pam}), as in the
#' original PAM algorithm. This is computationally intensive and can be skipped. Default = TRUE
#' @param ... Additional parameters to be passed to \code{cluster::pam}
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

u.PAM <- function(x,
                  k = 2,
                  diss = FALSE,
                  metric = 'euclidean',
                  do.swap = TRUE,
                  verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "PAM"

  # [ DATA ] ====
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # [ DEPENDENCIES ] ====
  if (!depCheck("cluster", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(u.PAM))
    stop("x is missing")
  }

  # [ CLUST ] ====
  if (verbose) msg("Partitioning Around Medoids with k = ", k, "...", sep = "")
  clust <- cluster::pam(x,
                        k = k,
                        diss = diss,
                        metric = metric,
                        do.swap = do.swap,
                        trace.lev = ifelse(verbose, 3, 0), ...)

  # [ CLUSTERS ] ====
  clusters.train <- clust$clustering

  # [ OUTRO ] ====
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = NULL,
                    parameters = list(k = k, diss = diss, metric = metric),
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.PAM
