# c_PAM.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Partitioning Around Medoids
#'
#' Perform
#' [PAM clustering](https://en.wikipedia.org/wiki/K-medoids#Partitioning_Around_Medoids_(PAM))
#' using `cluster::pam`
#'
#' @inheritParams c_KMeans
#' @param x Input matrix / data.frame
#' @param diss Logical: If TRUE, `x` should be a `dist` or dissimilarity matrix.
#' Otherwise, `x` should be a matrix of cases by features. Default = FALSE
#' @param metric Character: Dissimilarity metric to be used. Options: 'euclidean', 'manhattan'
#' @param do.swap Logical: If TRUE, perform the swap phase (See `cluster::pam`), as in the
#' original PAM algorithm. This is computationally intensive and can be skipped. Default = TRUE
#' @param ... Additional parameters to be passed to `cluster::pam`
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_PAM <- function(
  x,
  k = 2,
  diss = FALSE,
  metric = "euclidean",
  do.swap = TRUE,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "PAM"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("cluster")

  # Arguments ----
  if (missing(x)) {
    print(args(c_PAM))
    stop("x is missing")
  }

  # CLUST ----
  if (verbose) msg2("Partitioning Around Medoids with k = ", k, "...", sep = "")
  clust <- cluster::pam(
    x,
    k = k,
    diss = diss,
    metric = metric,
    do.swap = do.swap,
    trace.lev = ifelse(verbose, 3, 0),
    ...
  )

  # Clusters ----
  clusters.train <- clust$clustering

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = NULL,
    parameters = list(k = k, diss = diss, metric = metric),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_PAM
