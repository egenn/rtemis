# c_PAMK.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Partitioning Around Medoids with k Estimation
#'
#' Estimate
#' [PAM clustering](https://en.wikipedia.org/wiki/K-medoids#Partitioning_Around_Medoids_(PAM))
#' solution and optimal k using `fpc::pamk`
#'
#' @param x Input matrix / data.frame
#' @param krange Integer vector: Range of k values to try
#' @param criterion Character: Criterion to use for selecting k: "asw",
#' "multiasw" or "ch". See `fpc::pamk`
#' @param usepam Logical: If TRUE, use `cluster::pam`, otherwise use
#' `cluster::clara`.
#' @param scaling Logical or Numeric vector: If TRUE, scale input. If numeric
#' vector of length equal to number of features, the features are divided by
#' the corresponding value.
#' @param diss Logical: If TRUE, treat `x` as a dissimilarity matrix,
#' otherwise as a matrix of
#' cases by features. Default = TRUE, if x inherits from class `dist`,
#' FALSE otherwise.
#' @param metric Character: Dissimilarity metric to be used. Options:
#' 'euclidean', 'manhattan'
#' @param do.swap Logical: If TRUE, perform the swap phase. See `fpc::pam`
#' for more info
#' @param trace Integer \[0, 3\]: Trace level for `fpc::pamk`
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional parameters to be passed to `fpc::pamk` and/or
#' `cluster::pam`
#'
#' @author E.D. Gennatas
#' @return `rtClust` object
#' @family Clustering
#' @export

c_PAMK <- function(
  x,
  krange = 2:10,
  criterion = "asw",
  usepam = ifelse(nrow(x) < 2000, TRUE, FALSE),
  scaling = TRUE,
  diss = inherits(data, "dist"),
  metric = "euclidean",
  do.swap = TRUE,
  trace = 0,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "PAMK"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("fpc")

  # Arguments ----
  if (missing(x)) {
    print(args(c_PAMK))
    stop("x is missing")
  }

  # PAMK ----
  if (verbose) msg2("Partitioning Around Medoids...")
  clust <- fpc::pamk(
    x,
    krange = krange,
    criterion = criterion,
    usepam = usepam,
    scaling = scaling,
    diss = diss,
    metric = metric,
    do.swap = do.swap,
    trace.lev = trace,
    ...
  )
  if (verbose) msg2("Estimated optimal number of clusters:", clust$nc)

  # Clusters ----
  clusters.train <- clust$pamobject$clustering

  # Outro ----
  extra <- list(bestk = clust$nc)
  cl <- rtClust$new(
    clust.name = clust.name,
    k = length(unique(clusters.train)),
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = NULL,
    parameters = list(
      krange = krange,
      criterion = criterion,
      usepam = usepam,
      scaling = scaling,
      diss = diss,
      metric = metric,
      do.swap = do.swap
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_PAMK
