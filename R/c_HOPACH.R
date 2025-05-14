# c_HOPACH.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Hierarchical Ordered Partitioning and Collapsing Hybrid
#'
#' Perform
#' [HOPACH clustering](https://www.sciencedirect.com/science/article/abs/pii/S0378375802003889)
#' using `hopach::hopach`
#'
#' @param x Input matrix / data.frame
#' @param dmat Matrix (numeric, no missing values) or `hdist` object of pairwise distances.
#' If NULL, it is computed based on `metric`
#' @param metric Character: Dissimilarity metric to be used. Options: "cosangle", "abscosangle", "euclid",
#' "abseuclid", "cor", "abscor"
#' @param k Integer, (0:15]: Maximum number of levels
#' @param kmax Integer, \[1:9\]: Maximum number of children at each node in the tree
#' @param khigh Integer, \[1:9\]: Maximum number of children at each nod ein the tree when computing the
#' the Mean/Median Split Silhouette. Usually same as `kmax`
#' @param trace Integer: If trace > 0, print messages during HOPACH run. Default = 0
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional parameters to pass to `cluster::hopach`
#'
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_HOPACH <- function(
  x,
  dmat = NULL,
  metric = c(
    "cosangle",
    "abscosangle",
    "euclid",
    "abseuclid",
    "cor",
    "abscor"
  ),
  k = 15,
  kmax = 9,
  khigh = 9,
  trace = 0,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = FALSE)
  clust.name <- "HOPACH"

  # Arguments ----
  metric <- match.arg(metric)

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("hopach")

  # HOPACH ----
  if (verbose) msg2("Running HOPACH clustering...")
  clust <- hopach::hopach(
    x,
    dmat = dmat,
    d = metric,
    K = k,
    kmax = kmax,
    khigh = khigh,
    verbose = trace > 0,
    ...
  )
  if (verbose) {
    msg2(
      "HOPACH identified ",
      clust$clustering$k,
      " clusters (sizes: ",
      paste(clust$clustering$sizes, collapse = ", "),
      ")",
      sep = ""
    )
  }

  # Clusters ----
  clusters.train <- clust$clustering$labels

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = length(unique(clusters.train)),
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = NULL,
    parameters = list(
      k = k,
      kmax = kmax,
      khigh = khigh
    ),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_HOPACH
