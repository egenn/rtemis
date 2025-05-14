# d_UMAP.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Uniform Manifold Approximation and Projection (UMAP)
#'
#' Perform UMAP decomposition using `uwot::umap`
#'
#' Updated 2023-12-09: See [GitHub issue](https://github.com/jlmelville/uwot/issues/115)
#' and [related comment](https://github.com/bwlewis/irlba/issues/70#issuecomment-1826900769)
#'
#' @param x Input matrix
#' @param x.test Optional test set matrix. Will be projected on to UMAP bases
#' @param k Integer: Number of projections
#' @param n.neighbors Integer: Number of keighbors
#' @param init Character: Initialization type. See `uwot::umap "init"`
#' @param metric Character: Distance metric to use: "euclidean", "cosine",
#' "manhattan", "hamming", "categorical". Default = "euclidean"
#' @param epochs Integer: Number of epochs
#' @param learning.rate Float: Learning rate. Default = 1
#' @param scale Logical: If TRUE, scale input data before doing UMAP.
#' Default = TRUE
#' @param verbose Logical: If TRUE, print messages to screen. Default = TRUE
#' @param ... Additional parameters to be passed to `uwot::umap`
#'
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_UMAP <- function(
  x,
  x.test = NULL,
  k = 2,
  n.neighbors = 15,
  init = "spectral",
  metric = c("euclidean", "cosine", "manhattan", "hamming", "categorical"),
  epochs = NULL,
  learning.rate = 1,
  scale = TRUE,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "UMAP"

  # Dependencies ----
  dependency_check("uwot")

  # Arguments ----
  init <- match.arg(init)
  metric <- match.arg(metric)

  # Data ----
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg2("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg2("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames

  # UMAP ----
  if (verbose) msg2("Performing UMAP Decomposition...")
  decom <- uwot::umap(
    x,
    n_components = k,
    n_neighbors = n.neighbors,
    init = init,
    n_epochs = epochs,
    learning_rate = learning.rate,
    metric = metric,
    scale = scale,
    verbose = verbose,
    ret_model = TRUE,
    ...
  )

  # Projections ----
  projections.train <- uwot::umap_transform(x, decom)
  if (!is.null(x.test)) {
    projections.test <- uwot::umap_transform(x.test, decom)
  } else {
    projections.test <- NULL
  }

  # Outro ----
  extra <- list()
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = projections.test,
    parameters = list(
      k = k,
      n.neighbors = n.neighbors,
      init = init,
      epochs = epochs,
      learning.rate = learning.rate,
      metric = metric,
      scale = scale
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_UMAP
