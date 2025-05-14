# d_TSNE.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' t-distributed Stochastic Neighbor Embedding
#'
#' Perform t-SNE decomposition using `Rtsne::Rtsne`
#'
#' @param x Input matrix
#' @param k Integer. Number of t-SNE components required
#' @param initial.dims Integer: Number of dimensions to retain in initial PCA. Default = 50
#' @param perplexity Numeric: Perplexity parameter
#' @param theta Float: 0.0: exact TSNE. Increase for higher speed, lower accuracy. Default = 0
#' @param check.duplicates Logical: If TRUE, Checks whether duplicates are present. Best to set test manually
#' @param pca Logical: If TRUE, perform initial PCA step. Default = TRUE
#' @param max.iter Integer: Maximum number of iterations. Default = 1000
#' @param scale Logical: If TRUE, scale before running t-SNE using `base::scale`. Default = FALSE
#' @param center Logical: If TRUE, and `scale = TRUE`, also center. Default = FALSE
#' @param is.distance Logical: If TRUE, `x` should be a distance matrix. Default = FALSE
#' @param verbose Logical: If TRUE, print messages to output
#' @param ... Options for `Rtsne::Rtsne`
#' @param outdir Path to output directory
#'
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_TSNE <- function(
  x,
  k = 3,
  initial.dims = 50,
  perplexity = 15,
  theta = 0,
  check.duplicates = TRUE,
  pca = TRUE,
  max.iter = 1000,
  scale = FALSE,
  center = FALSE,
  is.distance = FALSE,
  verbose = TRUE,
  outdir = "./",
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  if (verbose) msg2("Running t-distributed Stochastic Neighbot Embedding")
  decom.name <- "TSNE"

  # Dependencies ----
  dependency_check("Rtsne")

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
  # if (!is.null(x.test)) colnames(x.test) <- xnames
  x <- scale(x, scale = scale, center = center)
  # if (!is.null(x.test)) x.test <- scale(x.test, scale = scale, center = center)

  # t-SNE ----
  if (verbose) msg2("Running t-SNE...")
  decom <- Rtsne::Rtsne(
    X = x,
    dims = k,
    initial_dims = initial.dims,
    perplexity = perplexity,
    theta = theta,
    check_duplicates = check.duplicates,
    pca = pca,
    max_iter = max.iter,
    verbose = verbose,
    is_distance = is.distance,
    ...
  )

  # Output ----
  extra <- list()
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = decom$Y,
    projections.test = NULL,
    parameters = list(
      k = k,
      initial.dims = initial.dims,
      perplexity = perplexity,
      theta = theta,
      check.duplicates = check.duplicates,
      pca = pca,
      max.iter = max.iter,
      scale = scale,
      center = center
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_TSNE
