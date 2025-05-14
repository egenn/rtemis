# d_MDS.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Multidimensional Scaling
#'
#' Perform MDS decomposition using `stats:cmdscale`
#'
#' Project scaled variables to MDS components.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' fastMDS will be applied to the transpose of the n x p matrix.
#' fastMDS will fail if there are any NA values or constant features: remove them using [preprocess]
#'
#' @inheritParams d_SVD
#' @param x Input data
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param dist.method Character: method to use to calculate distance. See `stats::dist("method")`
#' @param eig Logical: If TRUE, return eigenvalues. Default = FALSE
#' @param add Logical: If TRUE, an additive constant `c*` will be computed and added to the
#' non-diagonal dissimilarities, which makes the Euclidean. Default = FALSE
#' @param x.ret Logical: If TRUE, return the doubly centered symmetric distance matrix. Default = FALSE
#' @param scale Logical: If TRUE, scale input data before decomposition. Default = TRUE
#' @param center Logical: If TRUE, also center input data if `scale` is `TRUE`.
#' Default = TRUE
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_MDS <- function(
  x,
  k = 2,
  dist.method = c(
    "euclidean",
    "maximum",
    "manhattan",
    "canberra",
    "binary",
    "minkowski"
  ),
  eig = FALSE,
  add = FALSE,
  x.ret = FALSE,
  scale = TRUE,
  center = TRUE,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  dist.method <- match.arg(dist.method)
  decom.name <- "MDS"

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
  if (scale) {
    x <- scale(x, center = center)
  }

  # MDS ----
  if (verbose) msg2("Running Multidimensional Scaling...")
  .dist <- dist(x, method = dist.method)
  decom <- cmdscale(
    .dist,
    k = k,
    eig = eig,
    add = add,
    x.ret = x.ret,
    list. = TRUE
  )

  # Projections ----
  projections.train <- decom$points
  colnames(projections.train) <- paste0("MDS", seq_len(NCOL(projections.train)))

  # Outro ----
  extra <- list()
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = NULL,
    parameters = list(
      k = k,
      dist.method = dist.method,
      eig = eig,
      add = add,
      x.ret = x.ret,
      scale = scale,
      center = center
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_MDS
