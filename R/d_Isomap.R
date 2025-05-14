# d_ISOMAP.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Isomap
#'
#' Perform ISOMAP decomposition using `vegan::isomap`
#'
#' Project scaled variables to ISOMAP components
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' ISOMAP will be applied to the transpose of the n x p matrix.
#'
#' @param x Input data
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param dist.method Character: Distance calculation method. See `vegan::vegdist`
#' @param nsd Integer: Number of shortest dissimilarities retained
#' @param path Character: The `path` argument of `vegan::isomap`
#' @param center Logical: If TRUE, center data prior to decomposition. Default = TRUE
#' @param scale Logical: If TRUE, scale data prior to decomposition. Default = TRUE
#' @param verbose Logical: If TRUE, print messages to output
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional parameters to be passed to `vegan::isomap`
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_Isomap <- function(
  x,
  k = 2, # isomap :ndim
  dist.method = "euclidean",
  nsd = 0,
  path = c("shortest", "extended"),
  center = TRUE,
  scale = TRUE,
  verbose = TRUE,
  n.cores = rtCores,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  path <- match.arg(path)
  decom.name <- "ISOMAP"

  # Dependencies ----
  dependency_check("vegan")

  # Data ----
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg2("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg2("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  xnames <- colnames(x)
  x <- as.matrix(x)

  # scale ----
  if (scale || center) {
    x <- scale(x, scale = scale, center = center)
    .center <- attr(x, "scaled:center")
    .scale <- attr(x, "scaled:scale")
  } else {
    .center <- .scale <- NULL
  }

  # ISOMAP ----
  if (verbose) msg2("Running Isomap...")
  dist <- vegan::vegdist(x = x, method = dist.method)
  decom <- vegan::isomap(dist, ndim = k, k = nsd, path = path, ...)

  # Projections ----
  projections.train <- decom$points

  # Outro ----
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = NULL,
    parameters = list(k = k, dist.method = dist.method, nsd = nsd, path = path),
    center = .center,
    scale = .scale,
    extra = list()
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_ISOMAP
