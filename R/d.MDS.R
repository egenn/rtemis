# d.MDS.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

#' Multidimensional Scaling
#'
#' Calculates MDS decomposition and projections using the \code{stats:cmdscale}
#'
#' Project scaled variables to MDS components.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' fastMDS will be applied to the transpose of the n x p matrix.
#' fastMDS will fail if there are any NA values or constant features: remove them using \link{preprocess}
#'
#' @inheritParams d.SVD
#' @param x Input data
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param dist.method String: method to use to calculate distance. See \code{stats::dist("method")}
#' @param eig Logical: If TRUE, return eigenvalues. Default = FALSE
#' @param add Logical: If TRUE, an additive constant \code{c*} will be computed and added to the
#' non-diagonal dissimilarities, which makes the Euclidean. Default = FALSE
#' @param x.ret Logical: If TRUE, return the doubly centered symmetric distance matrix. Default = FALSE
#' @param scale Logicall: If TRUE, scale input data before decomposition. Default = TRUE
#' @param center Logicall: If TRUE, also center input data if \code{scale} is \code{TRUE}.
#' Default = TRUE
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @family Decomposition
#' @export

d.MDS <- function(x,
                  k = 2,
                  dist.method = c("euclidean", "maximum", "manhattan",
                                  "canberra", "binary", "minkowski"),
                  eig = FALSE,
                  add = FALSE,
                  x.ret = FALSE,
                  scale = TRUE,
                  center = TRUE,
                  verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  dist.method <- match.arg(dist.method)
  decom.name <- "MDS"

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(d.MDS))
    stop("x is missing")
  }

  # [ DATA ] ====
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0('Feature.', seq(NCOL(x)))
  xnames <- colnames(x)
  if (scale) {
    x <- scale(x, center = center)
  }

  # [ MDS ] ====
  if (verbose) msg("Running Multidimensional Scaling...")
  .dist <- dist(x, method = dist.method)
  decom <- cmdscale(.dist, k = k, eig = eig, add = add, x.ret = x.ret, list. = TRUE)

  # [ PROJECTIONS ] ====
  projections.train <- decom$points
  colnames(projections.train) <- paste0("MDS", seq(NCOL(projections.train)))

  # [ OUTRO ] ====
  extra <- list()
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = NULL,
                    extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.MDS
