# d.SPCA.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

#' Sparse Principal Component Analysis
#'
#' Perform sparse and/or non-negative PCA or cumulative PCA decomposition
#'   using \code{nsprcomp::nsprcomp} or \code{nsprcomp::nscumcomp} respectively
#'
#' Project scaled variables to sparse and/or non-negative PCA components.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' SPCA will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d.SVD
#' @param x Input matrix
#' @param x.test Optional test set. Will be projected on to SPCA basis
#' @param k Integer vector of length 1 or greater. N of components to return
#'   If set to 0, \code{th} determines eigenvalue below which PCs are ignored
#' @param nz Integer: Upper bound on non-zero loadings. See \code{nsprcomp::nscumcomp("k")}
#' @param nneg Logical: If TRUE, calculate non-negative loadings only. Default = TRUE
#' @param method Character: "cumulative" or "vanilla" sparse PCA. Default = "cumulative"
#' @param scale LogSPCAl: If TRUE, scale input data before projecting. Default = TRUE
#' @param center LogSPCAl: If TRUE, also center input data if \code{scale} is \code{TRUE}. Default = FALSE
#' @param ... Additional parameters to be passed to \code{fastSPCA::fastSPCA}
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @family Decomposition
#' @export

d.SPCA <- function(x,
                   x.test = NULL,
                   k = 1,
                   nz = .5 * NCOL(x),
                   nneg = TRUE,
                   method = c("cumulative", "vanilla"),
                   scale = TRUE,
                   center = FALSE,
                   verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  decom.name <- "SPCA"

  # [ DEPENDENCIES ] ====
  if (!depCheck("nsprcomp", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(d.SPCA))
    stop("x is missing")
  }
  method <- match.arg(method)

  # [ DATA ] ====
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames

  # [ SPCA ] ====
  if (verbose) msg("Performing Sparse Principal Components Analysis...")
  if (method == "cumulative") {
    decom <- nsprcomp::nscumcomp(x, ncomp = k, k = nz, nneg = nneg, ...)
  } else {
    decom <- nsprcomp::nsprcomp(x, ncomp = k, k = nz, nneg = nneg, ...)
  }

  vectors <- decom$rotation

  # [ PROJECTIONS ] ====
  projections.test <- NULL
  if (scale) {
    projections.train <- scale(x, center = center) %*% vectors
    if (!is.null(x.test)) projections.test <- scale(x.test, center = center) %*% vectors
  } else {
    projections.train <- x %*% vectors
    if (!is.null(x.test)) projections.test <- x.test %*% vectors
  }

  # [ OUTRO ] ====
  extra <- list(vectors = vectors)
  rt <- rtDecom$new(decom.name = decom.name,
                     decom = decom,
                     xnames = xnames,
                     projections.train = projections.train,
                     projections.test = projections.test,
                    parameters = list(k = k,
                                      nz = nz,
                                      nneg = nneg,
                                      method = method,
                                      scale = scale,
                                      center = center),
                     extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.SPCA
