# d.KPCA.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Kernel Principal Component Analysis
#'
#' Calculates kernel PCA decomposition and projections using \code{kernlab::kpca}
#'
#' Project scaled variables to KPCA components.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' KPCA will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d.SVD
#' @param x Input data
#' @param x.test Optional test set. Will be projected on to KPCA basis
#' @param k Integer vector of length 1 or greater. N of components to return
#'   If set to 0, \code{th} determines eigenvalue below which PCs are ignored
#' @param th Threshold for eigenvalue below which PCs are ignored if \code{k} is set to 0
#' @param kernel String: Type of kernel to use. See \code{kernlab::kpca}
#' @param kpar List of hyperparameters: See \code{kernlab::kpca("kpar")}
#' @param scale Logical: If TRUE, scale input data before projecting
#' @param center Logical: If TRUE, also center input data if \code{scale} is \code{TRUE}
#' @param ... Additional parameters to be passed to \code{fastKPCA::fastKPCA}
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @family Decomposition
#' @export

d.KPCA <- function(x,
                  x.test = NULL,
                  k = 2,
                  th = 0.0001,
                  kernel = "rbfdot",
                  kpar = NULL,
                  scale = TRUE,
                  center = FALSE,
                  verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  call <- NULL
  decom.name <- "KPCA"

  # [ DEPENDENCIES ] ====
  if (!depCheck("kernlab", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(d.KPCA))
    stop("x is missing")
  }

  ### Kernel parameters
  if (is.null(kpar)) {
    if (kernel == "rbfdot") {
      kpar <- list(sigma = 0.1)
    }
  }

  # [ DATA ] ====
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg("    interpreted as", n, "cases with", p, "features.")
  }
  # cat("    (If this is not what you intended, this would be the time to interrupt the run)\n")
  if (is.null(colnames(x))) colnames(x) <- paste0('Feature.', 1:NCOL(x))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames

  # [ KPCA ] ====
  if (verbose) msg("Running Kernel Principal Components Analysis...")
  decom <- kernlab::kpca(as.matrix(x), features = k, th = th,
                          kernel = kernel, kpar = kpar, ...)
  vectors <- decom@pcv

  # [ PROJECTIONS ] ====
  projections.test <- NULL
  if (scale) {
    projections.train <- scale(kernlab::predict(decom, x), center = center)
    if (!is.null(x.test)) projections.test <- scale(kernlab::predict(decom, x.test), center = center)
  } else {
    projections.train <- kernlab::predict(decom, x)
    if (!is.null(x.test)) projections.test <- kernlab::predict(decom, x.test)
  }

  # [ OUTRO ] ====
  extra <- list(vectors = vectors)
  rt <- rtDecom$new(decom.name = decom.name,
                     decom = decom,
                     xnames = xnames,
                     projections.train = projections.train,
                     projections.test = projections.test,
                     extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.KPCA
