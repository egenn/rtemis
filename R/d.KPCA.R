# d.KPCA.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Kernel Principal Component Analysis
#'
#' Perform kernel PCA decomposition using \code{kernlab::kpca}
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
#' @param kernel Character: Type of kernel to use. See \code{kernlab::kpca}
#' @param kpar List of hyperparameters: See \code{kernlab::kpca("kpar")}
#' @param center Logical: If TRUE, center data prior to decomposition. Default = TRUE
#' @param scale Logical: If TRUE, scale data prior to decomposition. Default = TRUE
#' @param ... Additional parameters to be passed to \code{fastKPCA::fastKPCA}
#' @return \link{rtDecom} object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d.KPCA <- function(x,
                   x.test = NULL,
                   k = 2,
                   th = 0.0001,
                   kernel = "rbfdot",
                   kpar = NULL,
                   center = TRUE,
                   scale = TRUE,
                   verbose = TRUE, ...) {

  # [ Intro ] ====
  start.time <- intro(verbose = verbose)
  decom.name <- "KPCA"

  # [ Dependencies ] ====
  if (!depCheck("kernlab", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
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

  # [ Data ] ====
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0('Feature_', seq(NCOL(x)))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames
  x <- as.matrix(x)

  # [ scale ] ====
  if (scale | center) {
    x <- scale(x, scale = scale, center = center)
    .center <- attr(x, "scaled:center")
    .scale <- attr(x, "scaled:scale")
  } else {
    .center <- .scale <- FALSE
  }

  # [ KPCA ] ====
  if (verbose) msg("Running Kernel Principal Components Analysis...")
  decom <- kernlab::kpca(x, features = k, th = th,
                         kernel = kernel, kpar = kpar, ...)
  vectors <- decom@pcv

  # [ Projections ] ====
  projections.test <- NULL
  projections.train <- kernlab::predict(decom, x)
  if (!is.null(x.test)) {
    if (scale | center) {
      # x.test <- (x.test + .center) %*% diag(.scale) # faster for small matrices only
      x.test <- t(t(x.test + .center) * .scale)
    }
    projections.test <- kernlab::predict(decom, x.test)
  }

  # [ Outro ] ====
  extra <- list(vectors = vectors)
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = projections.test,
                    parameters = list(k = k,
                                      th = th,
                                      kernel = kernel,
                                      kpar = kpar,
                                      scale = scale,
                                      center = center),
                    center = .center,
                    scale = .scale,
                    extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.KPCA
