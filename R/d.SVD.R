# d.SVD.R
# ::rtemis::
# 2015 E.D. Gennatas lambdamd.org

#' Singular Value Decomposition
#'
#' Perform SVD decomposition using \code{base::svd}
#'
#' Same solution as \link{d.PCA}
#'
#' @param x Input matrix
#' @param x.test Optional test set matrix. Will be projected on to SVD bases
#' @param k Integer: Number of right singular vectors to compute (\code{svd}'s \code{nv})
#' @param nu Integer: Number of left singular vectors to compute
#' @param scale Logical: If TRUE, scale input data before doing SVD. Default = TRUE
#' @param center Logical: If TRUE, also center input data if \code{scale} is \code{TRUE}. Default = TRUE
#' @param verbose Logical: If TRUE, print messages to screen. Default = TRUE
#' @param ... Additional parameters to be passed to \code{svd}
#' @return \link{rtDecom} object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d.SVD <- function(x,
                  x.test = NULL,
                  k = 2, # nv
                  nu = 0,
                  scale = TRUE,
                  center = TRUE,
                  verbose = TRUE, ...) {

  # [ Intro ] ====
  start.time <- intro(verbose = verbose)
  decom.name <- "SVD"

  # [ Arguments ] ====
  if (missing(x)) {
    print(args(d.SVD))
    stop("x is missing")
  }

  # [ Data ] ====
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
  if (scale) {
    x <- scale(x, center = center)
    if (!is.null(x.test)) x.test <- scale(x.test, center = center)
  }

  # [ SVD ] ====
  if (verbose) msg("Performing Singular Value Decomposition...")
  decom <- svd(x, nu = nu, nv = k, ...)
  rotation <- decom$v # same as prcomp's rotation output if nu = k = NCOL(x)
  row.names(rotation) <- xnames
  colnames(rotation) <- paste0("PC.", 1:k)

  # [ Projections ] ====
  projections.train <- data.matrix(x) %*% rotation
  projections.test <- NULL
  if (!is.null(x.test)) projections.test <- data.matrix(x.test) %*% rotation

  # [ Outro ] ====
  extra <- list(rotation = rotation)
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = projections.test,
                    parameters = list(k = k,
                                      nu = nu,
                                      scale = scale,
                                      center = center),
                    extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.SVD
