# d.NMF.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Non-negative Matrix Factorization (NMF)
#'
#' Calculate NMF decomposition and projections
#'
#' Project scaled variables to NMF bases.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' NMF will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d.SVD
#' @param x Input data
#' @param x.test Optional test set. Will be projected on to NMF basis
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param method NMF method. Defaults to "brunet". See \code{NMF::nmf}
#' @param nrun Integer: Number of runs to perform
#' @param scale Logical: If TRUE, scale input data before projecting
#' @param center Logical: If TRUE, also center input data if \code{scale} is \code{TRUE}
#' @param ... Additional parameters to be passed to \code{NMF::nmf}
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @family Decomposition
#' @export

d.NMF <- function(x,
                  x.test = NULL,
                  k = 2,
                  method = "brunet",
                  nrun = 30,
                  scale = TRUE,
                  center = FALSE,
                  verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  call <- NULL
  decom.name <- "NMF"

  # [ DEPENDENCIES ] ====
  if (!depCheck("NMF", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(d.NMF))
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
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature.", 1:NCOL(x))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames

  # [ NMF ] ====
  if (verbose) msg("Running Non-negative Matrix Factorization...")
  decom <- NMF::nmf(t(x), rank = k, method = method, ...)
  basis <- NMF::basis(decom)
  coef <- NMF::coef(decom)
  scoef <- NMF::scoef(decom)

  # [ PROJECTIONS ] ====
  projections.test <- NULL
  if (scale) {
    projections.train <- scale(x, center = center) %*% basis
    if (!is.null(x.test)) projections.test <- scale(x.test, center = center) %*% basis
  } else {
    projections.train <- x %*% basis
    if (!is.null(x.test)) projections.test <- x.test %*% basis
  }

  # [ OUTRO ] ====
  extra <- list(basis = basis,
                coef = coef,
                scoef = scoef)
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    parameters = list(k = k,
                                      method = method,
                                      nrun = nrun,
                                      scale = scale,
                                      center = center),
                    projections.train = projections.train,
                    projections.test = projections.test,
                    extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.NMF
