# d_NMF.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Non-negative Matrix Factorization (NMF)
#'
#' Perform NMF decomposition using `NMF::nmf`
#'
#' Project scaled variables to NMF bases.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' NMF will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d_SVD
#' @param x Input data
#' @param x.test Optional test set. Will be projected on to NMF basis
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param method NMF method. Defaults to "brunet". See `NMF::nmf`
#' @param nrun Integer: Number of runs to perform
#' @param scale Logical: If TRUE, scale input data before projecting
#' @param center Logical: If TRUE, also center input data if `scale` is `TRUE`
#' @param ... Additional parameters to be passed to `NMF::nmf`
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_NMF <- function(
  x,
  x.test = NULL,
  k = 2,
  method = "brunet",
  nrun = 30,
  scale = TRUE,
  center = FALSE,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "NMF"

  # Dependencies ----
  dependency_check("NMF")

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

  # NMF ----
  if (verbose) msg2("Running Non-negative Matrix Factorization...")
  decom <- NMF::nmf(t(x), rank = k, method = method, ...)
  basis <- NMF::basis(decom)
  coef <- NMF::coef(decom)
  scoef <- NMF::scoef(decom)

  # Projections ----
  projections.test <- NULL
  if (scale) {
    projections.train <- scale(x, center = center) %*% basis
    if (!is.null(x.test))
      projections.test <- scale(x.test, center = center) %*% basis
  } else {
    projections.train <- x %*% basis
    if (!is.null(x.test)) projections.test <- x.test %*% basis
  }

  # Outro ----
  extra <- list(basis = basis, coef = coef, scoef = scoef)
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = projections.test,
    parameters = list(
      k = k,
      method = method,
      nrun = nrun,
      scale = scale,
      center = center
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_NMF
