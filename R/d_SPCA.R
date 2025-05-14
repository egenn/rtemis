# d_SPCA.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Sparse Principal Component Analysis
#'
#' Perform sparse and/or non-negative PCA or cumulative PCA decomposition
#'   using `nsprcomp::nsprcomp` or `nsprcomp::nscumcomp` respectively
#'
#' Project scaled variables to sparse and/or non-negative PCA components.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' SPCA will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d_SVD
#' @param x Input matrix
#' @param x.test Optional test set. Will be projected on to SPCA basis
#' @param k Integer vector of length 1 or greater. N of components to return
#'   If set to 0, `th` determines eigenvalue below which PCs are ignored
#' @param nz Integer: Upper bound on non-zero loadings. See `nsprcomp::nscumcomp("k")`
#' @param nneg Logical: If TRUE, calculate non-negative loadings only. Default = FALSE
#' @param gamma Float (>0): Penalty on the divergence from otrhonormality of the pseudo-rotation
#' matrix. Default = 0, i.e. no penalty. May need to increase with collinear features.
#' @param method Character: "cumulative" or "vanilla" sparse PCA. Default = "cumulative"
#' @param scale Logical: If TRUE, scale input data before projecting. Default = TRUE
#' @param center Logical: If TRUE, also center input data if `scale` is `TRUE`. Default = FALSE
#' @param ... Additional parameters to be passed to `fastSPCA::fastSPCA`
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_SPCA <- function(
  x,
  x.test = NULL,
  k = 1,
  nz = floor(.5 * NCOL(x)),
  nneg = FALSE,
  gamma = 0,
  method = c("cumulative", "vanilla"),
  scale = TRUE,
  center = TRUE,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "SPCA"

  # Dependencies ----
  dependency_check("nsprcomp")

  # Arguments ----
  method <- match.arg(method)

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

  # scale ----
  if (scale || center) {
    x <- scale(x, scale = scale, center = center)
    .center <- attr(x, "scaled:center")
    .scale <- attr(x, "scaled:scale")
  } else {
    .center <- .scale <- FALSE
  }

  # sPCA ----
  if (verbose) msg2("Performing Sparse Principal Components Analysis...")
  if (method == "cumulative") {
    decom <- nsprcomp::nscumcomp(
      x,
      ncomp = k,
      k = nz,
      nneg = nneg,
      gamma = gamma,
      scale. = FALSE,
      ...
    )
  } else {
    decom <- nsprcomp::nsprcomp(
      x,
      ncomp = k,
      k = nz,
      nneg = nneg,
      scale. = FALSE,
      ...
    )
  }

  vectors <- decom$rotation

  # Projections ----
  projections.train <- x %*% vectors
  projections.test <- NULL
  if (!is.null(x.test)) {
    if (scale || center) {
      x.test <- t(t(x.test + .center) * .scale)
    }
    projections.test <- x.test %*% vectors
  }

  # Outro ----
  extra <- list(vectors = vectors)
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = projections.test,
    parameters = list(
      k = k,
      nz = nz,
      nneg = nneg,
      method = method,
      scale = scale,
      center = center
    ),
    center = .center,
    scale = .scale,
    extra = extra
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_SPCA
