# d_PCA.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Principal Component Analysis
#'
#' Perform PCA decomposition using \code{stats::prcomp}
#'
#' Same solution as \link{d_SVD}. d_PCA runs \code{prcomp}, which has useful
#' \code{summary} output
#'
#' @inheritParams d_SVD
#' @param x Input matrix
#' @param x.test Optional test set. Will be projected on to PCA basis
#' @param scale Logical: If TRUE, scale input data before doing SVD
#' @param center Logical: If TRUE, also center input data if \code{scale} is \code{TRUE}
#' @param ... Additional parameters to be passed to \code{PCA::PCA}
#' @return \link{rtDecom} object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_PCA <- function(x,
                  x.test = NULL,
                  k = NULL,
                  scale = TRUE,
                  center = TRUE,
                  verbose = TRUE, ...) {

  # [ Intro ] ====
  start.time <- intro(verbose = verbose)
  decom.name <- "PCA"

  # [ Arguments ] ====
  if (missing(x)) {
    print(args(d_PCA))
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
  # -> (x :matrix, x.test :matrix)

  # [ PCA ] ====
  if (verbose) msg("Performing Principal Component Analysis...")
  decom <- prcomp(x, scale = FALSE, center = FALSE, ...)
  # decom <- prcomp(~ ., data = x, scale = FALSE, center = FALSE, ...)
  rotation <- decom$rotation

  # [ Projections ] ====
  projections.train <- data.matrix(x) %*% rotation
  if (!is.null(k)) projections.train <- projections.train[, seq(k)]
  projections.test <- NULL
  if (!is.null(x.test)) {
    projections.test <- data.matrix(x.test) %*% rotation
    if (!is.null(k)) projections.test <- projections.test[, seq(k)]
  }

  # [ Outro ] ====
  extra <- list(rotation = rotation)
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = projections.test,
                    parameters = list(k = k,
                                      scale = scale,
                                      center = center),
                    extra = extra)
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d_PCA
