# d_ICA.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org
# TODO: x.test

#' Independent Component Analysis
#'
#' Perform ICA decomposition using the fastICA algorithm in \code{fastICA::fastICA} or \code{ica::fastica}
#'
#' Project scaled variables to ICA components.
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' fastICA will be applied to the transpose of the n x p matrix.
#' fastICA will fail if there are any NA values or constant features: remove them using \link{preprocess}
#'
#' @inheritParams d_SVD
#' @param x Input data
# @param x.test Optional test set. Will be projected on to ICA basis
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param package Character: Which package to use for ICA. "fastICA" will 
#' use \code{fastICA::fastICA},
#' "ica" will use \code{ica::fastica}. Default = "fastICA".
#' Note: only \code{fastICA} works with \code{k = 1}
#' @param alg.type Character: For \code{package = "fastICA"}, "parallel" or 
#' "deflation".
#' @param maxit Integer: Maximum N of iterations
#' @param scale Logical: If TRUE, scale input data before decomposition. 
#' @param center Logical: If TRUE, also center input data if \code{scale} 
#' is \code{TRUE}.
#' @param trace Integer: If > 0, print messages during ICA run. Default = 0
#' @param ... Additional parameters to be passed to \code{fastICA::fastICA} 
#' or \code{ica::icafast}
#' 
#' @return \link{rtDecom} object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_ICA <- function(x,
                  k = 3,
                  package = c("fastICA", "ica"),
                  alg.type = "parallel",
                  maxit = 100,
                  scale = TRUE,
                  center = TRUE,
                  verbose = TRUE,
                  trace = 0, ...) {

  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "ICA"
  package <- match.arg(package)

  # Dependencies ----
  if (package == "fastICA") {
    dependency_check("fastICA")
  } else {
    dependency_check("ica")
  }

  # Arguments ----
  if (missing(x)) {
    print(args(d_ICA))
    stop("x is missing")
  }

  # Data ----
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0('Feature_', seq(NCOL(x)))
  xnames <- colnames(x)
  # if (!is.null(x.test)) colnames(x.test) <- xnames
  if (scale) {
    x <- as.data.frame(scale(x, center = center))
    # if (!is.null(x.test)) x.test <- as.data.frame(scale(x.test, center = center))
  }

  # ICA ----
  if (verbose) msg("Running Independent Component Analysis...")
  if (package == "fastICA") {
    decom <- fastICA::fastICA(x,
                              n.comp = k,
                              method = "C",
                              alg.typ = alg.type,
                              maxit = maxit,
                              verbose = trace > 0, ...)
  } else {
    decom <- ica::icafast(x,
                          nc = k,
                          center = TRUE,
                          maxit = maxit,
                          alg = substr(alg.type, 1, 3), ...)
  }

  # Projections ----
  projections.train <- decom$S
  projections.test <- NULL
  colnames(projections.train) <- paste0("ICA", seq(k))

  # Outro ----
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = projections.test,
                    parameters = c(list(k = k,
                                        package = package,
                                        alg.type = alg.type,
                                        maxit = maxit,
                                        scale = scale),
                                   list(...)))
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d_ICA
