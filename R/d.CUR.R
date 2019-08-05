# d.CUR.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' CUR Decomposition
#'
#' Performs CUR decomposition using \code{rCUR::CUR}
#'
#' Note that \code{k} here does not correspond with \code{k} in the other decomposition functions.
#' Use \code{c} to determine dimensionality of resulting decomposition
#' @param x Numeric matrix / data.frame: Input data
#' @param c Integer: Number of columns to be selected
#' @param r Integer: Number of rows to be selected
#' @param k Integer: Rank of decomposition (Creates \code{k} components)
#' @param sv The SVD of \code{x}, if already available
#' @param method Character: "random", "top.scores", "ortho.top.scores",
#' "exact.num.random", "highest.ranks" (Default). See \code{rCUR::CUR("method")}
#' @param matrix.return Logical: if TRUE, matrices C, U, and R are returned. If FALSE,
#' U is not computed, which can be expensive, if r and c are large. Default = TRUE
#' @param error.return Logical: if TRUE, the Frobenius norm of the difference between the original matrix and the
#' CUR approximation is returned. Effective only if \code{matrix.return = TRUE}. Default = FALSE
#' @param scale Logical: If TRUE, scale input
#' @param center Logical: If TRUE, center input
#' @param verbose Logical: If TRUE, print messages to output
#' @param ... Additional parameters to be passed to \code{rCUR::CUR}
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @family Decomposition
#' @export

d.CUR <- function(x,
                  c = dim(x)[2],
                  r = dim(x)[1],
                  k = NULL,
                  sv = NULL,
                  method = "highest.ranks",
                  matrix.return = TRUE,
                  error.return = FALSE,
                  scale = TRUE,
                  center = TRUE,
                  verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  decom.name <- "CUR"

  # [ DEPENDENCIES ] ====
  if (!depCheck("rCUR", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ DATA ] ====
  x <- as.matrix(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0('Feature_', seq(NCOL(x)))
  xnames <- colnames(x)
  if (scale) {
    x <- scale(x, center = center)
  }

  # [ CUR ] ====
  if (verbose) msg("Running CUR Decomposition...")
  decom <- rCUR::CUR(x, c = c, r = r, k = k,
                     sv = sv,
                     method = method,
                     matrix.return = matrix.return,
                     error.return = error.return, ...)

  # [ PROJECTIONS ] ====
  projections.train <- rCUR::getC(decom)

  # [ OUTRO ] ====
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = NULL,
                    parameters = list(c = c,
                                      r = r,
                                      k = k,
                                      sv = sv,
                                      scale = scale,
                                      method = method))
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.CUR
