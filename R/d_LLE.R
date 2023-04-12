# d_LLE.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org
# Replace with RDRToolbox function

#' Locally Linear Embedding
#'
#' Perform LLE decomposition using `lle::lle`
#'
#' Project scaled variables to LLE components
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' LLE will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d_SVD
#' @param x Input data
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param nn Integer: Number of neighbors. If Set to 0 (default), will use `lle::calc_k` to
#'   estimate optimal number
#' @param nn.min Integer: Minimum n of neighbors to consider in search, used if `nn = 0`
#' @param nn.max Integer: Maximum n of enighbors to consider in search, used if `nn = 0`
#' @param plot.calcnn Logical: If TRUE, print plot after estimation of number of neighbors. Default = FALSE
#' @param id Logical: If TRUE, calculate `k` (the intrinsic dimension)
#' @param iLLE Logical: If TRUE, use the improved LLE algorithm; see Details in `lle::lle`
#'   Notice: It causes warnings for matrix dimensions (check `lle` code)
#' @param nnk Logical: If TRUE, use k nearest neighbors method; otherwise, epsilon environment neighbourhood
#'   will be used
#' @param reg Integer {1, 2, 3}: Regularization methods: See `lle::lle("reg")`
#' @param v Float: Threshold value for intrinsic dimension estimation. Suggestion for noiseless
#' data: .99, for noisy data: .9. Default = .9
#' @param n.cores Integer: Number of cores to use. Default = 1. At some point using more than one cores stopped working.
#' The `lle` package has not been updated since February 2015 - we will switch to a different implementation soon
#' @param ... Additional parameters to be passed to `LLE::LLE`
#' @return [rtDecom] object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_LLE <- function(x,
                  k = 2,
                  nn = 0,
                  nn.min = 1,
                  nn.max = 20,
                  plot.calcnn = FALSE,
                  id = FALSE,
                  iLLE = FALSE,
                  nnk = TRUE,
                  reg = 2,
                  v = .9,
                  verbose = TRUE,
                  n.cores = 1, ...) {

  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "LLE"

  # Dependencies ----
  dependency_check("lle")

  # Arguments ----
  if (missing(x)) {
    print(args(d_LLE))
    stop("x is missing")
  }

  # Data ----
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg2("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg2("    interpreted as", n, "cases with", p, "features.")
  }

  if (is.null(colnames(x))) colnames(x) <- paste0('Feature_', seq(NCOL(x)))
  xnames <- colnames(x)

  # LLE ----
  if (nn == 0) {
    if (verbose) msg2("Estimating optimal number of neighbors...")
    nn <- rt_lle_calc_k(x, m = k,
                        kmin = nn.min,
                        kmax = nn.max,
                        plotres = plot.calcnn,
                        n.cores = n.cores,
                        verbose = verbose)
    nn <- nn$k[which.min(nn$rho)]
  }
  if (verbose) msg2("Performing Locally Linear Embedding...")
  decom <- suppressWarnings(lle::lle(X = x,
                                     m = k,
                                     k = nn,
                                     reg = reg,
                                     v = v,
                                     id = id,
                                     iLLE = iLLE, ...))

  # Projections ----
  projections.train <- decom$Y

  # Outro ----
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = decom,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = NULL,
                    parameters = list(k = k,
                                      nn = nn,
                                      nn.min = nn.min,
                                      nn.max = nn.max,
                                      plot.calcnn = plot.calcnn,
                                      id = id,
                                      iLLE = iLLE,
                                      nnk = nnk,
                                      reg = reg,
                                      v = v))
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d_LLE
