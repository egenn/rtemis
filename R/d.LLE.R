# d.LLE.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Locally Linear Embedding
#'
#' Perform LLE decomposition using \code{lle::lle}
#'
#' Project scaled variables to LLE components
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#' LLE will be applied to the transpose of the n x p matrix.
#'
#' @inheritParams d.SVD
#' @param x Input data
#' @param k Integer vector of length 1 or greater. Rank of decomposition
#' @param nn Integer: Number of neighbors. If Set to 0 (default), will use \code{lle::calc_k} to
#'   estimate optimal number
#' @param nn.min Integer: Minimum n of neighbors to consider in search, used if \code{nn = 0}
#' @param nn.max Integer: Maximum n of enighbors to consider in search, used if \code{nn = 0}
#' @param plot.calcnn Logical: If TRUE, print plot after estimation of number of neighbors. Default = FALSE
#' @param id Logical: If TRUE, calculate \code{k} (the intrinsic dimension)
#' @param iLLE Logical: If TRUE, use the improved LLE algorithm; see Details in \code{lle::lle}
#'   Notice: It causes warnings for matrix dimensions (check \code{lle} code)
#' @param nnk Logical: If TRUE, use k nearest neighbors method; otherwise, epsilon environment neighbourhood
#'   will be used
#' @param reg Integer {1, 2, 3}: Regularization methods: See \code{lle::lle("reg")}
#' @param v Float: Threshold value for intrinsic dimension estimation. Suggestion for noiseless
#' data: .99, for noisy data: .9. Default = .9
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional parameters to be passed to \code{LLE::LLE}
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @family Decomposition
#' @export

d.LLE <- function(x,
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
                  n.cores = rtCores, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  decom.name <- "LLE"

  # [ DEPENDENCIES ] ====
  if (!depCheck("lle", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(d.LLE))
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
  # cat("    (If this is not what you intended, this would be the time to interrupt the run)\n")
  if (is.null(colnames(x))) colnames(x) <- paste0('Feature_', seq(NCOL(x)))
  xnames <- colnames(x)

  # [ LLE ] ====
  if (nn == 0) {
    if (verbose) msg("Estimating optimal number of neighbors...")
    loadNamespace("snow")
    # setDefaultClusterOptions <- snow::setDefaultClusterOptions
    # nn <- lle::calc_k(x, m = k, kmin = nn.min, kmax = nn.max, plotres = plot.calcnn,
    #                   parallel = ifelse(n.cores > 1, TRUE, FALSE), cpus = n.cores)
    nn <- rt_lle_calc_k(x, m = k, kmin = nn.min, kmax = nn.max, plotres = plot.calcnn,
                        n.cores = n.cores)
    nn <- nn$k[which.min(nn$rho)]
  }
  if (verbose) msg("Performing Locally Linear Embedding...")
  decom <- suppressWarnings(lle::lle(X = x,
                                     m = k,
                                     k = nn,
                                     reg = reg,
                                     v = v,
                                     id = id,
                                     iLLE = iLLE, ...))

  # [ PROJECTIONS ] ====
  projections.train <- decom$Y

  # [ OUTRO ] ====
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

} # rtemis::d.LLE

# This function is taken from the lle package; added "snowfall::" where appropriate
# calc_k <- function (X, m, kmin = 1, kmax = 20, plotres = TRUE, parallel = FALSE,
#                     cpus = 2, iLLE = FALSE)
# {
#   N <- dim(X)[1]
#   if (kmax >= N)
#     kmax <- N - 1
#   if (.Platform$OS.type == "windows")
#     dev <- "nul"
#   else dev <- "/dev/null"
#   if (parallel == TRUE)
#     snowfall::sfInit(parallel = TRUE, cpus = cpus)
#   else snowfall::sfInit(parallel = FALSE)
#   options(warn = -1)
#   sfLibrary(lle)
#   options(warn = 0)
#   perform_calc <- function(k, X, m, iLLE = FALSE) {
#     N <- dim(X)[1]
#     sink(dev)
#     Y <- lle(X, m, k, 2, 0, iLLE = iLLE)$Y
#     sink()
#     Dx <- as.matrix(dist(X))
#     Dy <- as.matrix(dist(Y))
#     rho <- c()
#     for (i in 1:N) rho <- c(rho, cor(Dx[i, ], Dy[i, ]))
#     return(mean(1 - rho^2))
#   }
#   rho <- invisible(snowfall::sfLapply(kmin:kmax, perform_calc, X, m,
#                             iLLE))
#   rho <- unclass(unlist(rho))
#   sfStop()
#   res <- data.frame(k = c(kmin:kmax), rho = rho)
#   if (plotres) {
#     par(mar = c(5, 5, 4, 2) + 0.1)
#     plot(res$k, res$rho, type = "b", xlab = "k", ylab = expression(1 -
#                                                                      rho^2), main = "")
#     abline(h = min(res$rho, na.rm = TRUE), col = "red")
#     grid()
#   }
#   else cat("best k:", head(res$k[order(res$rho)], 3), "\n\n")
#   return(res)
# }
