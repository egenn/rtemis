# x.CCA.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Sparse Canonical Correlation Analysis (CCA)
#'
#' Run a sparse Canonical Correlation Analysis using the \code{PMA} package
#'
#' #' \code{x.CCA} runs \code{PMA::CCA}. If penaltyx is NULL, penaltyx *and* penaltyz will be estimated automatically
#' using x.CCA.permute (adapted to run in parallel)
# \link{x.SD2RES} also performs sparse decompositions / CCA using \code{ANTsR}
#'
#' @param x Matrix: Training x dataset
#' @param z Matrix: Training z dataset
#' @param x.test Matrix (Optional): Testing x set
#' @param z.test Matrix (Optional): Testing z set
#' @param y Outcome vector (Optional): If supplied, linear combinations of \code{x} and \code{z} need to be
#'   additionaly correlated with this
#' @param outcome Character: Type of outcome \code{y}: "survival", "multiclass", "quantitative"
#' @param k Integer: Number of components
#' @param niter Integer: Number of iterations
#' @param permute.niter Integer: Number of iterations to run for each permutation with \code{CCA.permute}
#' @param typex Character: "standard", "ordered". Use "standard" if columns of x are unordered; lasso
#' penalty is applied to enforce sparsity. Otherwise, use "ordered"; fused lasso penalty is applied,
#' to enforce both sparsity and smoothness.
#' @param typez Character: "standard", "ordered". Same as \code{typex} for z dataset
#' @param penaltyx Float: The penalty to be applied to the matrix x, i.e. the penalty that results
#' in the canonical vector u. If typex is "standard" then the L1 bound on u is
#' penaltyx*sqrt(ncol(x)). In this case penaltyx must be between 0 and 1 (larger L1 bound
#' corresponds to less penalization). If "ordered" then it's the fused lasso penalty lambda, which
#' must be non-negative (larger lambda corresponds to more penalization).
#' @param penaltyz Float: The penalty to be applied to the matrix z, i.e. the penalty that results
#' in the canonical vector v. If typez is "standard" then the L1 bound on v is
#' penaltyz*sqrt(ncol(z)). In this case penaltyz must be between 0 and 1 (larger L1 bound
#' corresponds to less penalization). If "ordered" then it's the fused lasso penalty lambda, which
#' must be non-negative (larger lambda corresponds to more penalization).
#' @param standardize Logical: If TRUE, center and scale columns of \code{x} and \code{z}
#' @param nperms Integer: Number of permutations to run with \code{CCA.permute}
#'   The higher, the better.
#' @param upos Logical: Require elements of u to be positive
#' @param vpos Logical: Require elements of v to be positive
#' @param verbose Logical: Print messages, including \code{trace} from \code{x.CCA.permute} and
#' \code{PMA::CCA}
#' @param n.cores Integer: Number of cores to use
#' @param outdir Path to output directory. Default = NULL
#' @param save.mod Logical: If TRUE, and \code{outdir} is defined, will save trained CCA model to \code{outdir}.
#' Default = TRUE if \code{outdir} is set, otherwise FALSE
#' @param ... Additional arguments to be passed to \code{PMA::CCA}
#' @author E.D. Gennatas
#' @family Cross-Decomposition
#' @export

x.CCA <- function(x, z,
                  x.test = NULL, z.test = NULL,
                  y = NULL,
                  outcome = NULL,
                  k = 3,
                  niter = 20,
                  nperms = 50,
                  permute.niter = 15,
                  typex = "standard",
                  typez = "standard",
                  penaltyx = NULL,
                  penaltyz = NULL,
                  standardize = TRUE,
                  upos = FALSE,
                  vpos = FALSE,
                  verbose = TRUE,
                  n.cores = rtCores,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ====
  if (missing(x) | missing(z)) {
    print(args(x.CCA))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  xdecom.name <- "CCA"

  # Dependencies ====
  if (!depCheck("PMA", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  if (is.null(n.cores)) {
    n.cores <- parallel::detectCores()
    if (verbose) msg("n.cores set to", n.cores)
  }

  # Data ====
  if (is.null(colnames(x))) colnames(x) <- paste0('xFeature_', seq(NCOL(x)))
  xnames <- colnames(x)
  if (is.null(colnames(z))) colnames(z) <- paste0('zFeature_', seq(NCOL(z)))
  znames <- colnames(z)

  # CCA permute ====
  if (is.null(penaltyx)) {
    # Run permutations to find optimal penaltyx and penaltyz
    if (verbose) msg("Running CCA.permute to estimate best penalty for x and z...")
    CCA.perm <- x.CCA.permute(x, z,
                              typex = typex,
                              typez = typez,
                              niter = permute.niter,
                              nperms = nperms,
                              standardize = standardize,
                              upos = upos,
                              vpos = vpos,
                              trace = verbose,
                              n.cores = n.cores, ...)
    .penaltyx <- CCA.perm$bestpenaltyx
    .penaltyz <- CCA.perm$bestpenaltyz
  } else {
    .penaltyx <- penaltyx
    .penaltyz <- penaltyz
  }

  # CCA ====
  if (verbose) msg("\nRunning CCA...\n")
  xnames <- colnames(x)
  znames <- colnames(z)
  xdecom <- PMA::CCA(x, z,
                  typex = typex,
                  typez = typez,
                  penaltyx = .penaltyx,
                  penaltyz = .penaltyz,
                  K = k,
                  niter = niter,
                  y = y,
                  xnames = colnames(x),
                  znames = colnames(z),
                  trace = verbose, ...)

  # Projections ====
  xprojections <- data.matrix(x) %*% xdecom$u
  scaled.xprojections <- scale(data.matrix(x)) %*% xdecom$u

  zprojections <- data.matrix(z) %*% xdecom$v
  scaled.zprojections <- scale(data.matrix(z)) %*% xdecom$v

  test.xprojections <- scaled.test.xprojections <-
    test.zprojections <- scaled.test.zprojections <- NULL
  if (!is.null(x.test)) {
    test.xprojections <- data.matrix(x.test) %*% xdecom$u
    scaled.test.xprojections <- scale(data.matrix(x.test)) %*% xdecom$u
  }

  if (!is.null(z.test)) {
    test.zprojections <- data.matrix(z.test) %*% xdecom$v
    scaled.test.zprojections <- scale(data.matrix(z.test)) %*% xdecom$v
  }


  # Outro ====
  extra <- list(CCA.perm = CCA.perm,
                scaled.xprojections = scaled.xprojections,
                scaled.zprojections = scaled.zprojections,
                scaled.test.xprojections = scaled.test.xprojections,
                scaled.test.zprojections = scaled.test.zprojections)
  rt <- rtXDecom$new(xdecom.name = xdecom.name,
                     k = k,
                     xnames = xnames,
                     znames = znames,
                     xdecom = xdecom,
                     xprojections.train = xprojections,
                     xprojections.test = test.xprojections,
                     zprojections.train = zprojections,
                     zprojections.test = test.zprojections,
                     parameters = list(k = k,
                                       niter = niter,
                                       nperms = nperms,
                                       permute.niter = permute.niter,
                                       typex = typex,
                                       typez = typez,
                                       standardize = standardize,
                                       upos = upos,
                                       vpos = vpos),
                     extra = extra)
  if (save.mod) rtSave(rt, outdir, file.prefix = "x.", verbose = verbose)
  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::x.CCA


# modified PMA::CCA.permute for parallel execution
# ::rtemis::
# 2016 lambdamd.org

#' modified PMA::CCA.permute for parallel execution
#'
#' Run PMA::CCA.permute permutation in parallel
#'
#' @author adapted by Efstathios D Gennatas; original authors: Daniela M. Witten, Robert Tibshirani
#' @noRd

x.CCA.permute <- function(x, z,
                          typex = c("standard", "ordered"),
                          typez = c("standard", "ordered"),
                          penaltyxs = NULL,
                          penaltyzs = NULL,
                          niter = 3,
                          v = NULL,
                          trace = TRUE,
                          nperms = 25,
                          standardize = TRUE,
                          chromx = NULL,
                          chromz = NULL,
                          upos = FALSE,
                          uneg = FALSE,
                          vpos = FALSE,
                          vneg = FALSE,
                          outcome = NULL,
                          y = NULL,
                          cens = NULL,
                          verbose = TRUE,
                          n.cores = rtCores) {

  CheckVs <- getFromNamespace("CheckVs", "PMA")
  ChooseLambda1Lambda2 <- getFromNamespace("ChooseLambda1Lambda2", "PMA")
  CCA.permute.justone <- getFromNamespace("CCA.permute.justone", "PMA")
  CCA.permute.xonly <- getFromNamespace("CCA.permute.xonly", "PMA")
  CCA.permute.zonly <- getFromNamespace("CCA.permute.zonly", "PMA")

  # Arguments ====
  if (is.null(n.cores)) n.cores <- parallel::detectCores()

  if (NCOL(x) < 2)
    stop("Need at least 2 features in data set x.")
  if (NCOL(z) < 2)
    stop("Need at least 2 features in data set z.")
  u <- NULL
  typex <- match.arg(typex)
  typez <- match.arg(typez)
  call <- match.call()
  if (!is.null(penaltyxs) && !is.null(penaltyzs) && length(penaltyxs) >
      1 && length(penaltyzs) > 1 && length(penaltyxs) != length(penaltyzs))
    stop("Penaltyxs and Penaltyzs must be same length, or one must have length 1. This is because tuning parameters are considered in pairs.")
  if (is.null(penaltyxs) && typex == "ordered") {
    u <- CheckVs(NULL, z, x, 1)
    penaltyxs <- c(ChooseLambda1Lambda2(as.numeric(u)))
    warning("Since type of x is ordered, the penalty for x was chosen w/o permutations.")
  }
  if (is.null(penaltyzs) && typez == "ordered") {
    v <- CheckVs(v, x, z, 1)
    penaltyzs <- c(ChooseLambda1Lambda2(as.numeric(v)))
    warning("Since type of z is ordered, the penalty for z was chosen w/o permutations.")
  }
  if (is.null(penaltyxs))
    penaltyxs <- seq(0.1, 0.7, len = 10)
  if (is.null(penaltyzs))
    penaltyzs <- seq(0.1, 0.7, len = 10)
  if (typex == "ordered" && (upos || uneg))
    stop("If type=ordered then you cannot require elements of u to be positive or negative!")
  if (typez == "ordered" && (vpos || vneg))
    stop("If type=ordered then you cannot require elements of v to be positive or negative!")
  if (length(unique(penaltyxs)) == 1 && length(unique(penaltyzs)) ==
      1) {
    out <- CCA.permute.justone(x = x, z = z, typex = typex,
                               typez = typez, penaltyx = penaltyxs[1], penaltyz = penaltyzs[1],
                               niter = niter, v = v, trace = trace, nperms = nperms,
                               standardize = standardize, chromx = chromx, chromz = chromz,
                               upos = upos, uneg = uneg, vpos = vpos, vneg = vneg,
                               outcome = outcome, y = y, cens = cens)
  }
  if (length(penaltyxs) == 1 && length(penaltyzs) > 1)
    out <- CCA.permute.zonly(x = x, z = z, typex = typex,
                             typez = typez, penaltyx = penaltyxs, penaltyzs = penaltyzs,
                             niter = niter, v = v, trace = trace, nperms = nperms,
                             standardize = standardize, chromx = chromx, chromz = chromz,
                             upos = upos, uneg = uneg, vpos = vpos, vneg = vneg,
                             outcome = outcome, y = y, cens = cens)
  if (length(penaltyxs) > 1 && length(penaltyzs) == 1)
    out <- CCA.permute.xonly(x = x, z = z, typex = typex,
                             typez = typez, penaltyxs = penaltyxs, penaltyz = penaltyzs,
                             niter = niter, v = v, trace = trace, nperms = nperms,
                             standardize = standardize, chromx = chromx, chromz = chromz,
                             upos = upos, uneg = uneg, vpos = vpos, vneg = vneg,
                             outcome = outcome, y = y, cens = cens)
  if (length(penaltyzs) > 1 && length(penaltyxs) > 1)
    out <- x.CCA.permute.both(x = x, z = z, typex = typex,
                              typez = typez, penaltyxs = penaltyxs, penaltyzs = penaltyzs,
                              niter = niter, v = v, trace = trace, nperms = nperms,
                              standardize = standardize, chromx = chromx, chromz = chromz,
                              upos = upos, uneg = uneg, vpos = vpos, vneg = vneg,
                              outcome = outcome, y = y, cens = cens,
                              verbose = verbose, n.cores = n.cores)

  out$call <- call
  out$upos <- upos
  out$uneg <- uneg
  out$vpos <- vpos
  out$vneg <- vneg
  class(out) <- "CCA.permute"
  out
} # rtemis::x.CCA.permute


# x.CCA.permute.both
# ::rtemis::
# lambdamd.org

#' modified PMA:::CCA.permute.both for parallel execution
#'
#' Run PMA::CCA.permute.both permutations in parallel
#'
#' @author adapted by Efstathios D Gennatas; original authors: Daniela M Witten, Robert Tibshirani
#' @noRd

x.CCA.permute.both <- function(x, z,
                               typex,
                               typez,
                               penaltyxs,
                               penaltyzs,
                               niter,
                               v,
                               trace,
                               nperms,
                               standardize,
                               chromx,
                               chromz,
                               upos,
                               uneg,
                               vpos,
                               vneg,
                               outcome,
                               y,
                               cens,
                               verbose = TRUE,
                               n.cores = rtCores,
                               parallel.type = ifelse(.Platform$OS.type == "unix", "fork", "psock")) {

  ftrans <- getFromNamespace("ftrans", "PMA")
  CheckVs <- getFromNamespace("CheckVs", "PMA")

  # Dependencies ====
  if (!depCheck(c("PMA", "pbapply"), verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  if (is.null(n.cores)) n.cores <- parallel::detectCores()

  call <- match.call()
  if (standardize) {
    x <- scale(x, TRUE, TRUE)
    z <- scale(z, TRUE, TRUE)
  }
  v <- CheckVs(v, x, z, 1)
  ccperms = nnonzerous.perms = nnonzerovs.perms = matrix(NA,
                                                         length(penaltyxs),
                                                         nperms)
  ccperms1 <- rep(NA, length(penaltyxs))
  ccs = nnonzerous = nnonzerovs = numeric(length(penaltyxs))

  # Cluster ====
  pbapply.type <- if (verbose) "timer" else "none"
  pbapply::pboptions(type = pbapply.type)
  if (n.cores > 1) {
    if (parallel.type == "psock") {
      if (verbose) msg("Starting PSOCK cluster on", n.cores, "cores...")
      cl <- makePSOCKcluster(n.cores)
      on.exit(stopCluster(cl))
      clusterEvalQ(cl, library("rtemis"))
    } else {
      if (verbose) msg("Parallelizing by forking on", n.cores, "cores...")
      cl <- n.cores
    }
  } else {
    cl <- 1
  }

  # Permutations ====
  # pbapply version
  mango <- pbapply::pblapply(seq(nperms), FUN = function(i) {
    sampz <- sample(NROW(z))
    sampx <- sample(NROW(x))
    for (j in seq(length(penaltyxs))) {
      if (trace && .Platform$OS.type != "windows")
        cat(j, fill = FALSE)
      if (i == 1) {
        out <- PMA::CCA(x, z, typex = typex, typez = typez,
                        penaltyx = penaltyxs[j], penaltyz = penaltyzs[j],
                        y = y, outcome = outcome, cens = cens, niter = niter,
                        v = v, trace = FALSE, upos = upos, uneg = uneg,
                        vpos = vpos, vneg = vneg, standardize = FALSE,
                        chromz = chromz, chromx = chromx)
        nnonzerous[j] <- sum(out$u != 0)
        nnonzerovs[j] <- sum(out$v != 0)
        if (mean(out$u == 0) != 1 && mean(out$v == 0) !=
            1) {
          ccs[j] <- cor(x %*% out$u, z %*% out$v)
        }
        else {
          ccs[j] <- 0
        }
      }
      out <- PMA::CCA(x[sampx, ], z[sampz, ], typex = typex,
                      typez = typez, penaltyx = penaltyxs[j], penaltyz = penaltyzs[j],
                      y = y, outcome = outcome, cens = cens, niter = niter,
                      v = v, trace = FALSE, upos = upos, uneg = uneg,
                      vpos = vpos, vneg = vneg, standardize = FALSE,
                      chromz = chromz, chromx = chromx)
      nnonzerous.perms[j, i] <- sum(out$u != 0) # rtCheck no visible binding for i
      nnonzerovs.perms[j, i] <- sum(out$v != 0) # rtCheck no visible binding for i
      if (mean(out$u == 0) != 1 && mean(out$v == 0) !=
          1) {
        ccperms1[j] <- cor(x[sampx, ] %*% out$u, z[sampz,
                                                   ] %*% out$v)
      }
      else {
        ccperms1[j] <- 0
      }
    }
    list(ccs = ccs, ccperms1 = ccperms1) # foreach out
  }, cl = n.cores) # END PERMUTATIONS keep last ccs, whole ccperms matrix

  ccs <- mango[[length(mango)]]$ccs
  ccperms <- sapply(mango, function(i) c(i$ccperms1))
  cc.norm <- ftrans(ccs)
  ccperm.norm <- ftrans(ccperms)
  zstats <- (cc.norm - rowMeans(ccperm.norm))/(apply(ccperm.norm, 1, sd) + 0.05)
  if (trace) cat(fill = T)
  pvals <- apply(sweep(ccperms, 1, ccs, "-") >= 0, 1, mean)
  results <- list(zstats = zstats,
                  penaltyxs = penaltyxs,
                  penaltyzs = penaltyzs,
                  bestpenaltyx = penaltyxs[which.max(zstats)],
                  bestpenaltyz = penaltyzs[which.max(zstats)],
                  cors = ccs,
                  corperms = ccperms,
                  ft.cors = cc.norm,
                  ft.corperms = rowMeans(ccperm.norm),
                  nnonzerous = nnonzerous, nnonzerovs = nnonzerovs,
                  nnonzerous.perm = rowMeans(nnonzerous.perms),
                  nnonzerovs.perm = rowMeans(nnonzerovs.perms),
                  call = call,
                  v.init = v, pvals = pvals, nperms = nperms, chromz = chromz,
                  chromx = chromx, typex = typex, typez = typez, pvalbestz = pvals[which.max(zstats)])
  results
} # rtemis::x.CCA.permute.both
