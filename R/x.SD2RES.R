# x.SD2RES.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io
#
# based on ANTsR::sparseDecom2boot by Avants BB
# github.io/stnava/ANTsR
# This code was originally meant to remain as close to the original as possible,
# but needs cleaning to integrate better w rtemis

#' Sparse CCA with Initialization By Resampling
#'
#' Run sparse CCA on a pair of matrices using \code{ANTsR}
#'
#' This is based on \code{ANTsR::sparseDecom2boot} by Brian B. Avants
#'
#' @inheritParams resample
#' @param x Input matrix
#' @param z Input matrix
#' @param x.test (Optional) Testing set x matrix. Will be projected on vector solution
#' @param z.test (Optional) Testing set z matrix. Will be projected on vector solution
#' @param k Integer: Number of axes to project to (i.e. number of resulting dimensions you require)
#' @param sparseness Float, length 2: Required sparseness for each matrix.
#'   Defaults to c(.01, 1)
#' @param scale.first.x Logical: If TRUE, scale \code{x} before decomposition
#' @param scale.first.center.x Logical: If TRUE, and \code{scale.first.x} is TRUE, also center \code{x}
#' @param scale.first z Logical: See above
#' @param scale.first.center.z Logical: See above
#' @param resampler Resampling method to use  (with \link{resample})#' @param
#' @param its Integer: Number of iterations for ANTsR decomposition
#' @param cthresh Integer, pair: Cluster threshold for ANTsR decomposition. Used for voxerlwise data
#'   Default = c(0, 0), which should be used for ROI data
#' @param perms Integer: Number of permutations for ANTsR decomposition
#' @param uselong Logical, binary: If 1, enforce solutions on each side to be the same. Default = 0.
#'   See \code{ANTsR::sparseDecom2("uselong")}
#' @param row.sparseness Float (0, 1]: subject / case-space sparseness
#' @param smooth Float: Smooth the data (only if mask is provided). Default = 0
#' @param robust Logical, binary: If 1, Rank-transform input matrices. Default = 0
#' @param mycoption Integer {0, 1, 2}: Enforce 1. spatial orthogonality, 2. low-dimensional orthogonality or 0. both
#'  Default = 1
#' @param initializationList List: Initialization for x. Default = list()
#' @param initializationList2 List: Initialixzation for z. Default = list()
#' @param l1 Float: L1 normalization. Default = .05
#' @param doseg Logical: If TRUE, orthogonalize matrix after each resample
#' @author Efstathios D. Gennatas; original ANTsR code by Brian B. Avants
#' @family Cross-Decomposition
#' @export

x.SD2RES = function(x, z,
                    x.test = NULL, z.test = NULL,
                    k = 4,
                    inmask = c(NA, NA),
                    sparseness = c(0.01, 1),
                    scale.first.x = FALSE,
                    scale.first.center.x = FALSE,
                    scale.first.z = FALSE,
                    scale.first.center.z = FALSE,
                    resampler = "strat.boot",
                    n.res = 4,
                    stratify.var = NULL,
                    train.p = .75,
                    strat.n.bins = 5,
                    target.length = NROW(z),
                    its = 20,
                    cthresh = c(0, 0),
                    perms = 0,
                    uselong = 0,
                    row.sparseness = 0,
                    smooth = 0,
                    robust = 0,
                    mycoption = 1,
                    initializationList = list(),
                    initializationList2 = list(),
                    l1 = 0.05,
                    doseg = TRUE,
                    priorWeight = 0.0,
                    verbose = TRUE,
                    outdir = NULL,
                    save.mod = ifelse(!is.null(outdir), TRUE, FALSE)) {

  .cosineDist <- getFromNamespace(".cosineDist", "ANTsR")
  .eanatsparsify <- getFromNamespace(".eanatsparsify", "ANTsR")

  # [ INTRO ] ====
  if (missing(x) | missing(z)) {
    print(args(x.SD2RES))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  xdecom.name <- "SD2RES"

  # [ DEPENDENCIES ] ====
  if (!depCheck("ANTsR", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  nsamp <- 1
  nvecs <- k

  # [ DATA ] ====
  x <- data.matrix(x)
  z <- data.matrix(z)
  if (is.null(colnames(x))) colnames(x) <- paste0('xFeature_', seq(NCOL(x)))
  xnames <- colnames(x)
  if (is.null(colnames(z))) colnames(z) <- paste0('zFeature_', seq(NCOL(z)))
  znames <- colnames(z)

  if (length(sparseness == 1)) sparseness <- c(sparseness, 1) # assumes (x, y), with 1D y
  x.in <- x
  y.in <- z
  if (scale.first.x) x.in <- scale(x.in, center = scale.first.center.x)
  if (scale.first.z) y.in <- scale(y.in, center = scale.first.center.z)
  inmatrix <- list(as.matrix(x.in), as.matrix(y.in))
  nsubj <- nrow(inmatrix[[1]])
  mysize <- round(nsamp * nsubj)
  mat1 <- inmatrix[[1]]
  mat2 <- inmatrix[[2]]
  mymask <- inmask
  cca1out <- 0
  cca2out <- 0
  cca1outAuto <- 0
  cca2outAuto <- 0
  bootccalist1 <- list()
  bootccalist2 <- list()
  nsubs <- nrow(mat1)
  allmat1 <- matrix( ncol = nvecs * n.res, nrow = NCOL(mat1) )
  allmat2 <- matrix( ncol = nvecs * n.res, nrow = NCOL(mat2) )
  for (i in seq(nvecs)) {
    makemat <- matrix(rep(0, n.res * ncol(mat1)), ncol = NCOL(mat1))
    bootccalist1 <- c(bootccalist1, list(makemat))
    makemat <- matrix(rep(0, n.res * ncol(mat2)), ncol = NCOL(mat2))
    bootccalist2 <- c(bootccalist2, list(makemat))
  }
  if (nsamp >= 0.999999999) doreplace <- TRUE else doreplace <- FALSE

  if (verbose) msg("Running resamples...")
  for (boots in seq(n.res)) {
    # [ RESAMPLE ] ====
    res <- resample(y = z, n.resamples = 1, resampler = resampler,
                    stratify.var = stratify.var,
                    train.p = train.p, strat.n.bins = strat.n.bins,
                    target.length = target.length)[[1]]

    mysample <- res
    submat1 <- as.matrix(mat1[mysample, ]) # delta
    submat2 <- as.matrix(mat2[mysample, ]) # delta
    sublist <- list(submat1, submat2)

    # [ sparseDecom2 ] ====
    if (verbose) msg("      Resample #", boots, " of ", n.res)
    myres <- ANTsR::sparseDecom2(inmatrix = sublist,
                                 inmask = mymask,
                                 sparseness = sparseness,
                                 nvecs = nvecs,
                                 its = its,
                                 cthresh = cthresh,
                                 perms = 0,
                                 uselong = uselong,
                                 z = row.sparseness,
                                 smooth = smooth,
                                 robust = robust,
                                 mycoption = mycoption,
                                 initializationList = initializationList,
                                 initializationList2 = initializationList2,
                                 ell1 = l1,
                                 verbose = verbose )
    myressum <- abs(diag(cor(myres$projections, myres$projections2)))
    cca1 <- (myres$eig1)
    cca2 <- (myres$eig2)
    if (boots > 1 & TRUE) {
      cca1copy <- cca1
      mymult <- matrix(rep(0, ncol(cca1) * ncol(cca1)), ncol = ncol(cca1))
      for (j in 1:ncol(cca1out)) {
        for (k in 1:ncol(cca1)) {
          temp1 <- abs(cca1out[, j])
          temp2 <- abs(cca1[, k])
          mymult[j, k] <- .cosineDist(temp1, temp2)
        }
      }
      for (ct in 1:(ncol(cca1))) {
        arrind <- which(mymult == min(mymult), arr.ind = T)
        cca1copy[, arrind[1]] <- cca1[, arrind[2]]
        mymult[arrind[1], ] <- 0
        mymult[, arrind[2]] <- 0
      }
      cca1 <- cca1copy
      ###### nextview ######
      cca2copy <- cca2
      mymult <- matrix(rep(0, ncol(cca2) * ncol(cca2)), ncol = ncol(cca2))
      for (j in 1:ncol(cca2out)) {
        for (k in 1:ncol(cca2)) {
          temp1 <- abs(cca2out[, j])
          temp2 <- abs(cca2[, k])
          mymult[j, k] <- .cosineDist(temp1, temp2)
        }
      }
      for (ct in 1:(ncol(cca2))) {
        arrind <- which(mymult == min(mymult), arr.ind = T)
        cca2copy[, arrind[1]] <- cca2[, arrind[2]]
        mymult[arrind[1], ] <- 0
        mymult[, arrind[2]] <- 0
      }
      cca2 <- cca2copy
    }
    cca1out <- cca1out + (cca1) # * myressum
    cca2out <- cca2out + (cca2) # * myressum
    bootInds <- (( boots - 1 ) * nvecs+1):(boots*nvecs)
    allmat1[,  bootInds ] <- (cca1)
    allmat2[,  bootInds ] <- (cca2)
    for (nv in 1:nvecs) {
      bootccalist1[[nv]][boots, ] <- (cca1[, nv])
      bootccalist2[[nv]][boots, ] <- (cca2[, nv])
    }
  }

  if ( doseg )
    for (k in seq(nvecs))
    {
      cca1out[, k] <-
        .eanatsparsify(abs(cca1out[, k]), sparseness[1])
      cca2out[, k] <-
        .eanatsparsify(abs(cca2out[, k]), sparseness[2])
    }
  init1 <- ANTsR::initializeEigenanatomy( t( cca1out ), inmask[[1]] )
  init2 <- ANTsR::initializeEigenanatomy( t( cca2out ), inmask[[2]] )
  if (verbose) msg("Running final decomposition using", perms, "permutations...")
  ccaout <- ANTsR::sparseDecom2(inmatrix = inmatrix,
                                inmask = c( init1$mask, init2$mask ),
                                sparseness = sparseness,
                                nvecs = nvecs,
                                its = its,
                                cthresh = cthresh,
                                perms = perms,
                                uselong = uselong,
                                z = row.sparseness,
                                smooth = smooth,
                                robust = robust,
                                mycoption = mycoption,
                                initializationList = init1$initlist,
                                initializationList2 = init2$initlist,
                                ell1 = l1,
                                priorWeight = priorWeight,
                                verbose = verbose)

  jh <- matrix( 0, nrow = ncol(inmatrix[[1]]), ncol = ncol(inmatrix[[2]]) )
  colnames(jh) <- colnames(inmatrix[[2]])
  rownames(jh) <- colnames(inmatrix[[1]])
  for (i in 1:NCOL(allmat1) ) {
    wh1 <- which( abs( allmat1[,i] ) > 1.e-10 )
    wh2 <- which( abs( allmat2[,i] ) > 1.e-10 )
    jh[ wh1, wh2 ] <- jh[ wh1, wh2 ] + 1
  }

  # [ PROJECTIONS ] ====
  scaled.xprojections <- abs(scale(as.matrix(x), center = FALSE) %*% ccaout$eig1)
  xprojections <- abs(as.matrix(x) %*% ccaout$eig1)
  scaled.zprojections <- abs(scale(as.matrix(z), center = FALSE) %*% ccaout$eig2)
  zprojections <- abs(as.matrix(z) %*% ccaout$eig2)
  scaled.test.xprojections <- scaled.test.zprojections <- test.xprojections <- test.zprojections <- NA
  if (!is.null(x.test)) {
    scaled.test.xprojections <- abs(scale(as.matrix(x.test), center = FALSE) %*% ccaout$eig1)
    test.xprojections <- abs(as.matrix(x.test) %*% ccaout$eig1)
  }
  if (!is.null(z.test)) {
    scaled.test.zprojections <- abs(scale(as.matrix(z.test), center = FALSE) %*% ccaout$eig2)
    test.zprojections <- abs(as.matrix(z.test) %*% ccaout$eig2)
  }

  # [ OUTRO ] ====
  extra <- list(cca1boot = cca1out,
                cca2boot = cca2out,
                bootccalist1 = bootccalist1,
                bootccalist2 = bootccalist2,
                allmat1 = allmat1,
                allmat2 = allmat2,
                init1 = init1,
                init2 = init2,
                jh = jh,
                scaled.xprojections = scaled.xprojections,
                scaled.zprojections = scaled.zprojections,
                scaled.test.xprojections = scaled.test.xprojections,
                scaled.test.zprojections = scaled.test.zprojections)
  rt <- rtXDecom$new(xdecom.name = xdecom.name,
                     k = k,
                     xnames = xnames,
                     znames = znames,
                     xdecom = ccaout,
                     xprojections.train = xprojections,
                     xprojections.test = test.xprojections,
                     zprojections.train = zprojections,
                     zprojections.test = test.zprojections,
                     extra = extra)
  if (save.mod) rtSave(rt, outdir, verbose = verbose)
  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::x.SD2RES
