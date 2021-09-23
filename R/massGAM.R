# massGAM.R
# ::rtemis::
# 2015 E.D. Gennatas lambdamd.org
# rtTODO: ?Save plots

#' Mass-univariate GAM Analysis
#'
#' Fits a GAM for each of multiple outcomes using a fixed set of features (many y's, one X).
# To fit GAMs for a fixed outcome using multiple different features, see \link{multiGAM}
#'
#' NA in the input will be kept as NA in the results, maintaining n of cases.
#'
#' @inheritParams s.GAM.default
#' @param y Float, Matrix / data frame: Outcomes
#' @param x.name Character: Name of the predictor
#' @param y.name Character, vector: Names of the outcomes
#' @param k Integer. Number of bases for smoothing spline
#' @param family \code{family} argument for \code{mgcv::gam}
#' @param weight Vector. Weights for GAM
#' @param method Estimation method for GAM
#' @param n.cores Integer. Number of cores to use
#' @param save.mods Logical. Should models be saved
#' @param save.summary Logical. Should model summary be saved
#' @param print.plots Logical Should plots be shown
#' @param outdir Path to save output
#' @param labeledNifti String. Path to labeled nifti file.
#' @param save.plots Logical. Should plots be saved
#' @param new.x.breaks Integer. Number of splits in the range of x
#'   to form vector of features for estimation of fitted values
#' @author Eftsthios D. Gennatas
#' @export

massGAM <- function(x, y,
                    covariates = NULL,
                    x.name = NULL, y.name = NULL,
                    k = NULL,
                    family = gaussian(),
                    weights = NULL,
                    method = "REML",
                    n.cores = rtCores,
                    save.mods = FALSE,
                    save.summary = TRUE,
                    print.plots = FALSE,
                    outdir = NULL,
                    labeledNifti = NULL,
                    save.plots = FALSE,
                    new.x.breaks = 9) {

  # [ Dependencies ] ====
  if (!depCheck("mgcv"))
    { cat("\n"); stop("Please install dependencies and try again") }

  # [ Arguments ] ====
  if (missing(x)) { print(args(massGAM)); stop("x is missing") }
  if (missing(y) & NCOL(x) == 1) { print(args(massGAM)); stop("y is missing") }
  if (is.null(n.cores)) n.cores <- parallel::detectCores()
  if (!is.null(covariates)) {
    if (!is.list(covariates)) covariates <- as.list(as.data.frame(covariates))
  }

  # [ Intro ] ====
  ptm <- proc.time()
  scriptVersion <- 0.2
  start.date <- date()
  cat(start.date, "\n::: massGAM version ", scriptVersion,
      "\nHello, ", Sys.getenv('USER'), ".\n", sep = "")

  # [ Data ] ====
  # Name cols first, because data.frame() or as.data.frame()
  # will assign colnames X1 ... Xn and V1 ... Vn respectively
  if (is.null(colnames(y))) colnames(y) <- paste0("Outcome.", 1:NCOL(y))
  y <- data.frame(y)

  x <- data.frame(x)
  if (!is.null(x.name) & NCOL(x) == length(x.name)) {
    colnames(x) <- x.name
  } else {
    colnames(x) <- paste0("Predictor.", seq(NCOL(x)))
  }

  ### Predictors
  if (!is.null(k)) {
    features <- paste0("s(",colnames(x), ", k = ", k, ")", collapse = " + ")
  } else {
    warning("k was not specified: I recommend you check results and rerun with specified k if necessary.")
    features <- paste0("s(",colnames(x), ")", collapse = " + ")
  }

  ### Covariates: ordered factors -- currently works with one main predictor
  if (!is.null(covariates)) {
    covs <- covariates
    if (is.null(names(covs))) names(covs) <- paste0("Covariate.", 1:length(covs))
    covariates <- ""
    for (j in 1:length(covs)) {
      if (is.null(k)) {
        covariates <- paste(covariates, paste0(names(covs)[j], " + s(", colnames(x)[1],
                                               ", by = ", names(covs)[j], ")" ), sep = " + ")
      } else {
        covariates <- paste(covariates, paste0(names(covs)[j], " + s(", colnames(x)[1],
                                               ", by = ", names(covs)[j], ", k = ", k, ")" ), sep = " + ")
      }}
  }

  ### Data frame
  if (is.null(covariates)) df.train <- data.frame(y, x) else df.train <- data.frame(y, x, covs)

  ### Formulae
  formulae <- lapply(1:NCOL(y),
                     function(index) as.formula(paste0(colnames(y)[index], " ~ ", features, covariates)))

  ### Output
  s.out <- list()

  # [ GAM ] ====
  # use na.exclude so that NAs give NAs when resid() is used. $residuals will still be shorter, don't use
  cat(">>> I will regress", NCOL(y), "outcomes on the following features:\n",
      paste0(features, covariates), "\n")
  cat(">>> Running", length(formulae), "GAMs on", n.cores, "cores...\n")
  mod.gam <- pbapply::pblapply(formulae, function(f) mgcv::gam(formula = f,
                                                               family = family,
                                                               data = df.train,
                                                               weights = weights,
                                                               na.action = na.exclude,
                                                               method = method),
                               cl = n.cores)
  names(mod.gam) <- names(y)

  cat("GAM fitting complete.\n")

  if (save.mods) s.out$mod.gam <- mod.gam

  # [ OUTPUT ] ====
  # summary
  cat(">>> Getting GAM summary and statistics...\n")
  mod.summary <- lapply(mod.gam, summary)
  if (save.summary) s.out$summary <- mod.summary
  # R squared
  s.out$r.sq <- data.frame(matrix(sapply(mod.summary, function(mod) c(mod$r.sq)), nrow = 1))
  colnames(s.out$r.sq) <- colnames(y)
  # P values - parameters
  s.out$p.table <- lapply(mod.summary, function(mod) return(mod$p.table))
  # P values - smooths
  s.out$s.table <- lapply(mod.summary, function(mod) return(mod$s.table))
  # P values - first smooth
  s.out$s.pv <- data.frame(matrix(sapply(mod.summary, function(mod) return(mod$s.pv[1])), nrow = 1))
  colnames(s.out$s.pv) <- colnames(y)
  # Holm-corrected P values of first smooth
  s.out$s.pv.holm <- p.adjust(s.out$s.pv, method = "holm")
  # FDR-corrected P values of first smooth
  s.out$s.pv.fdr <- p.adjust(s.out$s.pv, method = "fdr")
  #Residuals - Note: $residuals will omit NAs; use resid()
  # s.out$residuals <- data.frame(sapply(mod.gam, function(mod) return(mod$residuals)))
  s.out$residuals <- data.frame(sapply(mod.gam, resid))
  colnames(s.out$residuals) <- colnames(y)
  # Predict
  mod.pred <- lapply(mod.gam, function(mod) predict(mod, se.fit = T))
  # Fitted values
  s.out$fitted <- data.frame(sapply(mod.pred, function(mod) return(mod$fit)))
  colnames(s.out$fitted) <- colnames(y)
  # Standard error
  s.out$se.pred <- data.frame(sapply(mod.pred, function(mod) return(mod$se.fit)))
  colnames(s.out$se.pred) <- colnames(y)

  # [ RDS ]
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    outfile <- paste0(outdir, "massGAM.rds")
    saveRDS(s.out, outfile)
    if (!is.null(labeledNifti)) {
      labels2nii(s.out$s.pv.fdr, labeledNifti, paste0(outdir, "s.pv.fdr"))
      Rsq.fdr <- as.numeric(s.out$r.sq)
      Rsq.fdr[s.out$s.pv.fdr >= .05] <- 0
      labels2nii(Rsq.fdr, labeledNifti, paste0(outdir, "r.sq.fdr"))
    }
  }

  # [ Outro ]
  cat(date(), "::: massGAM version", scriptVersion, "completed.\n")
  elapsed <- proc.time() - ptm
  cat("Elapsed time:\n")
  print(elapsed)
  s.out

} # rtemis::massGAM
