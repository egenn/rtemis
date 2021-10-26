# modError.R
# ::rtemis::
# 2015-8 E.D. Gennatas lambdamd.org

#' Error Metrics for Supervised Learning
#'
#' Calculate error metrics for pair of vector, e.g. true and estimated values from a model
#'
#' In regression, NRMSE = RMSE / range(observed)
#' @param true Vector: True values
#' @param estimated Vector: Estimated values
#' @param estimated.prob Vector: Estimated probabilities for Classification, if available. Default = NULL
#' @param verbose Logical: If TRUE, print output to screen
#' @param type Character: "Regression", "Classification", or "Survival". If not provided, will be set
#' to Regression if y is numeric.
#' @param rho Logical: If TRUE, calculate Spearman's rho. Default = TRUE
#' @param tau Logical: If TRUE, calculate Kendall's tau. Default = FALSE
#'   This can be slow for long vectors
#' @param na.rm Logical: Passed to \code{mean} and \code{range} functions. Default = TRUE
#' @return Object of class \code{modError}
#' @author E.D. Gennatas
#' @export

modError <- function(true,
                     estimated,
                     estimated.prob = NULL,
                     verbose = FALSE,
                     type = NULL,
                     rho = TRUE,
                     tau = FALSE,
                     na.rm = TRUE) {

  x <- true
  y <- estimated

  if (is.null(type)) {
    if (survival::is.Surv(x)) {
      type <- "Survival"
    } else {
      type <- if (is.factor(y)) "Classification" else "Regression"
    }
  }

  # Check input lengths match (NROW works with Survival object as well as vectors)
  if (NROW(x) != NROW(y)) stop("Input lengths do not match:\n    Length of true = ", NROW(x),
                               "\n    Length of estimated = ", NROW(y))
  if (NROW(x) < 2) {
    # if (verbose) msg("Vector of length 1; no error estimated for LOOCV; estimate aggregate error")
    if (verbose) warning("Vector of length 1; no per resample test error can be estimated for LOOCV; estimate aggregate error")
    return(NULL)
  }

  if (type == "Regression") {

    # [ Regression ] ====
    x <- as.numeric(x)
    y <- as.numeric(y)
    error <- x - y
    mae <- mean(abs(error), na.rm = na.rm)
    mse <- mean(error^2, na.rm = na.rm)
    rmse <- sqrt(mse)
    nrmse <- rmse/diff(range(x, na.rm = na.rm))
    if (length(y) > 2) {
      corr <- suppressWarnings(cor.test(x, y))
      r <- corr$estimate
      r.p <- corr$p.value
    } else {
      r <- r.p <- NA
    }

    # Sum of Squared Errors of prediction (SSE) a.k.a. Residual Sum of Squares (RSS)
    SSE <- sum((x - y)^2)
    # Sum of Squares due to Regression (SSR) a.k.a. Explained Sum of Squares (ESS)
    SSR <- sum((mean(x) - y)^2)
    # Total Sum of Squares (TSS or SST)
    SST <- sum((x - mean(y))^2)
    # R-squared a.k.a. Coefficient of Determination i.e. percent variance explained
    Rsq <- 1 - (SSE / SST)

    # Standard Error of the Estimate
    stderr <- sqrt((sum((x - y)^2)) / length(x))

    # Error of expectation(x) and percent reduction
    error.exp <- x - mean(x)
    mae.exp <- mean(abs(error.exp))
    mae.red <- (mae.exp - mae) / mae.exp
    mse.exp <- mean(error.exp^2)
    mse.red <- (mse.exp - mse) / mse.exp
    rmse.exp <- sqrt(mse.exp)
    rmse.red <- (rmse.exp - rmse) / rmse.exp

    s.out <- data.frame(MAE = mae,
                        MSE = mse,
                        RMSE = rmse,
                        NRMSE = nrmse,
                        MAE.EXP = mae.exp,
                        MAE.RED = mae.red,
                        MSE.EXP = mse.exp,
                        MSE.RED = mse.red,
                        RMSE.EXP = rmse.exp,
                        RMSE.RED = rmse.red,
                        r = r,
                        r.p = r.p,
                        SSE = SSE,
                        SSR = SSR,
                        SST = SST,
                        Rsq = Rsq,
                        stderr = stderr,
                        row.names = NULL)
    if (rho) {
      s.out$rho <- suppressWarnings(cor(x, y, method = "spearman"))
      s.out$rho.p <- suppressWarnings(cor.test(x, y, method = "spearman")$p.value)
    }
    if (tau) {
      s.out$tau <- cor(x, y, method = "kendall")
      s.out$tau.p <- cor.test(x, y, method = "kendall")$p.value
    }

    if (verbose) print(s.out, row.names = FALSE)
    class(s.out) <- c("regError", "data.frame")

  } else if (type == "Classification")  {

    # [ Classification ] ====
    if (class(x) != "factor") x <- as.factor(x)
    # if (class(y) != "factor") y <- as.factor(y)
    n.classes <- length(levels(x))
    if (n.classes < 2) stop("Classification requires at least two classes")
    s.out <- classError(x, y, estimated.prob)

  } else {

    # [ Survival ] ====
    s.out <- survError(x, y)

  }

  s.out

} # rtemis::modError


#' Error functions
#'
#' Convenience functions for calculating loss.
#' These can be passed as arguments to learners that support custom loss functions.
#'
#' @rdname error
#' @param x Vector of True values
#' @param y Vector of Estimated values
#' @param na.rm Logical: If TRUE, remove NA values before computation. Default = TRUE
#' @export
mse <- function(x, y, na.rm = TRUE) {

  error <- x - y
  mean(error^2, na.rm = na.rm)

} # rtemis::mse


#' Weighted MSE
#'
#' @rdname error
#' @param weights Float, vector: Case weights
#' @export
msew <- function(x, y, weights = rep(1, length(y)), na.rm = TRUE) {

  error <- x - y
  error <- error * weights
  mean(error^2, na.rm = na.rm)

} # rtemis::msew


#' @rdname error
#' @export
rmse <- function(x, y, na.rm = TRUE) {

  sqrt(mse(x, y, na.rm = na.rm))

} # rtemis::rmse


#' @rdname error
#' @export
mae <- function(x, y, na.rm = TRUE) {

  error <- x - y
  mean(abs(error), na.rm = na.rm)

} # rtemis::mae


#' R-squared
#'
#' @param x Float, vector: True values
#' @param y Float, vector: Estimated values
#' @author E.D. Gennatas
#' @export

rsq <- function(x, y) {
  SSE <- sum((x - y)^2)
  # Sum of Squares due to Regression (SSR) a.k.a. Explained Sum of Squares (ESS)
  SSR <- sum((mean(x) - y)^2)
  # Total Sum of Squares (TSS or SST)
  SST <- sum((x - mean(y))^2)
  # R-squared a.k.a. Coefficient of Determination i.e. percent variance explained
  1 - (SSE / SST)
} # rtemis::rsq


#' Log Loss for a binary classifier
#'
#' @param true Factor: True labels. First level is the positive case
#' @param estimated.prob Float, vector: Estimated probabilities
#' @author E.D. Gennatas
#' @export

logloss <- function(true, estimated.prob) {

  true.bin <- 2 - as.numeric(true)
  eps <- 1e-16
  estimated.prob <- pmax(pmin(estimated.prob, 1 - eps), eps)
  - mean(true.bin * log(estimated.prob) + (1 - true.bin) * log(1 - estimated.prob))

} # rtemis::logloss


#' Sensitivity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param estimated Estimated labels
#' @param harmonize Logical: If TRUE, run \link{factorHarmonize} first
#' @param verbose Logical: If TRUE, print messages to output. Default = TRUE
#' @export

sensitivity <- function(true, estimated,
                        harmonize = FALSE,
                        verbose = TRUE) {

  if (harmonize) estimated <- factorHarmonize(true, estimated, verbose = verbose)
  pos.index <- true == levels(true)[1]
  condition.pos <- sum(pos.index)
  true.pos <- sum(true[pos.index] == estimated[pos.index])
  true.pos/condition.pos

}


#' Specificity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param estimated Estimated labels
#' @param harmonize Logical: If TRUE, run \link{factorHarmonize} first
#' @param verbose Logical: If TRUE, print messages to output. Default = TRUE
#' @export

specificity <- function(true, estimated,
                        harmonize = FALSE,
                        verbose = TRUE) {

  if (harmonize) estimated <- factorHarmonize(true, estimated, verbose = verbose)
  neg.index <- true == levels(true)[2]
  condition.neg <- sum(neg.index)
  true.neg <- sum(true[neg.index] == estimated[neg.index])
  true.neg/condition.neg

}


#' Balanced Accuracy
#'
#' Balanced Accuracy of a binary classifier
#'
#' BAcc = .5 * (Sensitivity + Specificity)
#'
#' @param true True labels
#' @param predicted Estimated labels
#' @param harmonize Logical: passed to \link{sensitivity} and \link{specificity}, which use \link{factorHarmonize}.
#' Default = FALSE
#' @param verbose Logical: If TRUE, print messages to output
#' @export

bacc <- function(true, predicted,
                 harmonize = FALSE,
                 verbose = TRUE) {

  .5 * (sensitivity(true, predicted, harmonize = harmonize, verbose = verbose) +
          specificity(true, predicted, harmonize = harmonize, verbose = verbose))

}

#' Precision (aka PPV)
#'
#' The first factor level is considered the positive case.
#'
#' @param true Factor: True labels
#' @param estimated Factor: Estimated labels
#' @param harmonize Logical: If TRUE, run \link{factorHarmonize} first
#' @param verbose Logical: If TRUE, print messages to output. Default = TRUE
#' @export

precision <- function(true, estimated,
                        harmonize = FALSE,
                        verbose = TRUE) {

  if (harmonize) estimated <- factorHarmonize(true, estimated, verbose = verbose)
  tbl <- table(estimated, true)
  predicted.totals <- rowSums(tbl)[1]
  hits <- diag(tbl)[1]

  if (hits == 0 & predicted.totals == 0) {
    1
  } else {
    hits/predicted.totals
  }

} # rtemis::precision

#' Factor harmonize
#'
#' @param x Input factor
#' @param reference Reference factor
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
# #' @param allow.rename Logical: If TRUE, allow renaming - not simply reordering - factor levels of input \code{x}
#' @export

factorHarmonize <- function(reference, x,
                            verbose = TRUE) {

  if (!is.factor(x) | !is.factor(reference)) stop("Inputs must be factors")
  if (!all(levels(x) == levels(reference))) {
    if (!all(levels(x) %in% levels(reference))) {
      if (verbose) msg("Levels of x:")
      levels(x)
      if (verbose) msg("levels of reference:")
      levels(reference)
      stop("Levels of two inputs do not match")
    }
    if (verbose) msg("Input factor levels are not in the same order, correcting")
    x <- factor(x, levels = levels(reference))
  }
  x

} # rtemis::factorHarmonize


# print.regError.R
# ::rtemis::
# 2016-8 E.D. Gennatas lambdamd.org

#' Print \code{regError} object
#'
#' \code{print} \code{regError} object
#'
#' @method print regError
#' @param x \code{regError} object
#' @author E.D. Gennatas
#' @export

print.regError <- function(x, ...) {

  obj <- as.data.frame(x)
  cat("    MSE = ", ddSci(obj$MSE), " (", ddSci(obj$MSE.RED * 100), "%)\n", sep = "")
  cat("   RMSE = ", ddSci(obj$RMSE), " (", ddSci(obj$RMSE.RED * 100), "%)\n", sep = "")
  cat("    MAE = ", ddSci(obj$MAE), " (", ddSci(obj$MAE.RED * 100), "%)\n", sep = "")
  cat("      r = ", ddSci(obj$r), " (p = ", ddSci(obj$r.p), ")\n", sep = "")
  if (!is.null(obj$rho)) {
    cat("    rho = ", ddSci(obj$rho), " (p = ", ddSci(obj$rho.p), ")\n", sep = "")
  }
  cat("   R sq = ", rtHighlight$bold(ddSci(obj$Rsq)), "\n", sep = "")

} # rtemis::print.regError
