# modError.R
# ::rtemis::
# 2015-8 Efstathios D. Gennatas egenn.github.io

#' Error Metrics for Supervised Learning
#'
#' Calculate error metrics for pair of vector, e.g. true and estimated values from a model
#'
#' For classification, caret's confusion matrix is used, modified to include $byClass measures under
#' $overall. In the case of multiclass outcome, these are averaged.
#'
#' In regression, NRMSE = RMSE / range(observed)
#' @param true Vector: True values
#' @param estimated Vector: Estimated values
#' @param verbose Logical: If TRUE, print output to screen
#' @param type String: "Regression", "Classification", or "Survival". If not provided, will be set
#' to Regression if y is numeric.
#' @param rho Logical: If TRUE, calculate Spearman's rho. Default = TRUE
#' @param tau Logical: If TRUE, calculate Kendall's tau. Default = FALSE
#'   This can be slow for long vectors
#' @param na.rm Logical: Passed to \code{mean} and \code{range} functions. Default = TRUE
#' @return Object of class \code{modError}
#' @author Efstathios D Gennatas
#' @export

modError <- function(true, estimated,
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

    if (verbose) print(s.out, row.names = F)
    class(s.out) <- c("regError", "data.frame")

  } else if (type == "Classification")  {
    
    # [ Classification ] ====
    if (!depCheck("caret", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    if (class(x) != "factor") x <- as.factor(x)
    # if (class(y) != "factor") y <- as.factor(y)
    n.classes <- length(levels(x))
    if (n.classes < 2) stop("Classification requires at least two classes")
    # s.out <- caret::confusionMatrix(y, x, mode = "everything")
    # byClass <- if (n.classes == 2) {
    #   data.frame(t(s.out$byClass))
    # } else {
    #   data.frame(t(colMeans(s.out$byClass)))
    # }
    # names(byClass) <- c("Sensitivity", "Specificity", "PosPredValue", "NegPredValue",
    #                     "F1", "Prevalence", "DetectionRate",
    #                     "DetectionPrevalence", "Balanced Accuracy")
    # s.out$overall <- data.frame(t(s.out$overall), byClass)
    s.out <- classError(x, y, estimated.prob)
  } else {
    
    # [ Survival ] ====
    if (!depCheck("survival", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    # s.out <- survcop::concordance(y, x)
    if (!survival::is.Surv(x)) stop("true must be Survival object")
    # s.out <- as.data.frame(t(survival::survConcordance.fit(x, y)))
    s.out <- survival::survConcordance(x ~ y)
    names(s.out)[1] <- "Concordance"
  }

  return(s.out)

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
#' @author Efstathios D. Gennatas
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
#' @author Efstathios D. Gennatas
#' @export

logloss <- function(true, estimated.prob) {
  
  true.bin <- 2 - as.numeric(true)
  eps <- 1e-16
  estimated.prob <- pmax(pmin(estimated.prob, 1 - eps), eps)
  - mean(true.bin * log(estimated.prob) + (1 - true.bin) * log(1 - estimated.prob))
  
} # rtemis::logloss


#' Area under the Curve by pairwise concordance
#'
#' Get the Area under the ROC curve to assess classifier performance using pairwise concordance
#'
#' The first level of \code{true.labels} must be the positive class, and high numbers in
#' \code{estimated.score} should correspond to the positive class.
#'
#' @param estimated.score Float, Vector: Probabilities or model scores (e.g. c(.32, .75, .63), etc)
#' @param true.labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param verbose Logical: If TRUE, print messages to output
#' @export


auc_pairs <- function(estimated.score, true.labels, verbose = TRUE) {

  true.labels <- as.factor(true.labels)
  true.levels <- levels(true.labels)
  n.levels <- length(true.levels)
  if (n.levels == 2) {
    outer.diff <- outer(estimated.score[true.labels == true.levels[1]],
                        estimated.score[true.labels == true.levels[2]], "-")
    .auc <- mean((outer.diff > 0) + .5*(outer.diff == 0))
  } else {
    stop("Multiclass AUC does not have a single definition and is not yet implemented")
  }
  if (verbose) {
    msg("Positive class:", true.levels[1])
    msg("AUC = ", .auc)
  }
  invisible(.auc)

} # rtemis::auc_pairs


#' Area under the Curve
#'
#' Get the Area under the ROC curve to assess classifier performance using \code{pROC}
#'
#' Consider looking at Balanced Accuracy and F1 as well
#'
#' @param prob Float, Vector: Probabilities or model scores (e.g. c(.32, .75, .63), etc)
#' @param labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param verbose Logical: If TRUE, print messages to output
#' @export

auc <- function(prob, labels,
                # method = c("pROC", "multiROC"),
                verbose = FALSE) {

  # method <- match.arg(method)
  method <- "pROC"

  if (method == "pROC") {
    if (!depCheck("pROC", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    if (length(levels(labels) > 2)) {
      .roc <- pROC::multiclass.roc(labels, prob)
    } else {
      .roc <- pROC::roc(labels, prob)
    }
  } else {
    if (!depCheck("multiROC", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }

  }
  
  if (verbose) msg("AUC is", .roc$auc)
  as.numeric(.roc$auc)

} # rtemis::auc


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

#' Factor harmonize
#'
#' @param x Input factor
#' @param reference Reference factor
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
# 2016-8 Efstathios D. Gennatas egenn.github.io

#' Print \code{regError} object
#'
#' \code{print} \code{regError} object
#'
#' @method print regError
#' @param x \code{regError} object
#' @author Efstathios D. Gennatas
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
  cat("   R sq = ", ddSci(obj$Rsq), "\n", sep = "")

} # rtemis::print.regError


# print.survError.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Print \code{survError} object
#'
#' \code{print} \code{survError} object
#'
#' @method print survError
#' @param x \code{survError} object
#' @author Efstathios D. Gennatas

print.survError <- function(x, ...) {

  obj <- x
  cat("    Concordance =", ddSci(obj$concordance), "\n")
  cat("             SE =", ddSci(obj$std.err), "\n")
  cat("              N =", ddSci(obj$n), "\n")
  cat("   N concordant =", ddSci(obj$stats[[1]]), "\n")
  cat("   N discordant =", ddSci(obj$stats[[2]]), "\n")
  cat("      Tied Risk =", ddSci(obj$stats[[3]]), "\n")
  cat("      Tied Time =", ddSci(obj$stats[[4]]), "\n")
  cat("      Std (c-d) =", ddSci(obj$stats[[5]]), "\n")
  cat("\n")

} # rtemis::print.survError
