# mod_error.R
# ::rtemis::
# 2015-8 E.D. Gennatas rtemis.org

#' Error Metrics for Supervised Learning
#'
#' Calculate error metrics for pair of vector, e.g. true and estimated values from a model
#'
#' In regression, NRMSE = RMSE / range(observed)
#' @param true Vector: True values
#' @param estimated Vector: Estimated values
#' @param estimated.prob Vector: Estimated probabilities for Classification,
#' if available.
#' @param verbosity Integer: If > 0, print messages to console.
#' @param type Character: "Regression", "Classification", or "Survival".
#' If not provided, will be set to Regression if y is numeric.
#' @param rho Logical: If TRUE, calculate Spearman's rho.
#' @param tau Logical: If TRUE, calculate Kendall's tau.
#'   This can be slow for long vectors
#' @param na.rm Logical: Passed to `mean` and `range` functions.
#'
#' @return Object of class `mod_error`
#' @author E.D. Gennatas
#' @export

mod_error <- function(
  true,
  estimated,
  estimated.prob = NULL,
  type = NULL,
  rho = FALSE,
  tau = FALSE,
  na.rm = TRUE,
  verbosity = 0
) {
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
  if (NROW(x) != NROW(y)) {
    stop(
      "Input lengths do not match:\n    Length of true = ",
      NROW(x),
      "\n    Length of estimated = ",
      NROW(y)
    )
  }
  if (NROW(x) < 2) {
    if (verbosity > 0) {
      warning(
        "Vector of length 1; no per-resample test error can be estimated for LOOCV; estimate aggregate error"
      )
    }
    return(NULL)
  }

  if (type == "Regression") {
    reg_error(x, y, rho = rho, tau = tau, na.rm = na.rm, verbosity = verbosity)
  } else if (type == "Classification") {
    if (!is.factor(x)) x <- as.factor(x)
    n.classes <- length(levels(x))
    if (n.classes < 2) stop("Classification requires at least two classes")
    class_error(x, y, estimated.prob)
  } else if (type == "Survival") {
    surv_error(x, y)
  } else {
    stop("Unknown type: ", type)
  }
} # rtemis::mod_error


#' Regression Error Metrics
#'
#' Calculate error metrics for regression
#'
#' @param x Numeric vector: True values
#' @param y Numeric vector: Predicted values
#' @param rho Logical: If TRUE, calculate Spearman's rho
#' @param tau Logical: If TRUE, calculate Kendall's tau
#' @param pct.red Logical: If TRUE, calculate percent reduction in error
#' @param na.rm Logical: If TRUE, remove NA values before computation
#' @param verbosity Integer: If > 0, print messages to console
#'
#' @return Object of class `regError`
#' @author E.D. Gennatas
reg_error <- function(
  x,
  y,
  rho = FALSE,
  tau = FALSE,
  pct.red = FALSE,
  na.rm = FALSE,
  verbosity = 0
) {
  inherits_check(x, "numeric")
  inherits_check(y, "numeric")
  error <- x - y
  mae <- mean(abs(error), na.rm = na.rm)
  mse <- mean(error^2, na.rm = na.rm)
  rmse <- sqrt(mse)
  nrmse <- rmse / diff(range(x, na.rm = na.rm))
  if (length(y) > 2) {
    corr <- suppressWarnings(cor.test(x, y))
    r <- corr$estimate
    r.p <- corr$p.value
  } else {
    r <- r.p <- NA
  }

  # Sum of Squared Errors of prediction (SSE) a.k.a. Residual Sum of Squares (RSS)
  SSE <- sum((x - y)^2, na.rm = na.rm)
  # Sum of Squares due to Regression (SSR) a.k.a. Explained Sum of Squares (ESS)
  SSR <- sum((mean(x, na.rm = na.rm) - y)^2, na.rm = na.rm)
  # Total Sum of Squares (TSS or SST)
  SST <- sum((x - mean(x, na.rm = na.rm))^2, na.rm = na.rm)
  # R-squared a.k.a. Coefficient of Determination i.e. percent variance explained
  Rsq <- 1 - (SSE / SST)

  # Standard Error of the Estimate
  stderr <- sqrt((sum((x - y)^2, na.rm = na.rm)) / length(x))

  # Error of expectation(x) and percent reduction
  if (pct.red) {
    error.exp <- x - mean(x, na.rm = na.rm)
    mae.exp <- mean(abs(error.exp))
    mae.red <- (mae.exp - mae) / mae.exp
    mse.exp <- mean(error.exp^2)
    mse.red <- (mse.exp - mse) / mse.exp # R-squared
    rmse.exp <- sqrt(mse.exp)
    rmse.red <- (rmse.exp - rmse) / rmse.exp
  }

  out <- data.frame(
    MAE = mae,
    MSE = mse,
    RMSE = rmse,
    NRMSE = nrmse,
    r = r,
    r.p = r.p,
    SSE = SSE,
    SSR = SSR,
    SST = SST,
    Rsq = Rsq,
    stderr = stderr,
    row.names = NULL
  )
  if (pct.red) {
    out$MAE.RED <- mae.red
    out$MSE.RED <- mse.red
    out$RMSE.RED <- rmse.red
  }
  if (rho) {
    out$rho <- suppressWarnings(cor(x, y, method = "spearman"))
    out$rho.p <- suppressWarnings(cor.test(x, y, method = "spearman")$p.value)
  }
  if (tau) {
    out$tau <- cor(x, y, method = "kendall")
    out$tau.p <- cor.test(x, y, method = "kendall")$p.value
  }

  if (verbosity > 0) print(out, row.names = FALSE)
  class(out) <- c("regError", "data.frame")
  out
} # rtemis::reg_error

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
  # SSR <- sum((mean(x) - y)^2)
  # Total Sum of Squares (TSS or SST)
  SST <- sum((x - mean(x))^2)
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
  -mean(
    true.bin * log(estimated.prob) + (1 - true.bin) * log(1 - estimated.prob)
  )
} # rtemis::logloss


#' Sensitivity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param estimated Estimated labels
#' @param harmonize Logical: If TRUE, run [factor_harmonize] first
#' @param verbosity Integer: If > 0, print messages to console.
#' @export

sensitivity <- function(true, estimated, harmonize = FALSE, verbosity = 1) {
  if (harmonize)
    estimated <- factor_harmonize(true, estimated, verbosity = verbosity)
  pos.index <- true == levels(true)[1]
  condition.pos <- sum(pos.index)
  true.pos <- sum(true[pos.index] == estimated[pos.index])
  true.pos / condition.pos
}


#' Specificity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param estimated Estimated labels
#' @param harmonize Logical: If TRUE, run [factor_harmonize] first
#' @param verbosity Integer: If > 0, print messages to console.
#' @export

specificity <- function(true, estimated, harmonize = FALSE, verbosity = 1) {
  if (harmonize)
    estimated <- factor_harmonize(true, estimated, verbosity = verbosity)
  neg.index <- true == levels(true)[2]
  condition.neg <- sum(neg.index)
  true.neg <- sum(true[neg.index] == estimated[neg.index])
  true.neg / condition.neg
}


#' Balanced Accuracy
#'
#' Balanced Accuracy of a binary classifier
#'
#' BAcc = .5 * (Sensitivity + Specificity)
#'
#' @param true True labels
#' @param predicted Estimated labels
#' @param harmonize Logical: passed to [sensitivity] and [specificity], which use [factor_harmonize].
#' Default = FALSE
#' @param verbosity Integer: If > 0, print messages to console.
#' @export

bacc <- function(true, predicted, harmonize = FALSE, verbosity = 1) {
  .5 *
    (sensitivity(
      true,
      predicted,
      harmonize = harmonize,
      verbosity = verbosity
    ) +
      specificity(
        true,
        predicted,
        harmonize = harmonize,
        verbosity = verbosity
      ))
}

#' Precision (aka PPV)
#'
#' The first factor level is considered the positive case.
#'
#' @param true Factor: True labels
#' @param estimated Factor: Estimated labels
#' @param harmonize Logical: If TRUE, run [factor_harmonize] first
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @export

precision <- function(true, estimated, harmonize = FALSE, verbosity = 1) {
  if (harmonize) {
    estimated <- factor_harmonize(true, estimated, verbosity = verbosity)
  }
  tbl <- table(estimated, true)
  predicted.totals <- rowSums(tbl)[1]
  hits <- diag(tbl)[1]

  if (hits == 0 && predicted.totals == 0) {
    1
  } else {
    hits / predicted.totals
  }
} # rtemis::precision

#' Factor harmonize
#'
#' @param x Input factor
#' @param reference Reference factor
#' @param verbosity Integer: If > 0, print messages to console.
# #' @param allow.rename Logical: If TRUE, allow renaming - not simply reordering - factor levels of input \code{x}
#' @export

factor_harmonize <- function(reference, x, verbosity = 1) {
  if (!is.factor(x) || !is.factor(reference)) stop("Inputs must be factors")
  if (!all(levels(x) == levels(reference))) {
    if (!all(levels(x) %in% levels(reference))) {
      if (verbosity > 0) msg2("Levels of x:")
      levels(x)
      if (verbosity > 0) msg2("levels of reference:")
      levels(reference)
      stop("Levels of two inputs do not match")
    }
    if (verbosity > 0) {
      msg2("Input factor levels are not in the same order, correcting")
    }
    x <- factor(x, levels = levels(reference))
  }
  x
} # rtemis::factor_harmonize


# print.regError.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org

#' Print `regError` object
#'
#' `print` `regError` object
#'
#' @method print regError
#' @param x `regError` object
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

print.regError <- function(x, ...) {
  obj <- as.data.frame(x)
  if (!is.null(x$MSE.RED)) {
    cat(
      "    MSE = ",
      ddSci(obj$MSE),
      " (",
      ddSci(obj$MSE.RED * 100),
      "%)\n",
      sep = ""
    )
    cat(
      "   RMSE = ",
      ddSci(obj$RMSE),
      " (",
      ddSci(obj$RMSE.RED * 100),
      "%)\n",
      sep = ""
    )
    cat(
      "    MAE = ",
      ddSci(obj$MAE),
      " (",
      ddSci(obj$MAE.RED * 100),
      "%)\n",
      sep = ""
    )
  } else {
    cat("    MSE = ", ddSci(obj$MSE), "\n", sep = "")
    cat("   RMSE = ", ddSci(obj$RMSE), "\n", sep = "")
    cat("    MAE = ", ddSci(obj$MAE), "\n", sep = "")
  }
  cat("      r = ", ddSci(obj$r), " (p = ", ddSci(obj$r.p), ")\n", sep = "")
  if (!is.null(obj$rho)) {
    cat(
      "    rho = ",
      ddSci(obj$rho),
      " (p = ",
      ddSci(obj$rho.p),
      ")\n",
      sep = ""
    )
  }
  cat("   R sq = ", hilite(ddSci(obj$Rsq)), "\n", sep = "")
  invisible(x)
} # rtemis::print.regError
