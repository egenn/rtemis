# metrics.R
# ::rtemis::
# 2019- EDG rtemis.org

#' Error functions
#'
#' Convenience functions for calculating loss.
#' These can be passed as arguments to learners that support custom loss functions.
#'
#' @rdname error
#' @param x Vector of True values
#' @param y Vector of predicted values
#' @param na.rm Logical: If TRUE, remove NA values before computation.
#'
#' @author EDG
#' @keywords internal
mae <- function(x, y, na.rm = TRUE) {
  error <- x - y
  mean(abs(error), na.rm = na.rm)
} # rtemis::mae

#' @rdname error
#' @keywords internal
mse <- function(x, y, na.rm = TRUE) {
  error <- x - y
  mean(error^2, na.rm = na.rm)
} # rtemis::mse

#' Weighted MSE
#'
#' @rdname error
#' @param weights Float, vector: Case weights
#' @keywords internal
msew <- function(x, y, weights = rep(1, length(y)), na.rm = TRUE) {
  error <- x - y
  error <- error * weights
  mean(error^2, na.rm = na.rm)
} # rtemis::msew

#' @rdname error
#' @keywords internal
rmse <- function(x, y, na.rm = TRUE) {
  sqrt(mse(x, y, na.rm = na.rm))
} # rtemis::rmse

#' R-squared
#'
#' @param x Float, vector: True values
#' @param y Float, vector: predicted values
#' @author EDG
#' @keywords internal

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
#' @param predicted_prob Float, vector: predicted probabilities
#'
#' @author EDG
#' @keywords internal
logloss <- function(true, predicted_prob) {
  true_bin <- 2 - as.numeric(true)
  eps <- 1e-16
  predicted_prob <- pmax(pmin(predicted_prob, 1 - eps), eps)
  -mean(true_bin * log(predicted_prob) + (1 - true_bin) * log(1 - predicted_prob))
} # rtemis::logloss


#' Sensitivity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param predicted predicted labels
#' @param harmonize Logical: If TRUE, run [factor_harmonize] first
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @keywords internal
sensitivity <- function(true, predicted,
                        harmonize = FALSE,
                        verbosity = 1L) {
  if (harmonize) predicted <- factor_harmonize(true, predicted, verbosity = verbosity)
  pos_index <- true == levels(true)[1]
  condition_pos <- sum(pos_index)
  true_pos <- sum(true[pos_index] == predicted[pos_index])
  true_pos / condition_pos
}

#' Specificity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param predicted predicted labels
#' @param harmonize Logical: If TRUE, run [factor_harmonize] first
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @keywords internal

specificity <- function(true, predicted,
                        harmonize = FALSE,
                        verbosity = 1L) {
  if (harmonize) predicted <- factor_harmonize(true, predicted, verbosity = verbosity)
  neg_index <- true == levels(true)[2]
  condition_neg <- sum(neg_index)
  true_neg <- sum(true[neg_index] == predicted[neg_index])
  true_neg / condition_neg
}

#' Balanced Accuracy
#'
#' Balanced Accuracy of a binary classifier
#'
#' BAcc = .5 * (Sensitivity + Specificity)
#'
#' @param true Factor: True labels.
#' @param predicted Factor: Predicted labels.
#' @param harmonize Logical: passed to [sensitivity] and [specificity], which use [factor_harmonize].
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @keywords internal
bacc <- function(true, predicted,
                 harmonize = FALSE,
                 verbosity = 1L) {
  .5 * (sensitivity(true, predicted, harmonize = harmonize, verbosity = verbosity) +
          specificity(true, predicted, harmonize = harmonize, verbosity = verbosity))
}

#' Precision (aka PPV)
#'
#' The first factor level is considered the positive case.
#'
#' @param true Factor: True labels
#' @param predicted Factor: predicted labels
#' @param harmonize Logical: If TRUE, run [factor_harmonize] first
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @keywords internal

precision <- function(true, predicted,
                      harmonize = FALSE,
                      verbosity = 1L) {
  if (harmonize) {
    predicted <- factor_harmonize(true, predicted, verbosity = verbosity)
  }
  tbl <- table(predicted, true)
  predicted_totals <- rowSums(tbl)[1]
  hits <- diag(tbl)[1]

  if (hits == 0 && predicted_totals == 0) {
    1
  } else {
    hits / predicted_totals
  }
} # rtemis::precision

#' Factor harmonize
#'
#' @param x Input factor.
#' @param reference Reference factor.
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @author EDG
#' @return Factor: x with levels in the same order as reference.
#' @keywords internal

factor_harmonize <- function(reference, x,
                             verbosity = 1L) {
  if (!is.factor(x) || !is.factor(reference)) stop("Inputs must be factors")
  if (!all(levels(x) == levels(reference))) {
    if (!all(levels(x) %in% levels(reference))) {
      if (verbosity > 0L) msg2("Levels of x:")
      levels(x)
      if (verbosity > 0L) msg2("levels of reference:")
      levels(reference)
      stop("Levels of two inputs do not match")
    }
    if (verbosity > 0L) {
      msg2("Input factor levels are not in the same order, correcting")
    }
    x <- factor(x, levels = levels(reference))
  }
  x
} # /rtemis::factor_harmonize

#' F1 score
#'
#' Calculate the F1 score for classification:
#'
#' \deqn{F1 = 2 \frac{Recall \cdot Precision}{Recall + Precision}}{F1 = 2 * (Recall * Precision)/(Recall + Precision)}
#'
#' @param recall Float \[0, 1\]: Recall a.k.a. Sensitivity
#' @param precision Float \[0, 1\]: Precision a.k.a. Positive Predictive Value
#'
#' @author EDG
#' @keywords internal

f1 <- function(precision, recall) {
  2 * (recall * precision) / (recall + precision)
} # /rtemis::f1


#' Brier Score
#'
#' Calculate the Brier Score for classification:
#'
#' \deqn{BS = \frac{1}{N} \sum_{i=1}^{N} (y_i - p_i)^2}{BS = 1/N * sum_{i=1}^{N} (y_i - p_i)^2}
#'
#' @param true Numeric vector, {0, 1}: True labels
#' @param predicted_prob Numeric vector, \[0, 1\]: predicted probabilities
#'
#' @author EDG
#' @keywords internal
brier_score <- function(true, predicted_prob) {
  mean((true - predicted_prob)^2)
} # /rtemis::brier_score

labels2int <- function(x, binclasspos = 2L) {
  stopifnot(is.factor(x))
  # Convert factor to 0, 1 where 1 is the positive class
  # defined by binclasspos
  if (binclasspos == 1L) {
    xi <- 2 - as.numeric(x)
  } else {
    xi <- as.numeric(x) - 1
  }
  return(xi)
} # rtemis::labels2int

# classification_metrics ----
#' Classification Metrics
#'
#' Note that auc_method = "pROC" is the only one that will output an AUC even if
#' one or more predicted probabilities are NA.
#'
#' @param true Factor: True labels.
#' @param predicted Factor: predicted values.
#' @param predicted_prob Numeric vector: predicted probabilities.
#' @param binclasspos Integer: Factor level position of the positive class in binary classification.
#' @param calc_auc Logical: If TRUE, calculate AUC. May be slow in very large datasets.
#' @param calc_brier Logical: If TRUE, calculate Brier Score.
#' @param auc_method Character: "pROC", "ROCR", "auc_pairs": Method to use, passed to
#' [auc].
#' @param verbosity Integer: If > 0, print messages to console.
#'
#' @author EDG
#' @return S3 object of type "class_error"
#' @export
#'
#' @examples
#' \dontrun{
#' true <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
#' predicted <- factor(c("a", "b", "a", "b", "b", "a", "b", "b", "b", "a"))
#' predicted_prob <- c(0.7, 0.45, 0.55, 0.25, 0.43, 0.7, 0.2, .37, .38, .61)
#'
#' class_error(true, predicted, predicted_prob, auc_method = "pROC")
#' class_error(true, predicted, predicted_prob, auc_method = "ROCR")
#' class_error(true, predicted, predicted_prob, auc_method = "auc_pairs")
#' }
classification_metrics <- function(true,
                                   predicted,
                                   predicted_prob = NULL,
                                   binclasspos = 2L,
                                   calc_auc = TRUE,
                                   calc_brier = TRUE,
                                   auc_method = c("pROC", "ROCR", "auc_pairs"),
                                   sample = character(),
                                   verbosity = 0L) {
  # Input ----
  # Binary class probabilities only for now
  if (length(predicted_prob) > length(true)) predicted_prob <- NULL

  # Metrics ----
  if (!all(levels(true) == levels(predicted))) {
    stop(
      "True and predicted must have the same levels.",
      "\nlevels(true): ", paste(levels(true), collapse = ", "),
      "\nlevels(predicted): ", paste(levels(predicted), collapse = ", ")
    )
  }
  # Positive class ----
  # For confusion table, make positive class the first factor level
  if (binclasspos == 2L) {
    true <- factor(true, levels = rev(levels(true)))
    predicted <- factor(predicted, levels = rev(levels(predicted)))
  }
  true_levels <- levels(true)
  n_classes <- length(true_levels)
  Positive_Class <- if (n_classes == 2) true_levels[binclasspos] else NA
  if (verbosity > 0) {
    if (n_classes == 2) {
      msg2("There are two outcome classes:", hilite(paste(true_levels, collapse = ", ")))
      msg2("        The positive class is:", hilite(Positive_Class))
    } else {
      msg2("There are", n_classes, "classes:", true_levels)
    }
  }
  tbl <- table(true, predicted)

  names(attributes(tbl)$dimnames) <- c("Reference", "Predicted")

  Class <- list()
  Overall <- list()
  Class$Totals <- rowSums(tbl)
  Class$Predicted_totals <- colSums(tbl)
  Total <- sum(tbl)
  Class$Hits <- diag(tbl)
  # Class$Misses <- Class$Totals - Class$Hits
  Class$Sensitivity <- Class$Hits / Class$Totals
  Class$Condition.negative <- Total - Class$Totals
  Class$True_negative <- Total - Class$Predicted_totals - (Class$Totals - Class$Hits)
  Class$Specificity <- Class$True_negative / Class$Condition.negative
  Class$Balanced_Accuracy <- .5 * (Class$Sensitivity + Class$Specificity)
  # PPV = true positive / predicted condition positive
  Class$PPV <- Class$Hits / Class$Predicted_totals
  # NPV  = true negative / predicted condition negative
  Class$NPV <- Class$True_negative / (Total - Class$Predicted_totals)
  Class$F1 <- 2 * (Class$PPV * Class$Sensitivity) / (Class$PPV + Class$Sensitivity)

  # Binary vs Multiclass ----
  if (n_classes == 2) {
    Overall$Sensitivity <- Class$Sensitivity[1]
    Overall$Specificity <- Class$Specificity[1]
    Overall$Balanced_Accuracy <- Class$Balanced_Accuracy[1]
    Overall$PPV <- Class$PPV[1]
    Overall$NPV <- Class$NPV[1]
    Overall$F1 <- Class$F1[1]
  } else {
    Overall$Balanced_Accuracy <- mean(Class$`Sensitivity`)
    Overall$`F1 Mean` <- mean(Class$`F1`)
  }
  Overall$Accuracy <- sum(Class$Hits) / Total

  # Prob-based ----
  if (!is.null(predicted_prob) && n_classes == 2) {
    if (calc_auc) {
      Overall$AUC <- auc(preds = predicted_prob, labels = true, method = auc_method)
    }
    if (calc_brier) {
      true_bin <- if (binclasspos == 2L) {
        2 - as.numeric(true)
      } else {
        as.numeric(true) - 1
      }
      Overall$`Brier Score` <- brier_score(true_bin, predicted_prob)
    }
    Overall$`Log loss` <- logloss(true, predicted_prob)
  }

  # Outro ----
  Overall <- as.data.frame(do.call(cbind, Overall))
  rownames(Overall) <- "Overall"
  Class <- (data.frame(
    Sensitivity = Class$Sensitivity,
    Specificity = Class$Specificity,
    Balanced_Accuracy = Class$Balanced_Accuracy,
    PPV = Class$PPV,
    NPV = Class$NPV,
    F1 = Class$F1
  ))

  ClassificationMetrics(
    sample = sample,
    Confusion_Matrix = tbl,
    Overall = Overall,
    Class = Class,
    Positive_Class = Positive_Class
  )
} # /rtemis::classification_metrics


# regression_metrics ----
#' Regression Metrics
#'
#' @param true Numeric vector: True values
#' @param predicted Numeric vector: Predicted values
#' @param na.rm Logical: If TRUE, remove NA values before computation
#'
#' @return RegressionMetrics object
#' @author EDG
#' @export
regression_metrics <- function(true, predicted, na.rm = TRUE, sample = character()) {
  RegressionMetrics(
    MAE = mae(true, predicted, na.rm = na.rm),
    MSE = mse(true, predicted, na.rm = na.rm),
    RMSE = rmse(true, predicted, na.rm = na.rm),
    Rsq = rsq(true, predicted),
    sample = sample
  )
} # /rtemis::regression_metrics
