# class_error.R
# :: rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Classification Error
#'
#' Calculates Classification Metrics
#'
#' Note that auc.method = "pROC" is the only one that will output an AUC even if
#' one or more estimated probabilities are NA.
#'
#' @param true Factor: True labels
#' @param estimated Factor: Estimated values
#' @param estimated.prob Numeric vector: Estimated probabilities
#' @param calc.auc Logical: If TRUE, calculate AUC. May be slow in very large datasets.
#' @param calc.brier Logical: If TRUE, calculate Brier Score
#' @param auc.method Character: "pROC", "ROCR", "auc_pairs": Method to use, passed to
#' [auc].
#' @param trace Integer: If > 0, print  diagnostic messages. Default = 0
#'
#' @author E.D. Gennatas
#' @return S3 object of type "class_error"
#' @export
#'
#' @examples
#' \dontrun{
#' true <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
#' estimated <- factor(c("a", "a", "b", "b", "a", "a", "b", "b", "a", "a"))
#' estimated.prob <- c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2, .37, .57, .61)
#'
#' class_error(true, estimated, estimated.prob, auc.method = "pROC")
#' class_error(true, estimated, estimated.prob, auc.method = "ROCR")
#' class_error(true, estimated, estimated.prob, auc.method = "auc_pairs")
#' }
class_error <- function(
  true,
  estimated,
  estimated.prob = NULL,
  calc.auc = TRUE,
  calc.brier = TRUE,
  auc.method = c("pROC", "ROCR", "auc_pairs"),
  trace = 0
) {
  # Input ----
  # Binary class probabilities only (for now)
  if (length(estimated.prob) > length(true)) estimated.prob <- NULL

  # Metrics ----
  if (!all(levels(true) == levels(estimated))) {
    stop("True and Estimated must have the same levels")
  }
  true.levels <- levels(true)
  n.classes <- length(true.levels)
  Positive.class <- if (n.classes == 2) true.levels[1] else NA
  if (trace > 0) {
    if (n.classes == 2) {
      msg2(
        "There are two outcome classes:",
        hilite(paste(true.levels, collapse = ", "))
      )
      msg2("        The positive class is:", hilite(Positive.class))
    } else {
      msg2("There are", n.classes, "classes:", true.levels)
    }
  }
  tbl <- table(true, estimated)

  names(attributes(tbl)$dimnames) <- c("Reference", "Estimated")

  Class <- list()
  Overall <- list()
  Class$Totals <- rowSums(tbl)
  Class$Predicted.totals <- colSums(tbl)
  Total <- sum(tbl)
  Class$Hits <- diag(tbl)
  # Class$Misses <- Class$Totals - Class$Hits
  Class$Sensitivity <- Class$Hits / Class$Totals
  # attr(Class$Sensitivity, "Formula") <- "Class$Hits/Class$Totals"
  Class$Condition.negative <- Total - Class$Totals
  Class$True.negative <- Total -
    Class$Predicted.totals -
    (Class$Totals - Class$Hits)
  Class$Specificity <- Class$True.negative / Class$Condition.negative
  # attr(Class$Specificity, "Formula") <- "Class$True.negative / Class$Condition.negative"
  Class$`Balanced Accuracy` <- .5 * (Class$Sensitivity + Class$Specificity)
  # attr(Class$`Balanced Accuracy`, "Formula") <- ".5*(Class$Sensitivity + Class$Specificity)"
  # PPV = true positive / predicted condition positive
  Class$PPV <- Class$Hits / Class$Predicted.totals
  # attr(Class$PPV, "Formula") <- "Class$Hits/Class$Predicted.totals"
  # NPV  = true negative / predicted condition negative
  Class$NPV <- Class$True.negative / (Total - Class$Predicted.totals)
  # attr(Class$NPV, "Formula") <- "Class$True.negative/(Total - Class$Predicted.totals)"
  Class$F1 <- 2 *
    (Class$PPV * Class$Sensitivity) /
    (Class$PPV + Class$Sensitivity)
  # attr(Class$F1, "Formula") <- "2 * (Class$PPV * Class$Sensitivity) / (Class$PPV + Class$Sensitivity)"

  # Binary vs Multiclass ----
  if (n.classes == 2) {
    Overall$Sensitivity <- Class$Sensitivity[1]
    Overall$Specificity <- Class$Specificity[1]
    Overall$`Balanced Accuracy` <- Class$`Balanced Accuracy`[1]
    Overall$PPV <- Class$PPV[1]
    Overall$NPV <- Class$NPV[1]
    Overall$F1 <- Class$F1[1]
  } else {
    # Overall$`Balanced Accuracy Mean` <- mean(Class$`Balanced Accuracy`)
    # attr(Overall$`Balanced Accuracy Mean`, "Formula") <- "mean(Class$`Balanced Accuracy`)"
    Overall$`Balanced Accuracy` <- mean(Class$`Sensitivity`)
    # attr(Overall$`Balanced Accuracy`, "Formula") <- "mean(Class$`Sensitivity`)"
    Overall$`F1 Mean` <- mean(Class$`F1`)
    # attr(Overall$`F1 Mean`, "Formula") <- "mean(Class$F1)"
  }
  Overall$Accuracy <- sum(Class$Hits) / Total
  # attr(Overall$Accuracy, "Formula") <- "sum(Class$Hits)/Total"

  # Prob-based ----
  if (!is.null(estimated.prob) && n.classes == 2) {
    if (calc.auc) {
      Overall$AUC <- auc(
        preds = estimated.prob,
        labels = true,
        method = auc.method
      )
    }
    if (calc.brier) {
      true_bin <- if (rtenv$binclasspos == 1) {
        2 - as.numeric(true)
      } else {
        as.numeric(true) - 1
      }
      Overall$`Brier Score` <- brier_score(true_bin, estimated.prob)
    }
    Overall$`Log loss` <- logloss(true, estimated.prob)
  }

  # Outro ----
  # Overall <- as.data.frame(do.call(rbind, Overall))
  Overall <- as.data.frame(do.call(cbind, Overall))
  rownames(Overall) <- "Overall"
  Class <- (data.frame(
    Sensitivity = Class$Sensitivity,
    Specificity = Class$Specificity,
    `Balanced Accuracy` = Class$`Balanced Accuracy`,
    PPV = Class$PPV,
    NPV = Class$NPV,
    F1 = Class$F1
  ))
  metrics <- list(
    ConfusionMatrix = tbl,
    Overall = Overall,
    Class = Class,
    Positive.class = Positive.class
  )
  class(metrics) <- c("class_error", "list")
  metrics
} # rtemis::class_error


#' Print [class_error]
#'
#' @param x Object of type [class_error]
#' @param decimal.places Integer: Number of decimal places to print
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

print.class_error <- function(x, decimal.places = 4, ...) {
  x$Overall$`Log loss` <- NULL
  tblpad <- 17 - max(nchar(colnames(x$ConfusionMatrix)), 9)
  printtable(x$ConfusionMatrix, pad = tblpad)
  printdf(
    x$Overall,
    transpose = TRUE,
    ddSci.dp = decimal.places,
    justify = "left",
    newline.pre = TRUE,
    newline = TRUE,
    spacing = 2,
    row.col = reset
  )
  if (is.na(x$Positive.class)) {
    printdf(
      x$Class,
      transpose = TRUE,
      ddSci.dp = decimal.places,
      justify = "left",
      spacing = 2,
      row.col = reset
    )
  } else {
    cat("  Positive Class: ", hilite(x$Positive.class), "\n")
  }
  invisible(x)
} # rtemis::print.class_error


# f1.R
# ::rtemis::
# 2019 E.D. Gennatas

#' F1 score
#'
#' Calculate the F1 score for classification:
#'
#' \deqn{F1 = 2 \frac{Recall \cdot Precision}{Recall + Precision}}{F1 = 2 * (Recall * Precision)/(Recall + Precision)}
#'
#' @param recall Float \[0, 1\]: Recall a.k.a. Sensitivity
#' @param precision Float \[0, 1\]: Precision a.k.a. Positive Predictive Value
#'
#' @author E.D. Gennatas
#' @export

f1 <- function(precision, recall) {
  2 * (recall * precision) / (recall + precision)
} # rtemis::f1


#' Brier Score
#'
#' Calculate the Brier Score for classification:
#'
#' \deqn{BS = \frac{1}{N} \sum_{i=1}^{N} (y_i - p_i)^2}{BS = 1/N * sum_{i=1}^{N} (y_i - p_i)^2}
#'
#' @param true Numeric vector, {0, 1}: True labels
#' @param estimated.prob Numeric vector, \[0, 1\]: Estimated probabilities
#'
#' @author E.D. Gennatas
#' @export
brier_score <- function(true, estimated.prob) {
  mean((true - estimated.prob)^2)
} # rtemis::brier_score

labels2int <- function(x, pos.class = NULL) {
  if (is.null(pos.class)) {
    pos.class <- rtenv$binclasspos
  }
  stopifnot(is.factor(x))
  # Convert factor to 0, 1 where 1 is the positive class
  # defined by pos.class
  if (pos.class == 1) {
    xi <- 2 - as.numeric(x)
  } else {
    xi <- as.numeric(x) - 1
  }
  return(xi)
} # rtemis::labels2int
