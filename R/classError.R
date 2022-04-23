# classError.R
# :: rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Classification Error
#'
#' Calculates Classification Metrics
#'
#' @param true Factor vector: True values
#' @param estimated Factor vector: Estimated values
#' @param estimated.prob Numeric vector: Estimated probabilities
#' @param trace Integer: If > 0, print  diagnostic messages. Default = 0
#' 
#' @author E.D. Gennatas
#' @return S3 object of type  "classError"
#' @export

classError <- function(true, 
                       estimated,
                       estimated.prob = NULL,
                       trace = 0) {

  # Input ----
  # Binary class probabilities for now only
  if (length(estimated.prob) > length(true)) estimated.prob <- NULL

  # Metrics ----
  if (!all(levels(true) == levels(estimated))) stop("True and Estimated must have the same levels")
  true.levels <- levels(true)
  n.classes <- length(true.levels)
  Positive.class <- if (n.classes == 2) true.levels[1] else NA
  if (trace > 0) {
    if (n.classes == 2) {
      msg("There are two outcome classes:", rtHighlight$bold(paste(true.levels, collapse = ", ")))
      msg("        The positive class is:", rtHighlight$bold(Positive.class))
    } else {
      msg("There are", n.classes, "classes:", true.levels)
    }
  }
  tbl <- table(estimated, true)

  names(attributes(tbl)$dimnames) <- c("Estimated", "Reference")

  Class <- list()
  Overall <- list()
  Class$Totals <- colSums(tbl)
  Class$Predicted.totals <- rowSums(tbl)
  Total <- sum(tbl)
  Class$Hits <- diag(tbl)
  Class$Misses <- Class$Totals - Class$Hits
  Class$Sensitivity <- Class$Hits/Class$Totals
  # attr(Class$Sensitivity, "Formula") <- "Class$Hits/Class$Totals"
  Class$Condition.negative <- Total - Class$Totals
  Class$True.negative <- Total - Class$Predicted.totals - (Class$Totals - Class$Hits)
  Class$Specificity <- Class$True.negative / Class$Condition.negative
  # attr(Class$Specificity, "Formula") <- "Class$True.negative / Class$Condition.negative"
  Class$`Balanced Accuracy` <- .5*(Class$Sensitivity + Class$Specificity)
  # attr(Class$`Balanced Accuracy`, "Formula") <- ".5*(Class$Sensitivity + Class$Specificity)"
  # PPV = true positive / predicted condition positive
  Class$PPV <- Class$Hits/Class$Predicted.totals
  # attr(Class$PPV, "Formula") <- "Class$Hits/Class$Predicted.totals"
  # NPV  = true negative / predicted condition negative
  Class$NPV <- Class$True.negative/(Total - Class$Predicted.totals)
  # attr(Class$NPV, "Formula") <- "Class$True.negative/(Total - Class$Predicted.totals)"
  Class$F1 <- 2 * (Class$PPV * Class$Sensitivity) / (Class$PPV + Class$Sensitivity)
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
  Overall$Accuracy <- sum(Class$Hits)/Total
  # attr(Overall$Accuracy, "Formula") <- "sum(Class$Hits)/Total"

  # Prob-based ----
  if (!is.null(estimated.prob) & n.classes == 2) {
    Overall$AUC <- auc(estimated.prob, true)
    Overall$`Log loss` <- logloss(true, estimated.prob)
  }

  # Outro ----
  # Overall <- as.data.frame(do.call(rbind, Overall))
  Overall <- as.data.frame(do.call(cbind, Overall))
  rownames(Overall) <- "Overall"
  Class <- (data.frame(Sensitivity = Class$Sensitivity,
                       Specificity = Class$Specificity,
                       `Balanced Accuracy` = Class$`Balanced Accuracy`,
                       PPV = Class$PPV,
                       NPV = Class$NPV,
                       F1 = Class$F1))
  metrics <- list(ConfusionMatrix = tbl,
                  Overall = Overall,
                  Class = Class,
                  Positive.class = Positive.class)
  class(metrics) <- c("classError", "list")
  metrics

} # rtemis::classError


#' Print \link{classError}
#'
#' @param x Object of type \link{classError}
#' @param ... Not used
#' @author E.D. Gennatas
#' @export

print.classError <- function(x, decimal.places = 4, ...) {

  x$Overall$`Log loss` <- NULL
  tblpad <- 17 - max(nchar(colnames(x$ConfusionMatrix)), 9)
  printtable(x$ConfusionMatrix, pad = tblpad)
  printdf(x$Overall,
          transpose = TRUE,
          ddSci.dp = decimal.places,
          justify = "left",
          newline.pre = TRUE,
          newline = TRUE,
          spacing = 2,
          row.col = reset)
  if (is.na(x$Positive.class)) {
    printdf(x$Class,
            transpose = TRUE,
            ddSci.dp = decimal.places,
            justify = "left",
            spacing = 2,
            row.col = reset)
  } else {
    cat("  Positive Class: ", rtHighlight$bold(x$Positive.class), "\n")
  }

} # rtemis::print.classError

# f1.R
# ::rtemis::
# 2019 E.D. Gennatas

#' F1 score
#'
#' Calculate the F1 score for classification:
#'
#' \deqn{F1 = 2 \frac{Recall \cdot Precision}{Recall + Precision}}{F1 = 2 * (Recall * Precision)/(Recall + Precision)}
#'
#' @param recall Float [0, 1]: Recall a.k.a. Sensitivity
#' @param precision Float [0, 1]: Precision a.k.a. Positive Predictive Value
#' 
#' @author E.D. Gennatas
#' @export

f1 <- function(precision, recall) {
  
  2 * (recall * precision) / (recall + precision)
  
} # rtemis::f1
