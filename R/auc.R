# auc.R
# ::rtemis::
# 2019-23 E.D. Gennatas rtemis.org

#' Area under the ROC Curve
#'
#' Get the Area under the ROC curve to assess classifier performance.
#'
#' Important Note: We assume that true labels are a factor where the first level
#' is the "positive" case, a.k.a. the event. All methods used here, "pROC",
#' "auc_pairs", "ROCR", have been setup to expect this. This goes against the
#' default setting for both "pROC" and "ROCR", which will not give an AUC less
#' than .5 because they will reorder levels. We don't want this because you
#' can have a classifier perform worse than .5 and it can be very confusing if
#' levels are reordered automatically and different functions give you different
#' AUC.
#'
#' @param preds Numeric, Vector: Probabilities or model scores
#' (e.g. c(.32, .75, .63), etc)
#' @param labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param method Character: "pROC", "auc_pairs", or "ROCR": Method to use.
#' Will use `pROC::roc`, [auc_pairs], `ROCR::performance`, respectively.
#' @param verbose Logical: If TRUE, print messages to output
#' @param trace Integer: If > 0, print more messages to output
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' preds <- c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2)
#' labels <- factor(c("a", "a", "a", "b", "b", "b", "b"))
#' auc(preds, labels, method = "ROCR")
#' auc(preds, labels, method = "pROC")
#' auc(preds, labels, method = "auc_pairs")
#' }
auc <- function(
  preds,
  labels,
  method = c("pROC", "ROCR", "auc_pairs"),
  verbose = FALSE,
  trace = 0
) {
  method <- match.arg(method)
  if (length(unique(labels)) == 1) {
    return(NaN)
  }

  # if (method == "auc_cpp") {
  #   if (is.factor(labels)) {
  #     labels <- as.integer(labels == levels(labels)[rtenv$binclasspos])
  #   }
  #   .auc <- try(auc_cpp(preds, labels))
  if (method == "pROC") {
    dependency_check("pROC")
    .auc <- try(as.numeric(
      pROC::roc(
        labels,
        preds,
        levels = rev(levels(labels)),
        direction = "<"
      )$auc
    ))
  } else if (method == "ROCR") {
    dependency_check("ROCR")
    .pred <- try(ROCR::prediction(
      preds,
      labels,
      label.ordering = rev(levels(labels))
    ))
    .auc <- try(ROCR::performance(.pred, "auc")@y.values[[1]])
  } else if (method == "auc_pairs") {
    .auc <- auc_pairs(preds, labels, verbose = trace > 0)
  }

  if (inherits(.auc, "try-error")) {
    .auc <- NaN
  }

  if (verbose) msg2("AUC =", .auc)
  .auc
} # rtemis::auc
