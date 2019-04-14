#' Area under the ROC Curve
#'
#' Get the Area under the ROC curve to assess classifier performance using \code{pROC}
#'
#' Consider looking at Balanced Accuracy and F1 as well
#'
#' Important Note: We always assume that true labels are a factor where the first level is the "positive" case,
#' aka the event. All methods used here, "pROC", "auc_pairs", "ROCR", have been setup to expect this. This goes against
#' the default sertting for both "pROC" and "ROCR", which will not give an AUC less than .5 because they will reorder
#' levels. We don't want this because we believe you CAN make a classifier perform worse than chance (for research or
#' whatnot). It can be very confusing if levels are reordered automatically and different functions give you different
#' AUC. Also, AUC has been very popular, but I strongly recommend reporting Balanced Accuracy instead.
#' @param prob Float, Vector: Probabilities or model scores (e.g. c(.32, .75, .63), etc)
#' @param labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param method String: "pROC", "auc_pairs", or "ROCR": Method to use. Will use \code{pROC::roc}, \link{auc_pairs},
#' \code{ROCR::performance}, respectively. They should all give the same result, they are included for peace of mind.
#' See Details
#' @param verbose Logical: If TRUE, print messages to output
#' @export

auc <- function(prob, labels,
                method = c("auc_pairs", "pROC", "ROCR"),
                verbose = FALSE) {

  method <- match.arg(method)

  if (method == "auc_pairs") {
    .auc <- auc_pairs(prob, labels, verbose = verbose)
  } else if (method == "pROC") {
    if (!depCheck("pROC", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    .auc <- as.numeric(pROC::roc(labels, prob, direction = ">")$auc)
  } else {
    if (!depCheck("ROCR", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    .pred <- ROCR::prediction(prob, labels, label.ordering = rev(levels(labels)))
    .auc <- ROCR::performance(.pred, "auc")@y.values[[1]]
  }

  if (verbose) msg("AUC is", .auc)
  .auc

} # rtemis::auc
