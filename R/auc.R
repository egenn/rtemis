# auc.R
# ::rtemis::
# 2019 E.D. Gennatas www.lambdamd.org

#' Area under the ROC Curve
#'
#' Get the Area under the ROC curve to assess classifier performance using \code{pROC}
#'
#' Consider looking at Balanced Accuracy and F1 as well
#'
#' Important Note: We assume that true labels are a factor where the first level
#' is the "positive" case, a.k.a. the event. All methods used here, "pROC",
#' "auc_pairs", "ROCR", have been setup to expect this. This goes against the
#' default sertting for both "pROC" and "ROCR", which will not give an AUC less
#' than .5 because they will reorder levels. We don't want this because you
#' can have a classifier perform worse than .5 and tt can be very confusing if
#' levels are reordered automatically and different functions give you different
#' AUC.
#'
#' @param preds Numeric, Vector: Probabilities or model scores
#' (e.g. c(.32, .75, .63), etc)
#' @param labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param method Character: "pROC", "auc_pairs", or "ROCR": Method to use.
#' Will use \code{pROC::roc},
#' \link{auc_pairs},
#' \code{ROCR::performance}, respectively. They should all give the same result,
#' they are included for testing.
#' @param verbose Logical: If TRUE, print messages to output
#'
#' @examples
#' \dontrun{
#' preds <- c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2)
#' labels <- factor(c("a", "a", "a", "b", "b", "b", "b"))
#' auc(preds, labels)
#' auc(preds, labels, method = "pROC")
#' auc(preds, labels, method = "ROCR")
#' auc(preds, labels, method = "Lpp_auc")
#' }
#' @export

auc <- function(preds, labels,
                method = c("ROCR", "pROC", "Lpp_auc", "auc_pairs"),
                verbose = FALSE,
                trace = 0) {
    method <- match.arg(method)

    if (method == "auc_pairs") {
        .auc <- auc_pairs(preds, labels, verbose = trace > 0)
    } else if (method == "pROC") {
        dependency_check("pROC")
        # maybe define cases and controls here as well
        .auc <- as.numeric(pROC::roc(
            labels, preds,
            levels = rev(levels(labels)),
            direction = "<"
        )$auc)
    } else if (method == "ROCR") {
        dependency_check("ROCR")
        .pred <- ROCR::prediction(preds, labels, label.ordering = rev(levels(labels)))
        .auc <- ROCR::performance(.pred, "auc")@y.values[[1]]
    } else if (method == "Lpp_auc") {
        idi <- order(preds, decreasing = TRUE)
        .auc <- Lpp_ROC(preds[idi], 2 - as.integer(labels[idi]))
    }

    if (verbose) msg2("AUC =", .auc)
    .auc
} # rtemis::auc


# from: https://github.com/Laurae2/R_benchmarking#area-under-the-curve-roc-benchmarks
# different results with small N than other methods
Rcpp::cppFunction("double Lpp_ROC(NumericVector preds, NumericVector labels) {
  double LabelSize = labels.size();
  NumericVector ranked(LabelSize);
  NumericVector positives = preds[labels == 1];
  double n1 = positives.size();
  Range positives_seq = seq(0, n1 - 1);
  ranked[seq(0, n1 - 1)] = positives;
  double n2 = LabelSize - n1;
  NumericVector negatives = preds[labels == 0];
  NumericVector x2(n2);
  ranked[seq(n1, n1 + n2)] = negatives;
  ranked = match(ranked, clone(ranked).sort());
  double AUC = (sum(ranked[positives_seq]) - n1 * (n1 + 1)/2)/(n1 * n2);
  return AUC;
}")
