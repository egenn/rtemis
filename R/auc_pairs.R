# auc_pairs.R
# ::rtemis::
# 2019 E.D. Gennatas www.lambdamd.org

#' Area under the Curve by pairwise concordance
#'
#' Get the Area under the ROC curve to assess classifier performance using pairwise concordance
#'
#' The first level of \code{true.labels} must be the positive class, and high numbers in
#' \code{estimated.score} should correspond to the positive class.
#'
#' @param estimated.score Float, Vector: Probabilities or model scores 
#' (e.g. c(.32, .75, .63), etc)
#' @param true.labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param verbose Logical: If TRUE, print messages to output
#'
#' @examples
#' true.labels <- factor(c("a", "a", "a", "b", "b", "b", "b"))
#' estimated.score <- c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2)
#' auc_pairs(estimated.score, true.labels, verbose = TRUE)
#' @export


auc_pairs <- function(estimated.score, true.labels, verbose = TRUE) {
    true.labels <- as.factor(true.labels)
    true.levels <- levels(true.labels)
    n.levels <- length(true.levels)
    if (n.levels == 2) {
        outer.diff <- outer(
            estimated.score[true.labels == true.levels[1]],
            estimated.score[true.labels == true.levels[2]], "-"
        )
        .auc <- mean((outer.diff > 0) + .5 * (outer.diff == 0))
    } else {
        stop("Multiclass AUC does not have a unique definition and is not yet implemented")
    }
    if (verbose) {
        msg2("Positive class:", true.levels[1])
        msg2("AUC =", .auc)
    }
    invisible(.auc)
} # rtemis::auc_pairs


#' Area under Precision-Recall curve by average precision
# https://towardsdatascience.com/the-wrong-and-right-way-to-approximate-area-under-precision-recall-curve-auprc-8fd9ca409064
#'
# AP=Σ(Rₙ-Rₙ₋₁)Pₙ
# https://towardsdatascience.com/how-to-efficiently-implement-area-under-precision-recall-curve-pr-auc-a85872fd7f14

auPRc <- function(estimated.score, true.labels, thresholds = NULL) {
    true.labels <- as.factor(true.labels)
    true.levels <- levels(true.labels)
    n.classes <- length(true.levels)

    if (is.null(thresholds)) {
        thresholds <- sort(c(-Inf, unique(estimated.score), Inf))
    }

    if (n.classes == 2) {
        predicted.labels <- lapply(thresholds, function(i) {
            # pred <- factor(as.integer(estimated.score >= i), levels = c(1, 0))
            pred <- factor(ifelse(estimated.score >= i, 1, 0), levels = c(1, 0))
            levels(pred) <- true.levels
            pred
        })
        predicted.labels <- as.data.frame(predicted.labels, col.names = paste0("t_", seq(thresholds)))

        precision.t <- sapply(predicted.labels, function(i) precision(true.labels, i))
        sensitivity.t <- sapply(predicted.labels, function(i) sensitivity(true.labels, i))

        # specificity.t <- sapply(predicted.labels, function(i) specificity(true.labels, i))
    } else {
        stop("Multiclass auPRc not yet supported")
    }
    sum()
}

#' Precision of binary classifier
#' 
#' @param estimated Factor: Estimated labels
#' @param true Factor: true labels
#' 
#' @author E.D. Gennatas

# precision <- function(estimated, true) {
#     tbl <- table(estimated, true)
#     Totals <- colSums(tbl)
#     Predicted.totals <- rowSums(tbl)
#     Hits <- diag(tbl)
#     PPV <- Hits / Predicted.totals
# }
