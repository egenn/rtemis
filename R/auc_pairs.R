# auc_pairs.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

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
#' @examples
#' true.labels <- factor(c("a", "a", "a", "b", "b", "b", "b"))
#' estimated.score = c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2)
#' auc_pairs(estimated.score, true.labels, verbose = T)
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
    msg("AUC =", .auc)
  }
  invisible(.auc)

} # rtemis::auc_pairs
