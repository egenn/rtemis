# f1.R
# ::rtemis::
# 2019 Efstathios D. Gennatas

#' F1 score
#'
#' Calculate the F1 score for classification:
#'
#' \deqn{F1 = 2 \frac{Recall \cdot Precision}{Recall + Precision}}{F1 = 2 * (Recall * Precision)/(Recall + Precision)}
#'
#' @param recall Float [0, 1]: The model Recall aka Sensitivity
#' @param precision Float [0, 1]: The model Precision aka Positive Predictive Value
#' @author Efstathios D. Gennatas
#' @export

f1 <- function(precision, recall) {

  2 * (recall * precision) / (recall + precision)

} # rtemis::f1
