# f1.R
# ::rtemis::
# 2019 Efstathios D. Gennatas

#' F1 score
#'
#' Calculate the F1 score for classification:
#'
#' \deqn{F1 = 2 \frac{Sensitivity \cdot PPV}{Sensitivity + PPV}}{F1 = 2 * (Sensitivity * Specificity)/(Sensitivity + Specificity)}
#'
#' @param sensitivity Float [0, 1]: The model sensitivity
#' @param ppv Float [0, 1]: The model Positive Predictive Value
#' @author Efstathios D. Gennatas
#' @export

f1 <- function(sensitivity, ppv) {

  2 * (sensitivity * ppv) / (sensitivity + ppv)

} # rtemis::f1
