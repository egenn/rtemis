# reverseLevels.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Reverse factor levels
#'
#' Reverse the order of a factor's levels
#'
#' @param x Factor
#' @export
#' @author E.D. Gennatas

reverseLevels <- function(x) {
  if (!is.factor(x)) {
    x
  } else {
    factor(x, levels = rev(levels(x)))
  }
} # rtemis::reverseLevels
