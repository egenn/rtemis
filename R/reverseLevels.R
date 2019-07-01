# reverseLevels.R
# ::rtemis::
# 2019 Efstathios D Gennatas egenn.github.io

#' Reverse factor levels
#'
#' Reverse the order of a factor's levels
#'
#' @param x Factor
#' @export
#' @author Efstathios D Gennatas

reverseLevels <- function(x) {

  if (!is.factor(x)) {
    x
  } else {
    factor(x, levels = rev(levels(x)))
  }

} # rtemis::reverseLevels
