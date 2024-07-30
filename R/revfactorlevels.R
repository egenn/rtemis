# revfactorlevels.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' Reverse factor level order
#'
#' @param x factor
#'
#' @export
#' @author E.D. Gennatas

revfactorlevels <- function(x) {
  if (is.factor(x)) {
    factor(x, levels = rev(levels(x)))
  } else {
    x
  }
} # rtemis::revfactorlevels
