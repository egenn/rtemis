# drange.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Set Dynamic Range
#'
#' `rtemis preproc`: Adjusts the dynamic range of a vector or matrix input.
#'   By default normalizes to 0-1 range.
#'
#' @param x Numeric vector or matrix / data frame: Input
#' @param lo Target range minimum. Defaults to 0
#' @param hi Target range maximum. Defaults to 1
#' @param byCol Logical: If TRUE: if `x` is matrix, `drange` each
#' column separately
#'
#' @author E.D. Gennatas
#' @examples
#' x <- runif(20, -10, 10)
#' x <- drange(x)
#' @export

drange <- function(x, lo = 0, hi = 1, byCol = TRUE) {
  dr <- function(x, lo, hi) {
    .min <- min(x, na.rm = TRUE)
    (x - .min) / max(x - .min, na.rm = TRUE) * (hi - lo) + lo
  }

  if (NCOL(x) > 1) {
    if (byCol) {
      new.x <- apply(x, 2, function(x) dr(x, lo, hi))
    } else {
      new.x <- dr(x, lo, hi)
    }
  } else {
    new.x <- dr(x, lo, hi)
  }

  new.x
} # rtemis::drange
