# rsd.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Coefficient of Variation (Relative standard deviation)
#'
#' Calculates the coefficient of variation, also known as relative standard deviation, which is given by
#' \deqn{sd(x)/mean(x)}
#'
#' This is not meaningful if mean is close to 0. For such cases, set `adjust = TRUE`.
#' This will add `min(x)` to x
#' @param x Numeric: Input
#' @param as.percentage Logical: If TRUE, multiply by 100
#' @param na.rm Logical: If TRUE, remove missing values before computation
#' @param adjust Logical: If TRUE, if `x` contains values < `adjust.lo`, x will be shifted up
#'   by adding its minimum
#' @param adjust.lo Float: Threshold to be used if `adjust = TRUE`
#' @examples
#' \dontrun{
#' mplot3_x(sapply(1:100, function(x) cov(rnorm(100))), 'd', xlab = 'rnorm(100) x 100 times')
#' # cov of rnorm without adjustment is all over the place
#' mplot3_x(sapply(1:100, function(x) cov(rnorm(100), adjust = T)), 'd',
#' xlab = 'rnorm(100) x 100 times')
#' # COV after shifting above 1 is what you probably want
#' }
#' @export

rsd <- function(
  x,
  as.percentage = TRUE,
  na.rm = TRUE,
  adjust = FALSE,
  adjust.lo = 1
) {
  if (adjust) {
    if (any(x < adjust.lo)) {
      x <- x - min(x, na.rm = TRUE)
    }
  }

  cov <- sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
  if (as.percentage) cov <- cov * 100
  cov
} # rtemis::rsd
