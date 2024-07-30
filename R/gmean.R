# gmean.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' Geometric mean
#'
#' @param x Numeric vector
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' x <- c(1, 3, 5)
#' mean(x)
#' gmean(x)
#' # same as, but a little faster than:
#' exp(mean(log(x)))
gmean <- function(x) {
  prod(x)^(1 / length(x))
} # rtemis::gmean
