# setdiffsym.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Symmetric Set Difference
#'
#' @param x vector
#' @param y vector of same type as `x`
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' setdiff(1:10, 1:5)
#' setdiff(1:5, 1:10)
#' setdiffsym(1:10, 1:5)
#' setdiffsym(1:5, 1:10)
setdiffsym <- function(x, y) {
  union(setdiff(x, y), setdiff(y, x))
} # rtemis::setdiffsym
