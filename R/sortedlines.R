# sortedlines
# ::rtemis::
# 2020 E.D. Gennatas lambdamd.org

#' lines, but sorted
#'
#' @param x Input vector
#' @param y Input vector
#' @param col Line color. Default = "red"
#' @param ... Extra params to pass to \code{lines}
#' @export
#' @author E.D. Gennatas

sortedlines <- function(x, y, col = "red", ...) {

  index <- order(x)
  lines(x[index], y[index], col = col, ...)

}
