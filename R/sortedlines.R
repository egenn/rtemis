# sortedlines
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io

#' lines, but sorted
#'
#' @param x Input vector
#' @param y Input vector
#' @param col Line color. Default = "red"
#' @param ... Extra params to pass to \code{lines}
#' @export
#' @author Efstathios D. Gennatas

sortedlines <- function(x, y, col = "red", ...) {

  index <- order(x)
  lines(x[index], y[index], col = col, ...)

}
