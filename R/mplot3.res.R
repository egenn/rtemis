# mplot3.resample
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' \code{mplot3} Plot \code{resample}
#'
#' Visualizes resampling output using \link{mplot3.img}
#'
#' For resampling with no replacement where each case may be selected 0 or 1 time,
#' 0 is white and 1 is teal For resampling with replacement, 0 is white, 1 is blue, 2 is teal
#'
#' @author Efstathios D. Gennatas
#' @export

mplot3.res <- function(res, col = NULL,
                       theme = "white", ...) {

  ind <- seq(length(unique(unlist(res))))
  resn <- t(sapply(res, function(i) sapply(ind, function(k) sum(k == i))))
  nlevels <- max(resn) * 2 + 1
  if (is.null(col)) col <- colorGrad(nlevels, mid = "white", midhi = ucsfCol$blue, hi = ucsfCol$teal)
  ynames <- names(res)
  mar2 <- max(strwidth(ynames)) + 4
  mplot3.img(resn,
             theme = theme,
             col = col,
             ynames = ynames,
             y.axis.las = 2,
             xlab = "Cases", xlab.line = 0.5,
             mar = c(2, mar2, 1, 1))

} # rtemis::mplot3.res
