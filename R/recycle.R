# recycle.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Recycle values of vector to match length of target
#'
#' @param x Vector to be recycled
#' @param target Object whose length defines target length
#'
#' @author E.D. Gennatas
#' @export

recycle <- function(x, target) {
  lenx <- length(x)
  lent <- length(target)

  if (lenx >= lent) {
    x
  } else {
    rep(x, ceiling(lent / lenx))[seq(lent)]
  }
} # rtemis::recycle
