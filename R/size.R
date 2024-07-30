# size.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Size of matrix or vector
#'
#' Return the size of a matrix or vector as (Nrows, Ncolumns)
#' Are you tired of getting NULL when you run dim() on a vector?
#'
#' @param x Vector or matrix input
#' @return Integer vector of length 2: c(Nrow, Ncols)
#' @author E.D. Gennatas
#' @examples
#' x <- rnorm(20)
#' size(x)
#' # 20  1
#'
#' x <- matrix(rnorm(100), 20, 5)
#' size(x)
#' # 20  5
#' @export

size <- function(x) {
  return(c(NROW(x), NCOL(x)))
} # rtemis::size
