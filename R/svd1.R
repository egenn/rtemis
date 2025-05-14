# svd1.R
# ::rtemis::
# 2015 E.D. Gennatas rtemis.org

#' `rtemis-internals` Project Variables to First Eigenvector
#'
#' Convenience function for SVD k = 1
#'
#' @param x Input matrix / data frame
#' @param x.test Optional test matrix / data frame
#'
#' @author E.D. Gennatas
#' @export

svd1 <- function(x, x.test = NULL) {
  # [ Arguments ] ----
  if (missing(x)) {
    print(args(svd1))
    stop("x is missing")
  }

  # [ SVD ] ----
  x.svd <- svd(x, nu = 1, nv = 1)

  # [ PROJECTION ] ----
  x.proj <- abs(scale(x, center = FALSE) %*% x.svd$v)

  # [ TEST PROJECTION ] ----
  x.test.proj <- NA
  if (!is.null(x.test)) {
    x.test.proj <- abs(scale(x.test, center = FALSE) %*% x.svd$v)
  }

  s.out <- list(u = x.svd$u, d = x.svd$d, v = x.svd$v)
  s.out$proj <- x.proj
  if (!is.null(x.test)) s.out$test.proj <- x.test.proj

  s.out
} # rtemis::svd1
