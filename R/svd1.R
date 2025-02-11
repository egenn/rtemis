# svd1.R
# ::rtemis::
# 2015 EDG rtemis.org

#' `rtemis-internals` Project Variables to First Eigenvector
#'
#' Convenience function for SVD k = 1
#'
#' @param x Input matrix / data frame
#' @param x_testing Optional test matrix / data frame
#'
#' @author EDG
#' @export

svd1 <- function(x, x_testing = NULL) {

  # Arguments ----
  if (missing(x)) {
    print(args(svd1))
    stop("x is missing")
  }

  # SVD ----
  x.svd <- svd(x, nu = 1, nv = 1)

  # Projection ----
  x.proj <- abs(scale(x, center = FALSE) %*% x.svd[["v"]])

  # Test set Projection ----
  x_testing.proj <- NA
  if (!is.null(x_testing)) {
    x_testing.proj <- abs(scale(x_testing, center = FALSE) %*% x.svd[["v"]])
  }

  s.out <- list(u = x.svd[["u"]],
                d = x.svd[["d"]],
                v = x.svd[["v"]])
  s.out$proj  <- x.proj
  if (!is.null(x_testing)) s.out$test_proj <- x_testing.proj

  s.out

} # rtemis::svd1
