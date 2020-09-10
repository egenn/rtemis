# sparsernorm.R
# ::rtemis::
# Efstathios D. Gennatas egenn.lambdamd.org

#' Sparse rnorm
#'
#' A sparse version of \code{stats::rnorm}
#' Outputs a vector where a fraction of values are zeros (determined by \code{sparseness})
#' and the rest are drawn from a random normal distribution using \code{stats::rnorm}
#'
#' @param n Integer: Length of output vector
#' @param sparseness Float (0, 1): Fraction of required nonzero elements, i.e. output will have
#' \code{round(sparseness * n) nonzero elements}.
#' If \code{sparseness = 0}, a vector of zeros length \code{n} is returned,
#' if \code{sparseness = 1}, \code{rnorm(n, mean, sd)} is returned.
#' Default = 0.1
#' @param mean Float: Target mean of nonzero elements, passed to \code{stats::rnorm} Default = 0
#' @param sd Float: Target sd of nonzero elements, passed to \code{stats::rnorm} Default = 1
#' @author Efstathios D. Gennatas
#' @export

sparsernorm <- function(n,
                        sparseness = .1,
                        mean = 0,
                        sd = 1) {

  if (sparseness > 0 && sparseness < 1) {
    .n <- round(sparseness * n)
    .rnorm <- rnorm(.n, mean = mean, sd = sd)
    out <- rep(0, n)
    index <- sample(n, .n)
    out[index] <- .rnorm
    out
  } else if (sparseness == 0) {
    rep(0, n)
  } else {
    rnorm(n, mean = mean, sd = sd)
  }

} # rtemis::sparsernorm
