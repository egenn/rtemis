# decom.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Matrix Decomposition with \pkg{rtemis}
#'
#' Convenience function to perform any \pkg{rtemis} decomposition
#'
#' `decom` returns an R6 class object `rtDecom`
#'
#' @param x Numeric matrix / data frame: Input data
#' @param decom Character: Decomposer name. See ]link{decomSelect}.
#' @param verbose Logical: if TRUE, print messages to console
#' @param ... Additional arguments to be passed to `decom`
#'
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @export

decom <- function(x,
                  decom = "PCA",
                  verbose = TRUE, ...) {
  if (missing(x)) {
    cat('Usage:\n  decom(x, "nmf", ...)\n\n')
    return(decomSelect())
  }

  args <- c(
    list(
      x = x,
      verbose = verbose
    ),
    list(...)
  )

  decom <- do.call(decomSelect(decom, fn = TRUE), args)

  decom
} # rtemis::decom
