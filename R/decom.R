# decom.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Matrix Decomposition with \pkg{rtemis}
#'
#' Convenience function to perform any \pkg{rtemis} decomposition
#'
#' \code{decom} returns an R6 class object \code{rtDecom}
#'
#' @param x Numeric matrix / data frame: Input data
#' @param decom String: Decomposition algorithm name, e.g. 'nmf' (case-insensitive)
#' @param x.test Numeric matrix / data frame: Testing set data if supported by \code{decom}
#' @param verbose Logical: if TRUE, print messages to screen
#' @param ... Additional arguments to be passed to \code{decom}
#' @return \link{rtDecom} object
#' @author Efstathios D Gennatas
#' @export

decom <- function(x,
                  decom = "nmf",
                  x.test = NULL,
                  verbose = TRUE, ...) {

  if (missing(x)) {
    cat('Usage:\n  decom(x, "nmf", ...)\n\n')
    return(decomSelect())
  }

  # [ INTRO ] ====
  decomposer <- decomSelect(decom, fn = TRUE)

  # [ DECOMPOSER ] ====
  decom <- decomposer(x = x,
                      x.test = x.test,
                      verbose = verbose, ...)

  # [ OUTRO ] ====
  decom

} # rtemis::decom
