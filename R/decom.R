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
#' @param decom String: Decomposer name. See ]link{decomSelect}. Default = "ICA"
#' @param x.test Numeric matrix / data frame: Testing set data if supported by \code{decom}
#' @param verbose Logical: if TRUE, print messages to screen
#' @param ... Additional arguments to be passed to \code{decom}
#' @return \link{rtDecom} object
#' @author Efstathios D Gennatas
#' @export

decom <- function(x,
                  decom = "ICA",
                  x.test = NULL,
                  verbose = TRUE, ...) {

  if (missing(x)) {
    cat('Usage:\n  decom(x, "nmf", ...)\n\n')
    return(decomSelect())
  }

  # [ DECOMPOSER ] ====
  args <- c(list(x = x,
                 x.test = x.test,
                 verbose = verbose),
            list(...))
  decom <- R.utils::doCall(decomSelect(decom, fn = TRUE),
                           args = args)

  # [ OUTRO ] ====
  decom

} # rtemis::decom
