# winsorize.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.lambdamd.org

#' Winsorize vector
#'
#' Replace extreme values by absolute or quantile threshold
#'
#' If both lo and prob.lo or both hi and prob.hi are NULL, cut-off is set to min(x) and max(x) respectively, i.e.
#' no values are changed
#'
#' @param lo Numeric: If not NULL, replace any values in \code{x} lower than this with this. Default = NULL
#' @param hi Numeric: If not NULL, replace any values in \code{x} higher than this with this. Default = NULL
#' @param prob.lo Numeric (0, 1): If not NULL and \code{lo = NULL}, find sample quantile that corresponds to this
#' probability and set as \code{lo}. Default = .025
#' @param prob.hi Numeric (0, 1): If not NULL and \code{hi = NULL}, find sample quantile that corresponds to this
#' probability and set as \code{hi}. Default = .975
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @examples
#' # Winsorize a normally distributed variable
#' x <- rnorm(500)
#' xw <- winsorize(x)
#' # Winsorize an exponentially distributed variable only on the top 5% highest values
#' x <- rexp(500)
#' xw <- winsorize(x, prob.lo = NULL, prob.hi = .95)
#' @author Efstathios D. Gennatas
#' @export

winsorize <- function(x, lo = NULL, hi = NULL,
                      prob.lo = .025, prob.hi = .975,
                      quantile.type = 7,
                      verbose = TRUE) {

  lo.cut <- if (!is.null(lo)) {
    lo
  } else if (!is.null(prob.lo)) {
    as.numeric(quantile(x, prob.lo, type = quantile.type))
  } else {
    min(x)
  }
  if (verbose) msg("Lo cut set to", lo.cut)

  hi.cut <- if (!is.null(hi)) {
    hi
  } else if (!is.null(prob.hi)) {
    as.numeric(quantile(x, prob.hi, type = quantile.type))
  } else {
    max(x)
  }
  if (verbose) msg("Hi cut set to", hi.cut)

  xw <- ifelse(x < lo.cut, lo.cut, x)
  xw <- ifelse(xw > hi.cut, hi.cut, xw)

  xw

} # rtemis::winsorize
