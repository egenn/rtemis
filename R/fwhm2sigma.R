# fwhm2sigma.R
# ::rtemis::
# 2015 E.D. Gennatas rtemis.org

#' FWHM to Sigma
#'
#' Convert Full width at half maximum values to sigma
#'
#' @param fwhm FWHM value
#' @return sigma
#' @author E.D. Gennatas
#' @examples
#' fwhm2sigma(8)
#' # FWHM of 8 is equivalent to sigma = 3.397287
#' @export

fwhm2sigma <- function(fwhm) {
  sigma <- fwhm / (2 * sqrt(2 * log(2)))
  cat("FWHM of", fwhm, "is equivalent to sigma =", sigma, "\n")
  # return only prints if function run without assignment
  sigma
} # rtemis::fwhm2sigma
