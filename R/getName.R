# getName.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' \pkg{rtemis} internal: Get Variable Name from Arguments
#'
#' Get the name of the variable passed as argument, limit number of characters in case of failure
#'
#' One way to test is to use \link{learn} with x.name = NULL, y.name = NULL
#'
#' @param x Variable whose name you want to extract
#' @param alt Character: If name derived from \code{deparse(substitute(x))} exceeds \code{max.nchar} characters, use this name instead
#' @param max.nchar Integer: Maximum N of characters to allow for name
#' @author E.D. Gennatas
#' @noRd

getName <- function(x, alt = "x", max.nchar = 20) {

  name <- deparse(substitute(x))

  if (nchar(name) > max.nchar) name <- alt

  name

} # rtemis::getName
