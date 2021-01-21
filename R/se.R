# se.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Extract standard error of fit from rtemis model
#'
#' Returns mod$se.fit
#'
#' @param x An \link{rtMod} object
#' @return Standard error of fitted values of \code{mod}
#' @author E.D. Gennatas
#' @export

se <- function(x) {

  return(as.numeric(x$se.fit))

} # rtemis::se
