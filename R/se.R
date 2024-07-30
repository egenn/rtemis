# se.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Extract standard error of fit from rtemis model
#'
#' Returns mod$se.fit
#'
#' @param x An `rtMod` object
#' @return Standard error of fitted values of `mod`
#' @author E.D. Gennatas
#' @export

se <- function(x) {
  return(as.numeric(x$se.fit))
} # rtemis::se
