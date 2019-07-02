# se.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Extract standard error of fit from rtemis model
#'
#' Returns mod$se.fit
#'
#' @param x An \link{rtMod} object
#' @return Standard error of fitted values of \code{mod}
#' @author Efstathios D. Gennatas
#' @export

se <- function(x) {

  return(as.numeric(x$se.fit))

} # rtemis::se
