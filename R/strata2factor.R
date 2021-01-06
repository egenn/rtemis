# strata2factor.survfit.R
# ::rtemis::
# 2020 Efstathios D Gennatas lambdamd.org

#' Convert \code{survfit} object's strata to a factor
#'
#' @param x \code{survfit} object
#' @return factor
#' @author Efstathios D. Gennatas
#' @export

strata2factor <- function(x) {

  UseMethod("strata2factor", x)

}

strata2factor.survfit <- function(x) {

  unlist(lapply(seq(x$strata), function(i) rep(names(x$strata)[i], x$strata[i])))

} # rtemis::strata2factor.survfit
