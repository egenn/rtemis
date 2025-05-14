# strata2factor.survfit.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Convert `survfit` object's strata to a factor
#'
#' @param x `survfit` object
#' @return factor
#' @author E.D. Gennatas
#' @export

strata2factor <- function(x) {
  UseMethod("strata2factor", x)
}


#' @export

strata2factor.survfit <- function(x) {
  unlist(lapply(
    seq(x$strata),
    function(i) rep(names(x$strata)[i], x$strata[i])
  ))
} # rtemis::strata2factor.survfit
