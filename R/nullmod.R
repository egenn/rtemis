# nullmod.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' \pkg{rtemis} internal: predict for an object of class \code{nullmod}
#'
#' @param object Object of class \code{nullmod}
#' @param newdata Not used
#' @param ... Not used
#' 
#' @method predict nullmod
#' @export

predict.nullmod <- function(object, newdata = NULL, ...) {

  if (!is.null(object$fitted)) object$fitted else 0

} # rtemis::predict.nullmod
