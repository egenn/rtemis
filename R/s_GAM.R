# s_GAM.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Generalized Additive Model (GAM) {C, R}
#'
#' Trains a GAM using `mgcv::gam` and validates it.
#' Input will be used to create a formula of the form:
#' \deqn{y = s(x_{1}, k = gam.k) + s(x_{2}, k = gam.k) + ... + s(x_{n}, k = gam.k)}
#'
#' Only `s_GAM.default` is actively maintained at the moment
#' 
#' @inheritParams s_GLM
#' @param ... Additional arguments to be passed to `mgcv::gam`
#' 
#' @return `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_GAM <- function(x, ...) {

  UseMethod("s_GAM")

} # rtemis::s_GAM
