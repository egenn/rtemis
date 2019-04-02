# s.GAM.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Generalized Additive Model (GAM) {C, R}
#'
#' Trains a GAM using \code{mgcv::gam} and validates it.
#' Input will be used to create a formula of the form:
#' \deqn{y = s(x_{1}, k = gam.k) + s(x_{2}, k = gam.k) + ... + s(x_{n}, k = gam.k)}
#'
#' Only \code{s.GAM.default} is actively maintained at the moment
#' 
#' @inheritParams s.GLM
#' @param covariates Factors to be included as covariates in model building
#' @param covariates.test Factors to be included as covariates in model validation
#' @param k Integer. Number of bases for smoothing spline
#' @param ... Additional arguments to be passed to \code{mgcv::gam}
#' @return \link{rtMod}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.GAM <- function(x, ...) {

  UseMethod("s.GAM")

} # rtemis::s.GAM
