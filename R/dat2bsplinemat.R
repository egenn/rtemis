# dat2bsplinemat.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

#' B-Spline matrix from dataset
#'
#' Convert a dataset to its b-spline basis set
#'
#' @param x data.frame: Input
#' @param df Integer: Degrees of freedom. See \code{splines::bSpline}
#' @param knots Float, vector: Internal breakpoints. See \code{splines::bSpline}
#' @param degree Integer (>0): Degree of the piecewise polynomial. See \code{splines::bSpline}
#' @param intercept Logical: If TRUE, an intercept is included. Default = FALSE
#' @param Boundary.knots Float, vector (length = 2): Boundary points to anchor the spline basis.
#' @param return.deriv Logical: If TRUE, return list containing a data frame with the splines and another
#' data frame with their derivatives
#' @param as.data.frame Logical: If TRUE, return data.frame, otherwise matrix. Default = TRUE
#' See \code{splines::bSpline}
#' @return If \code{return.deriv=F}, a data frame where each original feature is replaced with its basis set or a list,
#' otherwise a list containing a data frame with splines and a data frame with their derivatives
#' @author Efstathios D. Gennatas
#' @export

dat2bsplinemat <- function(x,
                           df = NULL,
                           knots = NULL,
                           degree = 3L,
                           intercept = FALSE,
                           Boundary.knots = range(x, na.rm = TRUE),
                           return.deriv = FALSE,
                           as.data.frame = TRUE) {

  nc <- NCOL(x)
  feat.names <- if (!is.null(colnames(x))) colnames(x) else paste0("Feat", seq(nc))

  # Splines ====
  x.s <- lapply(seq(nc), function(i) splines2::bSpline(x[, i],
                                                       df = df,
                                                       knots = knots,
                                                       degree = degree,
                                                       intercept = intercept,
                                                       Boundary.knots = Boundary.knots))

  # Derivatives ====
  if (return.deriv) {
    dx.s <- lapply(seq(nc), function(i) deriv(x.s[[i]]))
    dx.s <- do.call(cbind, dx.s)
    colnames(dx.s) <- unlist(lapply(seq(nc), function(i) paste0(feat.names[i], "_basis", seq(degree))))
  }

  x.s <- do.call(cbind, x.s)
  colnames(x.s) <- unlist(lapply(seq(nc), function(i) paste0(feat.names[i], "_basis", seq(degree))))

  if (as.data.frame) {
    x.s <- as.data.frame(x.s)
    if (return.deriv) dx.s <- as.data.frame(dx.s)
  }

  if (return.deriv) {
    return(list(Splines = x.s, Derivatives = dx.s))
  } else {
    return(x.s)
  }

} # rtemis::dat2bsplinemat
