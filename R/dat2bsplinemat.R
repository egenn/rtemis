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
#' See \code{splines::bSpline}
#' @return A data.frame where each original feature is replaces with its basis set
#' @author Efstathios D. Gennatas
#' @export

dat2bsplinemat <- function(x,
                           df = NULL,
                           knots = NULL,
                           degree = 3L,
                           intercept = FALSE,
                           Boundary.knots = range(x, na.rm = TRUE)) {

  nc <- NCOL(x)
  x.s <- lapply(seq(nc), function(i) splines2::bSpline(x[, i],
                                                       df = df,
                                                       knots = knots,
                                                       degree = degree,
                                                       intercept = intercept,
                                                       Boundary.knots = Boundary.knots))
  x.s <- as.data.frame(do.call(cbind, x.s))
  k <- NCOL(x.s) / NCOL(x)
  feat.names <- if (!is.null(colnames(x))) colnames(x) else paste0("Feat", seq(nc))
  colnames(x.s) <- unlist(lapply(seq(nc), function(i) paste0(feat.names[i], "_basis", seq(k))))
  x.s

} # rtemis::dat2bsplinemat
