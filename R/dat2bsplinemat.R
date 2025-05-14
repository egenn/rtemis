# dat2bsplinemat.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' B-Spline matrix from dataset
#'
#' Convert a dataset to its b-spline basis set
#'
#' @param x data.frame: Input
#' @param df Integer: Degrees of freedom. See `splines::bSpline`
#' @param knots Float, vector: Internal breakpoints. See `splines::bSpline`
#' @param degree Integer (>0): Degree of the piecewise polynomial. See `splines::bSpline`
#' @param intercept Logical: If TRUE, an intercept is included. Default = FALSE
#' @param Boundary.knots Float, vector (length = 2): Boundary points to anchor the spline basis.
#' @param return.deriv Logical: If TRUE, return list containing a data frame with the splines and another
#' data frame with their derivatives
#' @param as.data.frame Logical: If TRUE, return data.frame, otherwise matrix. Default = TRUE
#' See `splines::bSpline`
#' @return If `return.deriv=F`, a data frame where each original feature is replaced with its basis set or a list,
#' otherwise a list containing a data frame with splines and a data frame with their derivatives
#' @author E.D. Gennatas
#' @export

dat2bsplinemat <- function(
  x,
  df = NULL,
  knots = NULL,
  degree = 3L,
  intercept = FALSE,
  Boundary.knots = range(x, na.rm = TRUE),
  return.deriv = FALSE,
  as.data.frame = TRUE
) {
  nc <- NCOL(x)
  feat.names <- if (!is.null(colnames(x))) colnames(x) else
    paste0("Feat", seq(nc))

  # Splines ----
  Splines <- lapply(seq(nc), function(i) {
    splines2::bSpline(
      x[, i],
      df = df,
      knots = knots,
      degree = degree,
      intercept = intercept,
      Boundary.knots = Boundary.knots
    )
  })

  # Derivatives ----
  if (return.deriv) {
    dx.s <- lapply(seq(nc), function(i) deriv(Splines[[i]]))
    dx.s <- do.call(cbind, dx.s)
    colnames(dx.s) <- unlist(lapply(
      seq(nc),
      function(i) paste0(feat.names[i], "_basis", seq(degree))
    ))
  }

  x.s <- do.call(cbind, Splines)
  colnames(x.s) <- unlist(lapply(
    seq(nc),
    function(i) paste0(feat.names[i], "_basis", seq(degree))
  ))

  if (as.data.frame) {
    x.s <- as.data.frame(x.s)
    if (return.deriv) dx.s <- as.data.frame(dx.s)
  }

  if (return.deriv) {
    out <- list(Splines = x.s, Derivatives = dx.s, SplineObj = Splines)
  } else {
    out <- list(Splines = x.s, SplineObj = Splines)
  }

  class(out) <- c("rtBSplines", "list")
  out
} # rtemis::dat2bsplinemat


#' Predict S3 method for `rtBSplines`
#'
#' @method predict rtBSplines
#' @param object `rtBSplines` object created by [dat2bsplinemat]
#' @param newdata `data.frame` of new data.
#' @param ... Not used.
#'
#' @author E.D. Gennatas
#' @export

predict.rtBSplines <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$Splines)
  } else {
    nsplines <- length(object$SplineObj)
    if (NCOL(newdata) != nsplines)
      stop("N of columns in newdata does not match N of spline objects")
    feat.names <- colnames(newdata)
    if (is.null(feat.names)) feat.names <- paste0("Feature", seq_len(nsplines))
    predicted <- do.call(
      cbind,
      lapply(
        seq_len(nsplines),
        function(i) predict(object$SplineObj[[i]], newdata[, i])
      )
    )
    degree <- attr(object$SplineObj[[1]], "degree")
    colnames(predicted) <- unlist(lapply(
      seq(nsplines),
      function(i) paste0(feat.names[i], "_basis", seq(degree))
    ))
  }

  predicted
} # rtemis::predict.rtBSplines
