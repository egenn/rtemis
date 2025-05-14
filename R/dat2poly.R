# dat2poly.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Create n-degree polynomial from data frame
#'
#' This is a convenience function that will take each column of the input and calculate 1:degree powers and concatenate
#' into a data.frame of dimensions `n * (degree * p)` given an `n * p` input
#' @param dat Numeric, matrix / data.frame: Input
#' @param method Character: "simple", "poly". "simple": raise each column of `dat` up to `degree`.
#' "poly": use `stats::poly` with arguments `degree` and `raw`
#' @param degree Integer: degree of polynomials to create. Default = 2
#' @param raw Logical: If TRUE, create simple polynomial, not orthogonalized. Default = FALSE
#' @param as.data.frame Logical: If TRUE, return data.frame. Default = TRUE
#' @author E.D. Gennatas
#' @export
dat2poly <- function(
  dat,
  method = c("simple", "poly"),
  degree = 2,
  raw = FALSE,
  as.data.frame = TRUE
) {
  method <- match.arg(method)
  nc <- NCOL(dat)
  if (NCOL(dat) == 1) dat <- as.data.frame(dat)
  if (is.null(colnames(dat))) colnames(dat) <- paste("V", seq(nc))
  dat <- as.data.frame(dat)
  names <- colnames(dat)

  if (method == "simple") {
    dat.poly <- lapply(seq(nc), function(i) {
      pol <- sapply(seq(degree), function(j) dat[, i]^j)
      colnames(pol) <- paste0(names[i], ".", seq(degree))
      pol
    })
  } else {
    dat.poly <- lapply(seq(nc), function(i) {
      pol <- poly(dat[, i], degree = degree, raw = raw)
      colnames(pol) <- paste0(names[i], ".", seq(degree))
      pol
    })
  }
  out <- do.call(cbind, dat.poly)
  if (as.data.frame) out <- as.data.frame(out)
  out
} # rtemis::dat2poly
