# colorgradient.x
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Color gradient for continuous variable
#'
#' @param x Float, vector
#' @param symmetric Logical: If TRUE, make symmetric gradient between
#' `-max(abs(x))` and `max(abs(x))`
#' @param lo.col Low color
#' @param mid.col Middle color
#' @param hi.col High color
#' @param space Character: "rgb" or "Lab". Default = "Lab"
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- seq(-10, 10, length.out = 51)
#' previewcolor(colorgradient.x(x))
#' x <- sort(rnorm(40))
#' previewcolor(colorgradient.x(x, mid.col = "white"))
#' # Notice how most values are near zero therefore almost white
#' }

colorgradient.x <- function(
  x,
  symmetric = FALSE,
  lo.col = "#0290EE",
  mid.col = "#1A1A1A",
  hi.col = "#FFBD4F",
  space = "Lab"
) {
  grad <- colorRampPalette(c(lo.col, mid.col, hi.col), space = space)(201)

  if (symmetric) {
    maxabsx <- max(abs(x))
    cuts <- cut(c(-maxabsx, x, maxabsx), 201, labels = FALSE)[
      -c(1, length(x) + 2)
    ]
  } else {
    cuts <- cut(x, 201, labels = FALSE)
  }

  grad[cuts]
} # rtemis::colorgradient.x
