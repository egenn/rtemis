# colorAdjust.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Adjust HSV Color
#'
#' Modify alpha, hue, saturation and value (HSV) of a color
#'
#' @param color Input color. Any format that grDevices::col2rgb() recognizes
#' @param alpha Numeric: Scale alpha by this amount. Future: replace with absolute setting
#' @param hue Float: How much hue to add to \code{color}
#' @param sat Float: How much saturation to add to \code{color}
#' @param val Float: How much to increase value of \code{color} by
#' @return Adjusted color
#' @author Efstathios D. Gennatas
#' @export

colorAdjust <- function(color,
                        alpha = NULL,
                        hue = 0,
                        sat = 0,
                        val = 0) {

  # [ MAIN ] ====
  ac <- color

  # [ HSV ] ====
  ac.hsv <- grDevices::rgb2hsv(grDevices::col2rgb(ac))
  ac <- grDevices::hsv(ac.hsv[1] + hue, ac.hsv[2] + sat, ac.hsv[3] + val)

  # [ Alpha ] ====
  if (!is.null(alpha)) ac <- adjustcolor(color, alpha.f = alpha)
  ac

} # rtemis::colorAdjust
