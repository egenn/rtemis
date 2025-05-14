# desaturate.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Pastelify a color (make a color more pastel)
#'
#' Lower a color's saturation by a given percent in the HSV color system
#'
#' @param color Color, vector: Color(s) to operate on
#' @param s Float: Decrease saturation by this fraction. Default = .3, which means if saturation of given color is 1,
#' it will become .7
#' @return List of adjusted colors
#' @author E.D. Gennatas
#' @export
#' @examples
#' color <- c("red", "green", "blue")
#' color.p <- desaturate(color)

desaturate <- function(color, s = .3) {
  # Infer color names, if available
  if (is.character(color)) {
    .names <- color
  } else if (!is.null(names(color))) {
    .names <- names(color)
  } else {
    .names <- NULL
  }

  x <- as.list(color)
  x <- lapply(x, col2rgb)
  x <- lapply(x, rgb2hsv)
  xp <- lapply(x, function(i) {
    .s <- i[2]
    i[2] <- .s - (.s * s)
    hsv(i[1], i[2], i[3])
  })

  names(xp) <- .names
  xp
} # rtemis::desaturate
