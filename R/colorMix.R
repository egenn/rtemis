# colorMix.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Create an alternating sequence of graded colors
#'
#' @param color List: List of two or more elements, each containing two colors.
#' A gradient will be created from the first to the second color of each element
#' @param n Integer: Number of steps in each gradient.
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' color <- list(blue = c("#82afd3", "#000f3a"),
#'               gray = c("gray10", "gray85"))
#' previewcolor(desaturate(colorMix(color, 6), .3))
#'
#' color <- list(blue = c("#82afd3", "#57000a"),
#'               gray = c("gray10", "gray85"))
#' previewcolor(desaturate(colorMix(color, 6), .3))
#'
#' color <- list(blue = c("#82afd3", "#000f3a"),
#'               purple = c("#23001f", "#c480c1"))
#' previewcolor(desaturate(colorMix(color, 5), .3))

colorMix <- function(color, n = 4) {
  if (class(color)[1] != "list") stop("Please provide list of color pairs")

  color.grad <- lapply(color, function(i) colorRampPalette(i)(n))

  c(t(as.data.frame(color.grad)))
} # rtemis::colorMix
