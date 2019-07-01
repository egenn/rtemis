# colorGrad.x
# ::rtemis::
# 2019 Efstathios D Gennatas egenn.github.io

#' Color gradient for continuous variable
#'
#' @param x FLoat, vector
#' @param color Color, vector, length 2
#' @author Efstathios D Gennatas
#' @export

colorGrad.x <- function(x, color = c("gray20", "#18A3AC")) {

  colors <- rep(color[1], length(x))
  bipolar <- min(x) < 0 & 0 < max(x)

  if (bipolar) {
    maxabsx <- max(abs(x))
    grad <- colorRampPalette(c(color[2], color[1], color[2]))(201)
    cuts <- cut(c(-maxabsx, x, maxabsx), 201, labels = FALSE)[-c(1, length(x) + 2)]
    neg.index <- which(x < 0)
    colors[neg.index] <- grad[cuts[neg.index]]
    colors[-neg.index] <- grad[cuts[-neg.index]]
  } else {
    grad <- colorRampPalette(color)(101)
    cuts <- cut(x, 101, labels = FALSE)
    colors <- grad[cuts]
  }

  colors

} # rtemis::colorGrad.x
