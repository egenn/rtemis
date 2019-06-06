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
    # n.neg <- sum(x < 0)
    # grad.neg <- colorRampPalette(rev(color))(n)
    # grad.pos <- colorRampPalette(color)(n + 1)
    grad <- c(colorRampPalette(rev(color))(100), colorRampPalette(color)(101))
    cuts <- cut(x, 201, labels = FALSE)
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
