# previewcolor.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io

#' Preview color v2.0
#'
#' Preview one or multiple colors using little rhombi with their little labels up top
#'
#' @param x Color, vector: One or more colors that R understands
#' @param bg Background color. Default = "#333333" (dark gray)
#' @param width Float: Plot width. Default = NULL, i.e. set automatically
#' @param asp Float: Plot aspect ratio. Default = 1
#' @param labels.y Float: y coord for labels. Default = 1.55 (rhombi are fixed and range y .5 - 1.5)
#' @param label.cex Float: Character expansion for labels. Default = NULL, and is calculated automatically based on
#' length of \code{x} in order to look reasonable in your RStudio plot panel.
#'
#' @return Nothing, prints plot
#' @export
#'
#' @examples
#' colors <- colorgradient.x(seq(-5, 5))
#' previewcolor(colors)
previewcolor <- function(x,
                         bg = "#333333",
                         main = NULL,
                         main.col = "#FFFFFF",
                         width = NULL,
                         ylim = c(0.7, 2),
                         asp = 1,
                         labels.y = 1.55,
                         label.cex = NULL) {

  x <- unlist(x)
  .par <- par(no.readonly = TRUE)
  on.exit(par(.par))

  if (is.null(width)) width <- max(5, .3 * length(x))
  xlim <- c(0.3, width + .7)
  par(bg = bg, xaxs = "i", yaxs = "i", mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  plot(NULL, NULL, axes = FALSE,
       xlim = xlim, ylim = ylim, xlab = NA, ylab = NA,
       asp = asp)
  xmid <- seq(1, width, length.out = length(x))
  for (i in seq(x)) rhombus(xmid[i], 1, col = x[i])

  # labels
  if (is.null(label.cex)) {
    # label.cex <- max(.1, 1.0555 - .0111 * length(x))
    label.cex <- max(.1, 1.30 - .02 * length(x))
  }

  if (is.null(names(x))) {
    labels <- as.character(x)
  } else {
    labels <- names(x)
  }
  text(xmid - .2, labels.y, labels, col = x, srt = 45, adj = 0, offset = 0, cex = label.cex)

  # Title
  if (!is.null(main)) mtext(main, 3, -1, adj = 0, col = main.col)

} # rtemis::previewcolor

rhombus <- function(xmid = 1, ymid = 1, width = 1, height = 1, col = "#80FFFF") {
  # left, top, right, bottom
  hw <- .5*width
  hh <- .5*height
  polygon(x = c(xmid - hw, xmid, xmid + hw, xmid),
          y = c(ymid, ymid + hh, ymid, ymid - hh),
          col = col, border = NA)

}
