# previewcolor.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io

#' Preview color v2.0
#'
#' Preview one or multiple colors using little rhombi with their little labels up top
#'
#' @param x Color, vector: One or more colors that R understands
#' @param bg Background color. Default = "#333333" (dark gray)
#' @param main String: Title. Default = NULL, which results in
#' \code{deparse(substitute(x))}
#' @param main.x Float: x coordinate for \code{main}. Default = .75
#' @param main.y Float: y coordinate for \code{main}. Default = 0
#' @param main.adj Float: \code{adj} argument to mtext for \code{main}.
#' @param width Float: Plot width. Default = NULL, i.e. set automatically
#' @param asp Float: Plot aspect ratio. Default = 1
#' @param labels.y Float: y coord for labels. Default = 1.55 (rhombi are fixed and range y .5 - 1.5)
#' @param label.cex Float: Character expansion for labels. Default = NULL, and is calculated automatically based on
#' @param par.rest Logical: If TRUE, reset \code{par} setting on exit. Default = TRUE
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
                         main.col = "#b3b3b3",
                         main.x = .75,
                         main.y = 0.3,
                         width = NULL,
                         ylim = c(0.7, 2),
                         asp = 1,
                         labels.y = 1.55,
                         label.cex = NULL,
                         par.reset = TRUE) {

  if (is.null(main)) main <- deparse(substitute(x))
  x <- unlist(x)
  if (par.reset) {
    .par <- par(no.readonly = TRUE)
    on.exit(par(.par))
  }

  if (is.null(width)) width <- max(3, .3 * length(x))
  xlim <- c(0.3, width + .7)
  par(bg = bg, xaxs = "i", yaxs = "i", mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))

  # Plot ====
  plot(NULL, NULL, axes = FALSE,
       xlim = xlim, ylim = ylim, xlab = NA, ylab = NA,
       asp = asp)

  if (length(x) >= 3) {
    xmid <- seq(1, width, length.out = length(x))
  } else if (length(x) == 2) {
    xmid <- c(.3333 * width, .6666 * width) + .5
  } else {
    xmid <- .5*width + .5
  }

  for (i in seq(x)) rhombus(xmid[i], 1, col = x[i])

  # '- Labels ====
  if (is.null(label.cex)) {
    label.cex <- max(.1, 1.30 - .02 * length(x))
  }

  if (is.null(names(x))) {
    labels <- as.character(x)
  } else {
    labels <- names(x)
  }
  text(xmid - .2, labels.y, labels, col = x, srt = 45, adj = 0, offset = 0, cex = label.cex)

  # '- Title ====
  if (!is.null(main)) {
    text(main.x, main.y, main, col = main.col, adj = 0)
  }

} # rtemis::previewcolor


rhombus <- function(xmid = 1, ymid = 1, width = 1, height = 1, col = "#80FFFF") {
  # left, top, right, bottom
  hw <- .5*width
  hh <- .5*height
  polygon(x = c(xmid - hw, xmid, xmid + hw, xmid),
          y = c(ymid, ymid + hh, ymid, ymid - hh),
          col = col, border = NA)

}
