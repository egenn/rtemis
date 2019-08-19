# dplot3.bar.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Dynamic Barplots
#'
#' Draw a dynamic barplot using \code{plotly}
#'
#' This is an early release, part of an ongoing effort to complete graphic functions.
#' Will be adding theme support, among other options.
#' @param x data.frame: Input where rows are groups (can be 1 row), columns are features
#' @param main Character: Plot title. Default = NULL
#' @param xlab Character: x-axis label. Default = NULL
#' @param ylab  Character: y-axis label. Default = NULL
#' @param col Color, vector: Color for bars. Defaults to colors defined by \code{palette}
#' @param alpha Float (0, 1]: Transparency for bar colors. Default = .8
#' @param bg Color: Background color. Default = "white"
#' @param plot.bg Color: Background color for plot area. Default = "white"
#' @param palette Character: Name of `rtemis` palette to use. Default = "rtCol1". Only used if \code{col = NULL}
#' @param group.names Character, vector, length = NROW(x): Group names. Default = NULL, which uses \code{rownames(x)}
#' @param feature.names Character, vector, length = NCOL(x): Feature names. Default = NULL, which uses
#' \code{colnames(x)}
#' @param font.size  Float: Font size for all labels excluding main. Default = 16
#' @param font.alpha Float (0, 1]: Transparency for fonts. Default = .8
#' @param font.col Color: Font color. Default = "black"
#' @param font.family String: Font family to use. Default = "Helvetica Neue"
#' @param grid.col Color: Grid color. Default = "gray85"
#' @param grid.alpha Float (0, 1]: Transparency for \code{grid.col}
#' @param margin Named list: plot margins. Default = \code{list(t = 35)}
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.bar(VADeaths)
#' }

dplot3.bar <-  function(x,
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        col = NULL,
                        alpha = .8,
                        bg = "white",
                        plot.bg = "gray95",
                        palette = getOption("rt.palette", "rtCol1"),
                        group.names = NULL,
                        feature.names = NULL,
                        font.size = 16,
                        font.alpha = .8,
                        font.col = "black",
                        font.family = "Helvetica Neue",
                        grid.col = "gray85",
                        grid.alpha = .8,
                        margin = list(t = 35)) {

  # Arguments ====
  color.bygroup <- FALSE

  dat <- as.data.frame(x)

  # Group names ====
  .group.names <- group.names
  if (is.null(group.names)) {
    if (!is.null(rownames(x))) .group.names <- rownames(x)
  } else if (is.numeric(group.names)) {
    .group.names <- dat[, group.names]
    rownames(dat) <- .group.names
    dat <- dat[, -group.names]
  }

  # Feature names ====
  .feature.names <- feature.names
  if (is.null(.feature.names)) {
    if (!is.null(colnames(dat))) {
      .feature.names <- colnames(dat)
    } else {
      .feature.names <- paste0("Feature", seq(NCOL(dat)))
    }
  }

  # Colors ====
  plot.bg <- plotly::toRGB(plot.bg)
  font.col <- plotly::toRGB(font.col, font.alpha)
  grid.col <- plotly::toRGB(grid.col, grid.alpha)

  if (is.character(palette)) palette <- rtPalette(palette)
  p <- NCOL(dat)
  n <- NROW(dat)
  if (is.null(col)) {
    if (p == 1) {
      col <- palette[1]
    } else {
      if (color.bygroup) {
        col <- rep(palette[seq(p)], each = n)
      } else {
        col <- rep(palette[seq(n)], p)
      }
    }
  }

  if (length(col) < p) col <- rep(col, p/length(col))

  # plotly ====
  plt <- plotly::plot_ly(dat, x = .group.names, y = dat[, 1],
                         type = 'bar',
                         name = .feature.names[1],
                         marker = list(color = plotly::toRGB(col[1], alpha)))
  if (p > 1) {
    for (i in seq_len(p)[-1]) plt <- plotly::add_trace(plt, y = dat[, i],
                                                       name = .feature.names[i],
                                                       marker = list(color = plotly::toRGB(col[i], alpha)))
  }

  # '- layout ====
  plt <- plotly::layout(plt,
                        yaxis = list(title = ylab,
                                     gridcolor = grid.col),
                        xaxis = list(title = xlab,
                                     gridcolor = grid.col),
                        barmode = "group",  # group works without actual groups too
                        title = main,
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        font = list(family = font.family,
                                    size = font.size,
                                    color = font.col),
                        margin = margin)

  plt


} # rtemis::dplot3.bar.R
