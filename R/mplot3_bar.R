# mplot3_bar
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org
# TODO: groups support (set xlim for besides = T)

#' `mplot3`: Barplot
#'
#' Draw barplots
#'
#' @inheritParams mplot3_xy
#' @param x Vector or Matrix: If Vector, each value will be drawn as a bar.
#' If Matrix, each column is a vector, so multiple columns signify a different group.
#' e.g. Columns could be months and rows could be N days sunshine, N days rainfall, N days snow, etc.
#' @param error Vector or Matrix: If Vector, each value will be drawn as an error bar.
#' If Matrix, each column is a vector, so multiple columns signify a different group.
#' @param col Vector of colors to use
#' @param alpha Float: Alpha to be applied to `col`
#' @param border Color if you wish to draw border around bars, NA for no borders (Default)
#' @param space Float: Space left free on either side of the bars, as a fraction of bar width. A single number or a
#' vector, one value per bar. If `x` is a matrix, space can be length 2 vector, signifying space between bars
#' within group and between groups. Default = c(0, 1) if x is matrix and `beside = TRUE`, otherwise Default = .2
#' @param color.bygroup Logical: If TRUE, and input is a matrix, each group's bars will be given the same color,
#' otherwise bars across groups will be given the same sequence of colors. Default = FALSE
#' @param legend Logical: If TRUE, and input is matrix, draw legend for each case. Note: you may need to adjust
#' `mar` and `legend.inset` if you want to place the legend outside the plot
#' (can use e.g.`legend.inset = c(-.5, 0)`)
#' @param ... Additional arguments to `graphics::barplot`
#' @author E.D. Gennatas
#' @export

mplot3_bar <- function(
  x,
  error = NULL,
  col = NULL,
  error.col = "white",
  error.lwd = 2,
  alpha = 1,
  beside = TRUE,
  border = NA,
  width = 1,
  space = NULL, # c(1, .2),
  xlim = NULL,
  ylim = NULL,
  xlab = NULL,
  # xlab.line = 1.5,
  ylab = NULL,
  # ylab.line = 1.5,
  main = NULL,
  las = 1.5,
  xnames = NULL,
  xnames.srt = 0,
  xnames.adj = ifelse(xnames.srt == 0, .5, 1),
  xnames.line = 0.5,
  xnames.font = 1,
  xnames.cex = 1,
  xnames.y.pad = .08,
  xnames.at = NULL,
  color.bygroup = FALSE,
  group.legend = NULL,
  legend.x = NULL,
  legend.y = NULL,
  # legend.side = 3,
  # legend.adj = 0,
  # legend.at = NA,
  group.names = NULL,
  # legend.position = "topright",
  # legend.inset = c(0, 0),
  legend.font = 1, # 1: regular, 2: bold
  bartoplabels = NULL,
  bartoplabels.line = 0,
  bartoplabels.font = 1,
  mar = c(2.5, 3, 2, 1),
  pty = "m",
  barplot.axes = FALSE,
  yaxis = TRUE,
  ylim.pad = .04,
  # y.axis.padj = 1,
  # tck = -.015,
  theme = rtTheme,
  palette = rtPalette,
  autolabel = letters,
  par.reset = TRUE,
  pdf.width = 6,
  pdf.height = 6,
  filename = NULL,
  ...
) {
  # Arguments ----
  # Compatibility with rtlayout()
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE
  if (is.character(palette)) palette <- rtpalette(palette)
  x <- as.matrix(x)
  if (!is.null(error)) error <- as.matrix(error)
  if (NCOL(x) == 1) {
    xnames <- rownames(x)
    x <- c(x)
    error <- c(error)
  } else if (NROW(x) == 1) {
    xnames <- colnames(x)
    x <- c(x)
    error <- c(error)
  }

  p <- NCOL(x)
  n <- NROW(x)
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

  if (length(col) < p) col <- rep(col, p / length(col))

  if (is.null(space)) {
    space <- if (min(size(x)) >= 2 && beside) c(.1, .5) else .75
  }

  # Legend names
  if (is.null(group.names)) {
    group.names <- if (!is.null(rownames(x))) rownames(x) else
      paste0("Case", seq_len(NROW(x)))
  }

  cols <- colorAdjust(col, alpha = alpha)

  # Output directory
  if (!is.null(filename) && !dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # NAMES for vectors ----
  # if (is.null(xnames)) {
  #   # if (NROW(x) == 1 & !is.null(colnames(x))) xnames <- colnames(x)
  #   # if (NCOL(x) == 1 & !is.null(rownames(x))) xnames <- rownames(x)
  #   if (is.vector(x) & !is.null(names(x))) {
  #     xnames <- names(x)
  #   } else {
  #     xnames <- colnames(x)
  #   }
  # }
  if (is.null(xnames)) xnames <- colnames(x)

  # Data ----
  # x must be vector or matrix for barplot()
  # if (min(size(x)) > 1) x <- as.matrix(x)

  # if (!is.null(dim(x))) {
  #   if (NROW(x) == 1) {
  #     x <- as.numeric(x)
  #     if (!is.null(error)) error <- as.numeric(error)
  #   }
  # }
  # if (!is.null(error)) error <- as.matrix(error)

  # if (is.null(dim(x))) {
  #   if (is.null(group.legend)) group.legend <- FALSE
  # } else {
  #   if (is.null(group.legend)) group.legend <- TRUE
  # }
  # if (is.null(group.legend)) group.legend <- FALSE

  # 2up
  if (is.null(group.legend)) {
    group.legend <- if (!is.null(dim(x))) TRUE else FALSE
  }

  # PLOT ----
  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Expand right margin for legend
  # if (group.legend) mar[4] <- mar[4] + max(strwidth(group.names)) + 3
  if (group.legend) mar[4] <- mar[4] + .5 * max(nchar(group.names)) + .2
  par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex, xpd = FALSE)

  # XLIM & YLIM ----
  xlim <- range(barplot(
    x,
    beside = beside,
    width = width,
    space = space,
    plot = FALSE
  ))
  xlim[1] <- xlim[1] - .5 - max(space)
  xlim[2] <- xlim[2] + .5 + max(space)
  if (is.null(ylim)) {
    if (is.null(error)) {
      ylim <- if (beside) range(c(0, x)) else range(c(0, colSums(x)))
    } else {
      ylim <- range(c(0, x + error))
    }
  }

  # Add x% either side (unless zero)
  if (ylim[1] != 0) ylim[1] <- ylim[1] - ylim.pad * diff(ylim)
  ylim[2] <- ylim[2] + ylim.pad * diff(ylim)
  plot(
    NULL,
    NULL,
    xlim = xlim,
    ylim = ylim,
    bty = "n",
    axes = FALSE,
    ann = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # PLOT BG ----
  if (theme$plot.bg != "transparent") {
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = theme$plot.bg)
  }

  # GRID ----
  if (theme$grid) {
    grid(
      nx = 0,
      ny = theme$grid.ny,
      col = colorAdjust(theme$grid.col, theme$grid.alpha),
      lty = theme$grid.lty,
      lwd = theme$grid.lwd
    )
  }

  # BARPLOT ----
  barCenters <- barplot(
    x,
    beside = beside,
    col = cols,
    border = border,
    ylim = ylim,
    axes = barplot.axes,
    cex.axis = theme$cex,
    cex.names = theme$cex,
    add = TRUE,
    xlab = NULL,
    axisnames = FALSE,
    las = las,
    col.axis = theme$labs.col,
    width = width,
    space = space
  )

  # ERROR BARS ----
  if (!is.null(error)) {
    segments(
      as.vector(barCenters),
      as.vector(x) - as.vector(error),
      as.vector(barCenters),
      as.vector(x) + as.vector(error),
      lwd = error.lwd,
      col = error.col
    )

    arrows(
      barCenters,
      x - as.vector(error),
      barCenters,
      x + as.vector(error),
      lwd = error.lwd,
      angle = 90,
      code = 3,
      length = 0.05,
      col = error.col
    )
  }

  # y AXIS ----
  if (yaxis) {
    axis(
      side = 2,
      line = theme$y.axis.line,
      las = theme$y.axis.las,
      padj = theme$y.axis.padj,
      hadj = theme$y.axis.hadj,
      col = theme$axes.col,
      col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
      col.axis = theme$tick.labels.col,
      tck = theme$tck,
      cex = theme$cex,
      family = theme$font.family
    )
  }

  # MAIN TITLE ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    mtext(
      main,
      line = theme$main.line,
      font = theme$main.font,
      adj = theme$main.adj,
      cex = theme$cex,
      col = theme$main.col,
      family = theme$font.family
    )
  }

  # XNAMES ----
  if (!is.null(xnames)) {
    if (is.null(xnames.at)) {
      xnames.at <- if (NCOL(x) == 1 || (NROW(x) > 1 && !beside))
        c(barCenters) else colMeans(barCenters)
    }
    text(
      x = xnames.at,
      y = min(ylim) - diff(ylim) * xnames.y.pad,
      labels = xnames,
      srt = xnames.srt,
      adj = xnames.adj,
      xpd = TRUE,
      font = xnames.font,
      cex = 1, # multiplied by par("cex"), which is already theme$cex
      col = theme$labs.col,
      family = theme$font.family
    )
  }

  # GROUP LEGEND ----
  if (group.legend) {
    if (is.null(legend.x)) legend.x <- rep(xlim[2] + .01 * diff(xlim), n)
    if (is.null(legend.y))
      legend.y <- seq(
        ylim[2],
        ylim[2] - max(strheight(group.names)) * n,
        length.out = n
      )
    text(
      legend.x,
      legend.y,
      group.names,
      adj = 0,
      cex = 1,
      col = unlist(col[seq_len(n)]),
      xpd = TRUE,
      font = legend.font,
      family = theme$font.family
    )
  }

  # AXIS LABS ----
  if (!is.null(xlab)) {
    mtext(
      xlab,
      1,
      cex = theme$cex,
      line = theme$xlab.line,
      col = theme$labs.col,
      family = theme$font.family
    )
  }
  if (!is.null(ylab)) {
    mtext(
      ylab,
      2,
      cex = theme$cex,
      line = theme$ylab.line,
      col = theme$labs.col,
      family = theme$font.family
    )
  }

  # BARTOP LABELS ----
  if (!is.null(bartoplabels)) {
    mtext(
      bartoplabels,
      3,
      line = bartoplabels.line,
      at = barCenters,
      cex = theme$cex,
      font = bartoplabels.font,
      family = theme$font.family
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()
  invisible(barCenters)
} # rtemis::mplot3_bar
