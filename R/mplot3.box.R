# mplot3.box
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io
# TODO: make x vector or list

#' \code{mplot3}: Boxplot
#'
#' Draw boxplots
#'
#' @inheritParams mplot3.xy
#' @param x Matrix: Each column will be drawn as a box
#' @param col Vector of colors to use
#' @param alpha Float: Alpha to be applied to \code{col}
#' @param border Color for lines around boxes
#' @param ... Additional arguments to \code{graphics::boxplot}
#' @author Efstathios D. Gennatas
#' @export

mplot3.box <- function(x,
                       col = NULL,
                       alpha = .66,
                       border = NULL,
                       border.alpha = 1,
                       # pch = 16,
                       space = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xlab = NULL,
                       xlab.line = 1.5,
                       ylab = NULL,
                       ylab.line = 1.5,
                       main = NULL,
                       # main.line = .5,
                       # main.adj = 0,
                       # main.col = NULL,
                       # main.font = 2,
                       # main.family = "",
                       names.arg = NULL,
                       axisnames = FALSE,
                       group.names = NULL,
                       group.names.at = NULL,
                       group.names.y = NULL,
                       # group.names.line = 0.5,
                       group.names.font = 1,
                       group.names.adj = .5,
                       group.names.srt = 0,
                       legend = FALSE,
                       legend.names = NULL,
                       legend.position = "topright",
                       legend.inset = c(0, 0),
                       mar = c(4, 2, 2.5, .5),
                       pty = "m",
                       # cex = 1.2,
                       cex.axis = cex,
                       cex.names = cex,
                       # bg = NULL,
                       # plot.bg = NULL,
                       boxplot.axes = FALSE,
                       yaxis = TRUE,
                       ylim.pad = 0,
                       y.axis.padj = 1.7,
                       tck = -.015,
                       # tick.col = NULL,
                       theme = getOption("rt.theme", "white"),
                       palette = getOption("rt.palette", "rtCol1"),
                       # axes.col = NULL,
                       # labs.col = NULL,
                       # grid = FALSE,
                       # grid.lty = NULL,
                       # grid.lwd = NULL,
                       # grid.col = NULL,
                       # grid.alpha = 1,
                       par.reset = TRUE,
                       pdf.width = 6,
                       pdf.height = 6,
                       filename = NULL, ...) {

  # [ ARGUMENTS ] ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) {
    if (NCOL(x) == 1) {
      col <- palette[1]
    } else {
      col <- palette[seq(NCOL(x))]
    }
  }

  # Group names
  if (is.null(group.names)) {
    if (!is.null(colnames(x))) group.names <- colnames(x)
  }

  if (!is.null(group.names)) {
    if (is.null(group.names.at)) {
      group.names.at <- seq(NCOL(x))
    }
  }

  col.alpha <- colorAdjust(col, alpha = alpha)
  if (is.null(border)) border <- colorAdjust(col, alpha = border.alpha)
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # [ THEME ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # [ DATA ] ====
  x <- as.matrix(x)

  # [ XLIM & YLIM ] ====
  # .dat <- boxplot(x, plot = FALSE)
  if (is.null(xlim)) xlim <- c(.5, NCOL(x) + .5)
  # if (is.null(ylim)) ylim <- c(min(.dat$stats[1, ]), max(.dat$stats[5, ]))
  if (is.null(ylim)) ylim <- c(min(x), max(x))

  # [ PLOT ] ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex)
  plot(NULL, NULL, xlim = xlim, ylim = ylim, bty = "n", axes = FALSE, ann = FALSE)

  # [ PLOT BG ] ====
  if (!is.na(theme$plot.bg)) {
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = theme$plot.bg)
  }

  # [ GRID ] ====
  if (theme$grid) {
    grid(0,
         ny = theme$grid.ny,
         col = colorAdjust(theme$grid.col, theme$grid.alpha),
         lty = theme$grid.lty,
         lwd = theme$grid.lwd)
  }

  # [ BOXPLOT ] ====
  bp <- boxplot(x, col = col.alpha,
                pch = theme$pch,
                border = border,
                ylim = ylim,
                axes = boxplot.axes,
                add = TRUE,
                xlab = NULL, ...)

  # [ y AXIS ] ====
  if (yaxis) {
    axis(side = 2,
         # at = y.axis.at,
         # labels = y.axis.labs,
         col = theme$axes.col,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col.axis = theme$tick.labels.col,
         padj = y.axis.padj, tck = tck,
         cex = theme$cex,
         family = theme$font.family)
  }

  # [ MAIN TITLE ] ====
  if (exists("autolabel", envir = rtenv)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    mtext(main, line = theme$main.line,
          font = theme$main.font, adj = theme$main.adj,
          cex = theme$cex, col = theme$main.col,
          family = theme$font.family)
  }

  # [ GROUP NAMES ] ====
  if (is.null(group.names.y)) {
    group.names.y <- min(ylim) - diff(ylim) * .2
  }
  if (!is.null(group.names)) {
    text(x = group.names.at, y = group.names.y,
         labels = group.names,
         adj = group.names.adj,
         srt = group.names.srt, xpd = TRUE,
         font = group.names.font,
         col = theme$labs.col,
         family = theme$font.family)
  }

  # [ AXIS LABS ] ====
  if (!is.null(xlab))  mtext(xlab, 1, cex = cex, line = xlab.line)
  if (!is.null(ylab))  mtext(ylab, 2, cex = cex, line = ylab.line)

  if (!is.null(xlab)) mtext(xlab, side = theme$x.axis.side,
                            line = xlab.line, cex = theme$cex,
                            # adj = xlab.adj,
                            col = theme$labs.col,
                            family = theme$font.family)
  if (!is.null(ylab)) mtext(ylab, side = theme$y.axis.side,
                            line = ylab.line, cex = theme$cex,
                            # adj = ylab.adj,
                            col = theme$labs.col,
                            family = theme$font.family)

  # [ OUTRO ] ====
  if (!is.null(filename)) dev.off()

  invisible(bp)

} # rtemis::mplot3.box
