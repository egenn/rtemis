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
                       pch = 16,
                       space = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xlab = NULL,
                       xlab.line = 1.5,
                       ylab = NULL,
                       ylab.line = 1.5,
                       main = NULL,
                       main.line = .5,
                       main.adj = 0,
                       main.col = NULL,
                       main.font = 2,
                       main.family = "",
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
                       cex = 1.2,
                       cex.axis = cex,
                       cex.names = cex,
                       bg = NULL,
                       plot.bg = NULL,
                       boxplot.axes = FALSE,
                       yaxis = TRUE,
                       ylim.pad = 0,
                       y.axis.padj = 1.7,
                       tck = -.015,
                       tick.col = NULL,
                       theme = getOption("rt.theme", "light"),
                       palette = getOption("rt.palette", "rtCol1"),
                       axes.col = NULL,
                       labs.col = NULL,
                       grid = FALSE,
                       grid.lty = NULL,
                       grid.lwd = NULL,
                       grid.col = NULL,
                       grid.alpha = 1,
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

  cols <- colorAdjust(col, alpha = alpha)
  if (is.null(border)) border <- colorAdjust(cols, alpha = border.alpha)
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # [ DATA ] ====
  x <- as.matrix(x)

  # [ XLIM & YLIM ] ====
  .dat <- boxplot(x, plot = FALSE)
  if (is.null(xlim)) xlim <- c(.5, NCOL(x) + .5)
  # if (is.null(ylim)) ylim <- c(min(.dat$stats[1, ]), max(.dat$stats[5, ]))
  if (is.null(ylim)) ylim <- c(min(x), max(x))

  # # Add x% either side (unless zero)
  # ylim[1] <- ylim[1] + ylim.pad * ylim[1]
  # ylim[2] <- ylim[2] + ylim.pad * ylim[2]

  # [ THEMES ] ====
  # Defaults for all themes
  if (is.null(grid.lty)) grid.lty <- 1
  if (is.null(grid.lwd)) grid.lwd <- 1

  if (theme == "lightgrid" | theme == "darkgrid") {
    if (is.null(grid.lty)) grid.lty <- 1
    # if (is.null(zero.lty)) zero.lty <- 1
    if (is.null(grid.lwd)) grid.lwd <- 1.5
  }
  if (theme == "lightgrid") {
    theme <- "light"
    if (is.null(plot.bg)) plot.bg <- "gray90"
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "white"
    if (is.null(tick.col)) tick.col <- "white"
  }
  if (theme == "darkgrid") {
    theme <- "dark"
    if (is.null(plot.bg)) plot.bg <- "gray15"
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "black"
    if (is.null(tick.col)) tick.col <- "black"
  }
  themes <- c("light", "dark", "box", "darkbox")
  if (!theme %in% themes) {
    warning(paste(theme, "is not an accepted option; defaulting to \"light\""))
    theme <- "light"
  }

  if (theme == "light") {
    if (is.null(bg)) bg <- "white"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("black", alpha.f = point.alpha))
    #   col <- list("gray30")
    # }
    # box.col <- "white"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray10"
    if (is.null(labs.col)) labs.col <- "gray10"
    if (is.null(main.col)) main.col <- "black"
    if (is.null(grid.col)) grid.col <- "black"
    # if (is.null(diagonal.col)) diagonal.col <- "black"
    # if (is.null(hline.col)) hline.col <- "black"
    # gen.col <- "black"
  } else if (theme == "dark") {
    if (is.null(bg)) bg <- "black"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("white", alpha.f = point.alpha))
    #   col <- list("gray70")
    # }
    # box.col <- "black"
    if (is.null(axes.col)) axes.col <- adjustcolor("black", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray90"
    if (is.null(labs.col)) labs.col <- "gray90"
    if (is.null(main.col)) main.col <- "white"
    if (is.null(grid.col)) grid.col <- "white"
    # if (is.null(diagonal.col)) diagonal.col <- "white"
    # if (is.null(hline.col)) hline.col <- "white"
    gen.col <- "white"
  } else if (theme == "box") {
    if (is.null(bg)) bg <- "white"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("black", alpha.f = point.alpha))
    #   col <- list("gray30")
    # }
    # if (is.null(box.col)) box.col <- "gray10"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray10"
    if (is.null(labs.col)) labs.col <- "gray10"
    if (is.null(main.col)) main.col <- "black"
    if (is.null(grid.col)) grid.col <- "black"
    # if (is.null(diagonal.col)) diagonal.col <- "black"
    # if (is.null(hline.col)) hline.col <- "black"
    gen.col <- "black"
  } else if (theme == "darkbox") {
    if (is.null(bg)) bg <- "black"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("white", alpha.f = point.alpha))
    #   col <- list("gray70")
    # }
    # if (is.null(box.col)) box.col <- "gray90"
    if (is.null(axes.col)) axes.col <- adjustcolor("black", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray90"
    if (is.null(labs.col)) labs.col <- "gray90"
    if (is.null(main.col)) main.col <- "white"
    if (is.null(grid.col)) grid.col <- "white"
    # if (is.null(diagonal.col)) diagonal.col <- "white"
    # if (is.null(hline.col)) hline.col <- "white"
    gen.col <- "white"
  }

  # [ PLOT ] ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  par(mar = mar, bg = bg, pty = pty, cex = cex)
  plot(NULL, NULL, xlim = xlim, ylim = ylim, bty = 'n', axes = FALSE, ann = FALSE)

  # [ PLOT BG ] ====
  if (!is.null(plot.bg)) {
    bg.ylim <- c(min(ylim) - .04 * diff(range(ylim)), max(ylim) + .04 * diff(range(ylim)))
    rect(xlim[1], bg.ylim[1], xlim[2], bg.ylim[2], border = NA, col = plot.bg)
  }

  # [ GRID ] ====
  grid.col <- colorAdjust(grid.col, grid.alpha)
  if (grid) grid(col = grid.col, lty = grid.lty, lwd = grid.lwd, ny = NULL, nx = 0)

  # [ BOXPLOT ] ====
  bp <- boxplot(x, col = cols,
                pch = pch,
                border = border,
                ylim = ylim,
                axes = boxplot.axes,
                add = TRUE,
                xlab = NULL, ...)

  # [ y AXIS ] ====
  if (yaxis) axis(2, col = axes.col, col.axis = labs.col, col.ticks = tick.col,
                  padj = y.axis.padj, tck = tck, cex = cex)

  # [ MAIN ] ====
  if (!is.null(main)) {
    # suppressWarnings(mtext(bquote(paste(bold(.(main)))), line = main.line,
    #                        adj = main.adj, cex = cex, col = main.col))
    mtext(main, line = main.line, font = main.font, family = main.family,
          adj = main.adj, cex = cex, col = main.col)
  }

  # [ GROUP NAMES ] ====
  if (is.null(group.names.y)) {
    group.names.y <- min(ylim) - diff(ylim) * .2
  }
  if (!is.null(group.names)) {
    # mtext(group.names, side = 1, line = group.names.line, at = group.names.at,
    #       font = group.names.font, cex = cex)
    text(x = group.names.at, y = group.names.y,
         labels = group.names,
         adj = group.names.adj,
         srt = group.names.srt, xpd = TRUE,
         font = group.names.font,
         col = labs.col)
  }

  # # [ LEGEND ] ====
  # if (legend) {
  #   legend(legend.position, legend = legend.names,
  #          fill = cols, inset = legend.inset, xpd = TRUE, bty = "n")
  # }

  # [ AXIS LABS ] ====
  if (!is.null(xlab))  mtext(xlab, 1, cex = cex, line = xlab.line)
  if (!is.null(ylab))  mtext(ylab, 2, cex = cex, line = ylab.line)

  # [ OUTRO ] ====
  if (!is.null(filename)) dev.off()

  invisible(bp)

} # rtemis::mplot3.box
