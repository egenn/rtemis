# mplot3.box
# ::rtemis::
# 2017-2021 E.D. Gennatas lambdamd.org

#' \code{mplot3}: Boxplot
#'
#' Draw boxplots
#'
#' @inheritParams mplot3.xy
#' @param x Vector, data.frame or list: Each data.frame column or list element will be drawn as a box
#' @param col Vector of colors to use
#' @param alpha Float: Alpha to be applied to \code{col}
#' @param border Color for lines around boxes
#' @param horizontal Logical: If TRUE, draw horizontal boxplot(s). Default = FALSR
#' @param na.rm Logical: If TRUE, remove NA values, otherwise function will give error.
#' Default = TRUE
#' @param ... Additional arguments to \code{graphics::boxplot}
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' ## vector
#' x <- rnorm(500)
#' mplot3.box(x)
#'
#' ## data.frame
#' x <- data.frame(alpha = rnorm(50), beta = rnorm(50), gamma = rnorm(50))
#' mplot3.box(x)
#'
#' ## list - allows different length vectors
#' x <- list(alpha = rnorm(50),
#'           beta = rnorm(80, 4, 1.5),
#'           gamma = rnorm(30, -3, .5))
#' mplot3.box(x)
#' }
#' @export

mplot3.box <- function(x,
                       col = NULL,
                       alpha = .66,
                       border = NULL,
                       border.alpha = 1,
                       space = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       boxwex = .5,
                       horizontal = FALSE,
                       main = NULL,
                       names.arg = NULL,
                       axisnames = FALSE,
                       xnames = NULL,
                       xnames.at = NULL,
                       xnames.y = NULL,
                       xnames.font = 1,
                       xnames.adj = NULL,
                       xnames.pos = NULL,
                       xnames.srt = NULL,
                       legend = FALSE,
                       legend.names = NULL,
                       legend.position = "topright",
                       legend.inset = c(0, 0),
                       mar = NULL,
                       pty = "m",
                       cex.axis = cex,
                       cex.names = cex,
                       yaxis = TRUE,
                       ylim.pad = 0,
                       theme = getOption("rt.theme", "lightgrid"),
                       labelify = TRUE,
                       autolabel = letters,
                       na.rm = TRUE,
                       palette = getOption("rt.palette", "rtCol1"),
                       par.reset = TRUE,
                       pdf.width = 6,
                       pdf.height = 6,
                       filename = NULL, ...) {

  # [ ARGUMENTS ] ====

  # Group names
  if (is.null(xnames)) {
    if (!is.null(names(x))) {
      xnames <- names(x)
    } else {
      xnames <- deparse(substitute(x))
    }
  }
  if (labelify) xnames <- labelify(xnames)
  if (!is.list(x)) x <- list(x)
  if (!is.null(xnames)) {
    if (is.null(xnames.at)) {
      xnames.at <- seq_along(x)
    }
  }

  if (is.null(xnames.srt)) {
    xnames.srt <- ifelse(length(x) > 3, 90, 0)
  }

  if (is.null(xnames.adj)) {
    xnames.adj <- if (xnames.srt == 0) c(.5, 1) else 1
  }
  if (is.character(palette)) palette <- rtPalette(palette)

  if (is.null(col)) {
    if (length(x) == 1) {
      col <- palette[1]
    } else {
      col <- palette[seq(length(x))]
    }
  }

  # mar ====
  if (is.null(mar)) {
    mar.top <- if (is.null(main)) 1 else 2
    mar.bottom <- if (xnames.srt == 90) max(nchar(xnames)) * .5 else 2.5
    mar <- c(mar.bottom, 3, mar.top, 1)
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

  # [ XLIM & YLIM ] ====
  xv <- unlist(x)
  if (is.null(xlim)) xlim <- c(.5, length(x) + .5)
  if (is.null(ylim)) ylim <- c(min(xv, na.rm = na.rm) - .06 * abs(min(xv, na.rm = na.rm)),
                               max(xv, na.rm = na.rm) + .05 * abs(max(xv, na.rm = na.rm)))

  if (horizontal) {
    xxlim <- ylim
    ylim <- xlim
    xlim <- xxlim
  }

  # [ PLOT ] ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex)
  plot(NULL, NULL, xlim = xlim, ylim = ylim, bty = "n",
       axes = FALSE, ann = FALSE,
       xaxs = "i", yaxs = "i")

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
                boxwex = boxwex,
                horizontal = horizontal,
                ylim = ylim,
                axes = FALSE,
                add = TRUE,
                xlab = NULL)

  # [ y AXIS ] ====
  if (yaxis) {
    axis(side = 2,
         # at = y.axis.at,
         # labels = y.axis.labs,
         line = theme$y.axis.line,
         las = theme$y.axis.las,
         padj = theme$y.axis.padj,
         hadj = theme$y.axis.hadj,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col.axis = theme$tick.labels.col, # the axis numbers i.e. tick labels
         tck = theme$tck,
         tcl = theme$tcl,
         cex = theme$cex,
         family = theme$font.family)
  }

  # [ MAIN TITLE ] ====
  if (exists("autolabel", envir = rtenv)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (!is.null(main)) {
    mtext(main, line = theme$main.line,
          font = theme$main.font, adj = theme$main.adj,
          cex = theme$cex, col = theme$main.col,
          family = theme$font.family)
  }

  # [ XNAMES ] ====
  if (is.null(xnames.y)) {
    xnames.y <- ylo(.04)
  }
  if (!is.null(xnames)) {
    text(x = xnames.at, y = xnames.y,
         labels = xnames,
         adj = xnames.adj,
         pos = xnames.pos,
         srt = xnames.srt, xpd = TRUE,
         font = xnames.font,
         col = theme$labs.col,
         family = theme$font.family)
  }

  # [ AXIS LABS ] ====
  if (!is.null(xlab))  mtext(xlab, 1, cex = theme$cex, line = theme$xlab.line)
  if (!is.null(ylab))  mtext(ylab, 2, cex = theme$cex, line = theme$ylab.line)

  if (!is.null(xlab)) mtext(xlab, side = theme$x.axis.side,
                            line = theme$xlab.line, cex = theme$cex,
                            # adj = xlab.adj,
                            col = theme$labs.col,
                            family = theme$font.family)
  if (!is.null(ylab)) mtext(ylab, side = theme$y.axis.side,
                            line = theme$ylab.line, cex = theme$cex,
                            # adj = ylab.adj,
                            col = theme$labs.col,
                            family = theme$font.family)

  # [ OUTRO ] ====
  if (!is.null(filename)) dev.off()
  invisible(bp)

} # rtemis::mplot3.box
