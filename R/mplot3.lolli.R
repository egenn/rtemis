# mplot3.lolli
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' \code{mplot3} Lollipop Plot
#'
#' @inheritParams mplot3.xy
#' @param x Float, vector: Input data
#' @param xnames Character, vector: Names of \code{x}
#' @param main Character: Main title
#' @param col Color, vector: Lollipop color
#' @param matching.stick.col Logical: If TRUE, color line segments using \code{col}, i.e. same as
#' @param stick.alpha Float: Transparency for line segments. Default = .5
#' points. Default = FALSE, in which case they are colored with \code{theme$fg}
#'
#' @author E.D. Gennatas
#' @export

mplot3.lolli <- function(x,
                         xnames = NULL,
                         main = NULL,
                         col = NULL,
                         matching.stick.col = FALSE,
                         stick.alpha = .333,
                         theme = getOption("rt.theme", "lightgrid"),
                         palette = getOption("rt.palette", "rtCol1"),
                         autolabel = letters,
                         par.reset = TRUE,
                         pdf.width = 6,
                         pdf.height = 6,
                         mar = c(2.5, 3, 2, 1),
                         pty = "m",
                         pch = 16,
                         x.axis.at = NULL,
                         y.axis.at = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         label.las = 1,
                         label.padj = .5,
                         xaxs = "r",
                         yaxs = "r",
                         xlab.adj = .5,
                         ylab.adj = .5,
                         filename = NULL, ...) {

  # Arguments ====
  if (is.null(xlab)) xlab <- labelify(deparse(substitute(x)))
  if (is.null(ylab)) ylab <- labelify(deparse(substitute(y)))
  if (is.null(xnames)) {
    xnames <- if (is.null(names(x))) {
      seq_along(x)
    } else {
      names(x)
    }
  }

  # Output directory
  if (!is.null(filename) && !dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }

  # Theme ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # Palette ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) col <- rep(palette[1], length(x))
  if (length(col) < length(x)) col <- recycle(col, x)

  # Plot ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex, xpd = FALSE)
  plot(NULL, NULL, xlim = getlim(x, axs = "i"), ylim = c(1, length(x)), axes = FALSE)

  # Plot bg ====
  if (!is.na(theme$plot.bg)) {
    .xlim <- getlim(x, axs = xaxs)
    .ylim <- getlim(seq_along(x), axs = yaxs)
    rect(.xlim[1], .ylim[1], .xlim[2], .ylim[2], border = NA, col = theme$plot.bg)
  }

  # Grid ====
  if (theme$grid) {
    grid(nx = theme$grid.nx,
         ny = NA,
         col = colorAdjust(theme$grid.col, theme$grid.alpha),
         lty = theme$grid.lty,
         lwd = theme$grid.lwd)
  }

  # Zero line ====
  .xlim <- getlim(x, axs = xaxs)
  if (theme$zerolines & .xlim[1] < 0 & 0 < .xlim[2]) {
    abline(v = 0, col = adjustcolor(theme$fg, theme$zerolines.alpha))
  }

  # Axes ====
  if (theme$axes.visible) {
    axis(side = theme$x.axis.side,
         line = theme$x.axis.line,
         at = x.axis.at,
         # labels = x.axis.labs,
         col = theme$axes.col,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col.axis = theme$tick.labels.col,
         las = theme$x.axis.las,
         padj = theme$x.axis.padj,
         hadj = theme$x.axis.hadj,
         tck = theme$tck,
         tcl = theme$tcl,
         cex = theme$cex,
         family = theme$font.family)
    axis(side = theme$y.axis.side,
         line = theme$y.axis.line,
         at = seq_along(x),
         labels = xnames,
         col = theme$axes.col,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col.axis = theme$tick.labels.col,
         # las = theme$y.axis.las,
         las = label.las,
         # padj = theme$y.axis.padj,
         padj = .5,
         # hadj = theme$y.axis.hadj,
         mgp = c(3, .5, 0),
         hadj = 1,
         tck = theme$tck,
         tcl = theme$tcl,
         cex = theme$cex,
         family = theme$font.family)
    mtext(xlab, side = theme$x.axis.side,
          line = theme$xlab.line,
          cex = theme$cex,
          adj = xlab.adj, col = theme$labs.col,
          family = theme$font.family)
    mtext(ylab, side = theme$y.axis.side,
          line = theme$ylab.line,
          cex = theme$cex,
          adj = ylab.adj, col = theme$labs.col,
          family = theme$font.family)
  }

  # Lollipops ====
  for (i in seq_along(x)) {
    segments(0, i, x[i], i, col = if (matching.stick.col) adjustcolor(col[[i]], stick.alpha) else adjustcolor(theme$fg, stick.alpha))
  }
  points(x, seq_along(x), col = unlist(col), pch = pch)

  # Main Title ====
  if (!is.null(rtenv$autolabel)) {
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

} # rtemis::mplot3.lolli
