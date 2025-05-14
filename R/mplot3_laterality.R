# mplot3_laterality.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Laterality scatter plot
#'
#' @inheritParams mplot3_xy
#' @param x data.frame or data.table which includes columns with ROI names ending in "_L" or "_R"
#' @param regionnames Character, vector: Regions to plot. For example, if `regionnames`
#' contains "Ant_Insula", `x` must contain columns `Ant_Insula_L` and `Ant_Insula_R`
#' @param summary.fn Character: Name of function to summarize left and right values.
#' Default = "median"
#' @param summary.lty Integer: line type for summary arrows
#' @param summary.lwd Float: line width for summary arrows
#' @param summary.col  Color for summary arrows
#' @param arrowhead.length Float: arrowhead length in inches. Default = .075
#' @param deltas Logical, If TRUE, show summary statistics. Default = TRUE
#' @param line.col Color for individual cases' lines
#' @param line.alpha Float: transparency for individual lines
#' @param lty Integer: Line type for individual lines. Default = 1
#' @param lwd Float: Line width for individual lines. Default = .3
#' @param ylim Float, vector, length 2: y-axis limits
#' @param labelify Logical: If TRUE, [labelify] regionnames
#'
#' @author E.D. Gennatas
#' @export

mplot3_laterality <- function(
  x,
  regionnames,
  main = NULL,
  ylab = "Left to Right",
  summary.fn = "median",
  summary.lty = 1,
  summary.lwd = 2.5,
  summary.col = NULL,
  arrowhead.length = .075,
  deltas = TRUE,
  line.col = theme$fg,
  line.alpha = .25,
  lty = 1,
  lwd = .3,
  ylim = NULL,
  theme = rtTheme,
  labelify = TRUE,
  autolabel = letters,
  # na.rm = TRUE,
  mar = NULL,
  oma = rep(0, 4),
  pty = "m",
  palette = rtPalette,
  par.reset = TRUE,
  pdf.width = 6,
  pdf.height = 6,
  filename = NULL,
  ...
) {
  x <- as.data.table(x)
  xnames <- names(x)
  .names <- c(paste0(regionnames, "_L"), paste0(regionnames, "_R"))
  index <- sapply(.names, function(i) grep(paste0(i, "$"), xnames))
  # appease R CMD check: with = FALSE instead of ..index
  if (is.null(ylim)) ylim <- getlim(unlist(x[, index, with = FALSE]))
  xlim <- c(.5, length(regionnames) * 2 + .5)
  if (is.character(palette)) palette <- rtpalette(palette)
  if (is.null(summary.col)) summary.col <- palette[seq_along(regionnames)]
  if (is.null(mar)) {
    bottom.mar <- textwidth(regionnames)
    mar <- c(bottom.mar, 3, 2, 1)
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

  # Plot ----
  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  par.orig <- par(no.readonly = TRUE)
  if (!is.null(rtenv$rtpar)) {
    par.reset <- FALSE
    par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex)
  } else {
    par(mar = mar, oma = oma, bg = theme$bg, pty = pty, cex = theme$cex)
  }
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

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

  # Plot bg ----
  if (theme$plot.bg != "transparent") {
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = theme$plot.bg)
  }

  # Grid ----
  if (theme$grid) {
    grid(
      nx = 0,
      ny = theme$grid.ny,
      col = colorAdjust(theme$grid.col, theme$grid.alpha),
      lty = theme$grid.lty,
      lwd = theme$grid.lwd
    )
  }

  # lat plot ----
  line.col <- adjustcolor(line.col, line.alpha)
  delta <- numeric(length(regionnames))
  for (i in seq_along(regionnames)) {
    vleft <- x[[grep(paste0(regionnames[i], "_L$"), names(x))]]
    vright <- x[[grep(paste0(regionnames[i], "_R$"), names(x))]]
    vleft.summary <- do.call(summary.fn, list(vleft))
    vright.summary <- do.call(summary.fn, list(vright))
    segments(
      x0 = 2 * i - 1.2,
      x1 = 2 * i + .2,
      y0 = vleft,
      y1 = vright,
      lty = lty,
      lwd = lwd,
      col = line.col
    )
    arrows(
      x0 = 2 * i - 1.2,
      x1 = 2 * i + .2,
      y0 = vleft.summary,
      y1 = vright.summary,
      length = arrowhead.length,
      lty = summary.lty,
      lwd = summary.lwd,
      col = summary.col[[i]]
    )
    delta[i] <- vright.summary - vleft.summary
  }

  # y-axis ----
  axis(
    side = 2,
    las = theme$y.axis.las,
    padj = theme$y.axis.padj,
    hadj = theme$y.axis.hadj,
    col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
    col = NA, # The axis line, which we want to omit
    col.axis = theme$tick.labels.col, # the axis numbers i.e. tick labels
    tck = theme$tck,
    tcl = theme$tcl,
    cex = theme$cex,
    family = theme$font.family
  )

  # regionnames ----
  mtext(
    text = if (labelify) labelify(regionnames) else regionnames,
    side = 1,
    line = 1,
    las = 2,
    at = seq(regionnames) * 2 - .5,
    col = theme$labs.col
  )

  # ylab ----
  if (!is.null(ylab))
    mtext(
      ylab,
      side = theme$y.axis.side,
      line = theme$ylab.line,
      cex = theme$cex,
      # adj = ylab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )

  # deltas ----
  if (deltas) {
    mtext(
      text = paste(summary.fn, "delta"),
      side = 3,
      line = 1.1,
      adj = 0,
      col = adjustcolor(theme$fg, .5)
    )
    mtext(
      text = ddSci(delta),
      side = 3,
      line = .2,
      at = seq(regionnames) * 2 - .5,
      col = unlist(summary.col)
    )
  }

  # Main Title ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (!is.null(main)) {
    mtext(
      text = main,
      side = 3,
      line = theme$main.line,
      font = theme$main.font,
      adj = theme$main.adj,
      cex = theme$cex,
      col = theme$main.col,
      family = theme$font.family
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()
  invisible(delta)
} # rtemis::mplot3_laterality
