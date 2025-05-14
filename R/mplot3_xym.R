# mplot3_xym.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org
# TODO: Add group support for marginal plots to mhist

#' Scatter plot with marginal density and/or histogram
#'
#' Draw a scatter plot with fit line and marginal density and/or histogram
#'
#' To make wide plot, change `widths`: e.g. widths = c(7, 1)
#' @inheritParams mplot3_xy
#' @param x Numeric vector: x-axis data
#' @param y Numeric vector: y-axis data
#' @param margin Character: "density", "histogram", or "both". Type of marginal plots to draw.
#' @param fit Character: Algorithm to use to draw `y ~ x`.
#' @param se.fit Logical: If TRUE: plot +/- 2 * Standard Error of fit
#' @param col Color for marginal plots
#' @param density.alpha Numeric: Alpha for density plots
#' @param hist.breaks Integer: Number of histogram breaks
#' @param hist.alpha Numeric: Alpha for barplots
#' @param hist.space Numeric: Space between bars in barplots
#' @param hist.lwd Numeric: Line width for barplots
#' @param lwd Numeric: Line width
#' @param main Character: Main title
#' @param main.adj Numeric: Main title adjustment
#' @param margin.mar Numeric: Margin for marginal plots
#' @param axes.density Logical: If TRUE, plot margin plot axes for density (debugging only)
#' @param par.reset Logical: Resest `par` to original settings
#' @param ... Additional arguments to passed to [mplot3_xy]
#'
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' x <- rnorm(500)
#' y <- x^3 + 12 + rnorm(500)
#' mplot3_xym(x, y)
#' }
#' @export

mplot3_xym <- function(
  x,
  y,
  margin = c("histogram", "density", "both"),
  fit = "gam",
  se.fit = TRUE,
  xlim = NULL,
  ylim = NULL,
  col = "#18A3AC",
  density.alpha = .66,
  hist.breaks = 30,
  hist.alpha = .66,
  hist.space = .05,
  hist.lwd = 3,
  lwd = 4,
  main = NULL,
  main.adj = 0,
  axes.density = FALSE,
  pty = "m",
  mar = c(3, 3, 0, 0),
  margin.mar = .2,
  xaxs = "r",
  yaxs = "r",
  theme = rtTheme,
  par.reset = TRUE,
  widths = NULL,
  heights = NULL,
  filename = NULL,
  pdf.width = 7,
  pdf.height = 7,
  ...
) {
  # [ Arguments ] ----
  margin <- match.arg(margin)
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename))
    grDevices::pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )

  # Auto max fill
  if (is.null(widths)) {
    devw <- dev.size()[1]
    widths <- c(devw, devw / 5)
  }

  if (is.null(heights)) {
    devh <- dev.size()[2]
    heights <- c(devh / 5, devh)
  }

  # [ LAYOUT ] ----
  lmat <- cbind(c(2, 1), c(0, 3))
  layout(lmat, widths = widths, heights = heights, respect = TRUE)
  # layout.show(3)

  # [ PLOT 1: Scatter ] ----
  lims <- mplot3_xy(
    x,
    y,
    fit = fit,
    se.fit = se.fit,
    fit.col = col,
    xlim = xlim,
    ylim = ylim,
    xaxs = xaxs,
    yaxs = yaxs,
    par.reset = FALSE,
    mar = mar,
    lwd = lwd,
    pty = pty,
    theme = theme,
    return.lims = TRUE,
    ...
  )

  # [ PLOTS 2 & 3: Histogram / Density ] ----
  if (margin == "density") {
    # Density Top
    mplot3_x(
      x,
      type = margin,
      axes.visible = axes.density,
      density.mean = FALSE,
      col = col,
      alpha = density.alpha,
      lwd = lwd,
      pty = "m",
      xaxs = "i",
      par.reset = FALSE,
      mar = c(margin.mar, 3, 1, 0),
      xlim = lims$xlim
    )
    # Density Right
    mplot3_x(
      y,
      type = margin,
      axes.visible = axes.density,
      density.mean = FALSE,
      col = col,
      alpha = density.alpha,
      lwd = lwd,
      pty = "m",
      axes.swap = TRUE,
      yaxs = "i",
      par.reset = FALSE,
      mar = c(3, margin.mar, 0, 1),
      xlim = lims$ylim
    )
  } else if (margin == "histogram") {
    x.breaks <- seq(min(x), max(x), diff(range(x)) / hist.breaks)
    y.breaks <- seq(min(y), max(y), diff(range(y)) / hist.breaks)
    # xhist <- hist(x, breaks = x.breaks, plot = FALSE)
    # yhist <- hist(y, breaks = y.breaks, plot = FALSE)

    # Histogram Top
    par(mar = c(margin.mar, 3, 1, 0), pty = "m")
    mhist(
      x,
      breaks = x.breaks,
      col = col,
      xlim = lims$xlim,
      xaxs = "i",
      yaxs = "r",
      lwd = hist.lwd,
      plot.axes = FALSE,
      xaxis = FALSE,
      yaxis = FALSE
    )
    # Histogram Right
    par(mar = c(3, margin.mar, 0, 1), pty = "m")
    mhist(
      y,
      breaks = y.breaks,
      col = col,
      xlim = lims$ylim,
      xaxs = "i",
      yaxs = "r",
      lwd = hist.lwd,
      horiz = TRUE,
      plot.axes = FALSE,
      xaxis = FALSE,
      yaxis = FALSE
    )
  } else if (margin == "both") {
    # Histogram data
    x.breaks <- seq(min(x), max(x), diff(range(x)) / hist.breaks)
    y.breaks <- seq(min(y), max(y), diff(range(y)) / hist.breaks)
    # xhist <- hist(x, breaks = x.breaks, plot = FALSE)
    # yhist <- hist(y, breaks = y.breaks, plot = FALSE)
    # densityLim <- max(c(xhist$density, yhist$density))

    # Both Top
    par(mar = c(margin.mar, 3, 1, 0), pty = "m")
    mhist(
      x,
      breaks = x.breaks,
      col = col,
      xlim = lims$xlim,
      xaxs = "i",
      yaxs = "r",
      lwd = hist.lwd,
      plot.axes = FALSE,
      xaxis = FALSE,
      yaxis = FALSE
    )
    mplot3_x(
      x,
      type = "density",
      axes.visible = axes.density,
      density.mean = FALSE,
      col = col,
      alpha = density.alpha,
      lwd = lwd,
      pty = "m",
      xaxs = "i",
      new = TRUE,
      par.reset = FALSE,
      mar = c(margin.mar, 3, 1, 0),
      xlim = lims$xlim
    )

    # Both Right
    par(mar = c(3, margin.mar, 0, 1), pty = "m")
    mhist(
      y,
      breaks = y.breaks,
      col = col,
      xlim = lims$ylim,
      xaxs = "i",
      yaxs = "r",
      lwd = hist.lwd,
      horiz = TRUE,
      plot.axes = FALSE,
      xaxis = FALSE,
      yaxis = FALSE
    )
    mplot3_x(
      y,
      type = "density",
      axes.visible = axes.density,
      density.mean = FALSE,
      col = col,
      alpha = density.alpha,
      lwd = lwd,
      pty = "m",
      axes.swap = TRUE,
      yaxs = "i",
      new = TRUE,
      par.reset = FALSE,
      mar = c(3, margin.mar, 0, 1),
      xlim = lims$ylim
    )
  }
  if (!is.null(filename)) grDevices::dev.off()
} # rtemis::mplot3_xym
