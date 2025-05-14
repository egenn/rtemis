# mhist.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org
# TODO: Add group support

#' Histograms
#'
#' Draws a histogram using lines.
#'
#' Using `horiz = TRUE`, you can draw vertical histograms (as used by `mplot3_xym`)
#' @param x Input vector
#' @param breaks See `hist("breaks")` Default = "Sturges"
#' @param measure Character: "density"(Default), "counts"
#' @param lwd Float: Line width
#' @param xlim Vector, length 2: x-axis limits
#' @param ylim Vector, length 2: y-axis limits
#' @param plot.axes Logical: If TRUE, draws plot axes. Separate from `xaxis` and `yaxis`
#' @param xaxis Logical: If TRUE, draws x-axis
#' @param yaxis Logical: If TRUE, draws y-axis
#' @param xaxis.line Float: Number of lines into the margin to position `xaxis`. See `axis("line")`
#' @param yaxis.line Float: Number of lines into the margin to position `yaxis`. See `axis("line")`
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param xaxs Character: 'r' (Default): Extends x-axis range by 4 percent at each end, 'i': Does not extend x-axis range
#' @param yaxs Character: 'r' (Default): Extends y-axis range by 4 percent at each end, 'i': Does not extend y-axis range
#' @param box Logical: If TRUE, draws a box around plot
#' @param grid Logical: If TRUE, draws a grid
#' @param col Color to use for histogram lines
#' @param horiz Logical: If TRUE, switches x and y axes. Important: Provide all other arguments as if for a
#' non-rotated plot - i.e. `xlab` will become the y-axis label
#' @param main Character: Main title
#' @param add Logical: If TRUE, add histogram to existing plot (Caution: make sure the axes line up!)
#' @param ... Additional arguments to be passed to `graphics::plot`
#' @author E.D. Gennatas
#' @export

mhist <- function(
  x,
  breaks = "Sturges",
  measure = c("density", "counts"),
  lwd = 3,
  xlim = NULL,
  ylim = NULL,
  plot.axes = FALSE,
  xaxis = TRUE,
  yaxis = TRUE,
  xaxis.line = 0,
  yaxis.line = 0,
  xlab = NULL,
  ylab = measure,
  xaxs = "r",
  yaxs = "r",
  box = FALSE,
  grid = FALSE,
  col = pennCol$lighterBlue,
  horiz = FALSE,
  main = "",
  add = FALSE,
  ...
) {
  # [ Arguments ] ----
  measure <- match.arg(measure)
  xhist <- hist(x, breaks = breaks, plot = FALSE)
  .x <- if (horiz) xhist[[measure]] else xhist$mids
  .y <- if (horiz) xhist$mids else xhist[[measure]]
  if (is.null(xlab)) xlab <- deparse(substitute(x))
  if (is.null(ylim)) ylim <- c(0, max(xhist[[measure]]))
  .xlim <- if (horiz) ylim else xlim
  .ylim <- if (horiz) xlim else ylim
  .xaxs <- if (horiz) yaxs else xaxs
  .yaxs <- if (horiz) xaxs else yaxs
  .xaxis <- if (horiz) yaxis else xaxis
  .yaxis <- if (horiz) xaxis else yaxis
  .xaxis.line <- if (horiz) yaxis.line else xaxis.line
  .yaxis.line <- if (horiz) xaxis.line else yaxis.line

  # [ PLOT ] ----
  if (add) par(new = TRUE)
  plot(
    .x,
    .y,
    type = "n",
    xlim = .xlim,
    ylim = .ylim,
    axes = plot.axes,
    xaxs = .xaxs,
    yaxs = .yaxs,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  if (.xaxis) axis(1, line = .xaxis.line)
  if (.yaxis) axis(2, line = .yaxis.line)

  # [ BOX ] ----
  if (box) box()

  # [ GRID ] ----
  if (grid) grid()

  # [ HIST ] ----
  if (horiz) {
    for (i in seq_along(.x)) {
      lines(c(0, .x[i]), c(.y[i], .y[i]), lwd = lwd, col = col, ...)
    }
  } else {
    for (i in seq_along(.x)) {
      lines(c(.x[i], .x[i]), c(0, .y[i]), lwd = lwd, col = col, ...)
    }
  }
} # rtemis::mhist
