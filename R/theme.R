# theme.R
# ::rtemis::
# E.D. Gennatas rtemis.org

# Black ----

#' Themes for `mplot3` and `dplot3` functions
#'
#' @param bg Color: Figure background
#' @param plot.bg Color: Plot region background
#' @param fg Color: Foreground color used as default for multiple elements like
#' axes and labels, which can be defined separately
#' @param pch Integer: Point character.
#' @param cex Float: Character expansion factor.
#' @param lwd Float: Line width.
#' @param bty Character: Box type:  "o", "l", "7", "c", "u", or "]", or "n".
#' Default = "n" (no box)
#' @param box.col Box color if `bty != "n"`
#' @param box.alpha Float: Box alpha
#' @param box.lty Integer: Box line type
#' @param box.lwd Float: Box line width
#' @param grid Logical: If TRUE, draw grid in plot regions
#' @param grid.nx Integer: N of vertical grid lines
#' @param grid.ny Integer: N of horizontal grid lines
#' @param grid.col Grid color
#' @param grid.alpha Float: Grid alpha
#' @param grid.lty Integer: Grid line type
#' @param grid.lwd Float: Grid line width
#' @param axes.visible Logical: If TRUE, draw axes
#' @param axes.col Axes colors
#' @param tick.col Tick color
#' @param tick.alpha Float: Tick alpha
#' @param tick.labels.col Tick labels' color
#' @param tck `graphics::parr`'s tck argument: Tick length, can be negative
#' @param tcl `graphics::parr`'s tcl argument
#' @param x.axis.side Integer: Side to place x-axis. Default = 1 (bottom)
#' @param y.axis.side Integer: Side to place y-axis. Default = 2 (left)
#' @param labs.col Labels' color
#' @param x.axis.line Numeric: `graphics::axis`'s `line` argument for the x-axis
#' @param x.axis.las Numeric: `graphics::axis`'s `las` argument for the x-axis
#' @param x.axis.padj Numeric: x-axis' `padj`: Adjustment for the x-axis
#' tick labels' position
#' @param x.axis.hadj Numeric: x-axis' `hadj`
#' @param y.axis.line Numeric: `graphics::axis`'s `line` argument for the y-axis
#' @param y.axis.las Numeric: `graphics::axis`'s `las` argument for the y-axis
#' @param y.axis.padj Numeric: y-axis' `padj`
#' @param y.axis.hadj Numeric: y-axis' `hadj`
#' @param xlab.line Numeric: Line to place `xlab`
#' @param ylab.line Numeric: Line to place `ylab`
#' @param zerolines Logical: If TRUE, draw lines on x = 0, y = 0, if within
#' plot limits
#' @param zerolines.col Zerolines color
#' @param zerolines.alpha Float: Zerolines alpha
#' @param zerolines.lty Integer: Zerolines line type
#' @param zerolines.lwd Float: Zerolines line width
#' @param main.line Float: How many lines away from the plot region to draw
#' title.
#' @param main.adj Float: How to align title. Default = 0 (left-align)
#' @param main.font Integer: 1: Regular, 2: Bold
#' @param main.col Title color
#' @param font.family Character: Font to be used throughout plot.
#'
#' @rdname theme
#' @export

theme_black <- function(
  bg = "#000000",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = FALSE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = .2,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = .5,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_black

#' @rdname theme
#' @export

theme_blackgrid <- function(
  bg = "#000000",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = .2,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_blackgrid


#' @rdname theme
#' @export
theme_blackigrid <- function(
  bg = "#000000",
  plot.bg = "#1A1A1A",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = bg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_darkgrid

# Darkgray ----

#' @rdname theme
#' @export
theme_darkgray <- function(
  bg = "#121212",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = FALSE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = .2,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = .5,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_black

#' @rdname theme
#' @export
theme_darkgraygrid <- function(
  bg = "#121212",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#404040",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = "#00000000",
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_darkgraygrid

#' @rdname theme
#' @export
theme_darkgrayigrid <- function(
  bg = "#121212",
  plot.bg = "#202020",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = bg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = "transparent",
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_darkgraygrid

# White ----

#' @rdname theme
#' @export
theme_white <- function(
  bg = "#ffffff",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = FALSE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = .5,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_white

#' @rdname theme
#' @export
theme_whitegrid <- function(
  bg = "#ffffff",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#c0c0c0",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = "#00000000",
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_whitegrid

#' @rdname theme
#' @export
theme_whiteigrid <- function(
  bg = "#ffffff",
  plot.bg = "#E6E6E6",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = bg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = "transparent",
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_darkgrid

# Grays ----

#' @rdname theme
#' @export
theme_lightgraygrid <- function(
  bg = "#dfdfdf",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#c0c0c0",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = "#00000000",
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_lightgray


#' @rdname theme
#' @export
theme_mediumgraygrid <- function(
  bg = "#b3b3b3",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = .5,
  # grid --
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#d0d0d0",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  # axes --
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = "#00000000",
  tick.alpha = 1,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = .5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = .5,
  y.axis.hadj = .5,
  xlab.line = 1.4,
  ylab.line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = .5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  # title --
  main.line = .25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
) {
  list(
    bg = bg,
    plot.bg = plot.bg,
    fg = fg,
    pch = pch,
    cex = cex,
    lwd = lwd,
    # box --
    bty = bty,
    box.col = box.col,
    box.alpha = box.alpha,
    box.lty = box.lty,
    box.lwd = box.lwd,
    # grid --
    grid = grid,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    grid.col = grid.col,
    grid.alpha = grid.alpha,
    grid.lty = grid.lty,
    grid.lwd = grid.lwd,
    # axes --
    axes.visible = axes.visible,
    axes.col = axes.col,
    tick.col = tick.col,
    tick.alpha = tick.alpha,
    tick.labels.col = tick.labels.col,
    tck = tck,
    tcl = tcl,
    x.axis.side = x.axis.side,
    y.axis.side = y.axis.side,
    labs.col = labs.col,
    x.axis.line = x.axis.line,
    x.axis.las = x.axis.las,
    x.axis.padj = x.axis.padj,
    x.axis.hadj = x.axis.hadj,
    y.axis.line = y.axis.line,
    y.axis.las = y.axis.las,
    y.axis.padj = y.axis.padj,
    y.axis.hadj = y.axis.hadj,
    xlab.line = xlab.line,
    ylab.line = ylab.line,
    # zerolines --
    zerolines = zerolines,
    zerolines.col = zerolines.col,
    zerolines.alpha = zerolines.alpha,
    zerolines.lty = zerolines.lty,
    zerolines.lwd = zerolines.lwd,
    # title --
    main.line = main.line,
    main.adj = main.adj,
    main.font = main.font,
    main.col = main.col,
    font.family = font.family
  )
} # rtemis::theme_mediumdgray

#' Print available rtemis themes
#'
#' @export
themes <- function() {
  cat(hilite("  Available themes:\n"))
  cat('    "white", "whitegrid", "whiteigrid,\n')
  cat('    "black", "blackgrid", "blackigrid",\n')
  cat('    "darkgray", "darkgraygrid", "darkgrayigrid",\n')
  cat('    "lightgraygrid", "mediumgraygrid"\n')
}
