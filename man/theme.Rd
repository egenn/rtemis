% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_black}
\alias{theme_black}
\alias{theme_blackgrid}
\alias{theme_blackigrid}
\alias{theme_darkgray}
\alias{theme_darkgraygrid}
\alias{theme_darkgrayigrid}
\alias{theme_white}
\alias{theme_whitegrid}
\alias{theme_whiteigrid}
\alias{theme_lightgraygrid}
\alias{theme_mediumgraygrid}
\title{Themes for \code{mplot3} and \code{dplot3} functions}
\usage{
theme_black(
  bg = "#000000",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = FALSE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = 0.2,
  grid.lty = 1,
  grid.lwd = 1,
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = 0.5,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_blackgrid(
  bg = "#000000",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = 0.2,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_blackigrid(
  bg = "#000000",
  plot.bg = "#1A1A1A",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = bg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_darkgray(
  bg = "#121212",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = FALSE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = 0.2,
  grid.lty = 1,
  grid.lwd = 1,
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = 0.5,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_darkgraygrid(
  bg = "#121212",
  plot.bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#404040",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_darkgrayigrid(
  bg = "#121212",
  plot.bg = "#202020",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = bg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_white(
  bg = "#ffffff",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = FALSE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = fg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
  axes.visible = TRUE,
  axes.col = "transparent",
  tick.col = fg,
  tick.alpha = 0.5,
  tick.labels.col = fg,
  tck = -0.01,
  tcl = NA,
  x.axis.side = 1,
  y.axis.side = 2,
  labs.col = fg,
  x.axis.line = 0,
  x.axis.las = 0,
  x.axis.padj = -1.1,
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_whitegrid(
  bg = "#ffffff",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#c0c0c0",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_whiteigrid(
  bg = "#ffffff",
  plot.bg = "#E6E6E6",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = bg,
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_lightgraygrid(
  bg = "#dfdfdf",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#c0c0c0",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)

theme_mediumgraygrid(
  bg = "#b3b3b3",
  plot.bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  bty = "n",
  box.col = fg,
  box.alpha = 1,
  box.lty = 1,
  box.lwd = 0.5,
  grid = TRUE,
  grid.nx = NULL,
  grid.ny = NULL,
  grid.col = "#d0d0d0",
  grid.alpha = 1,
  grid.lty = 1,
  grid.lwd = 1,
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
  x.axis.hadj = 0.5,
  y.axis.line = 0,
  y.axis.las = 1,
  y.axis.padj = 0.5,
  y.axis.hadj = 0.5,
  xlab.line = 1.4,
  ylab.line = 2,
  zerolines = TRUE,
  zerolines.col = fg,
  zerolines.alpha = 0.5,
  zerolines.lty = 1,
  zerolines.lwd = 1,
  main.line = 0.25,
  main.adj = 0,
  main.font = 2,
  main.col = fg,
  font.family = getOption("rt.font", "Helvetica")
)
}
\arguments{
\item{bg}{Color: Figure background}

\item{plot.bg}{Color: Plot region background}

\item{fg}{Color: Foreground color used as default for multiple elements like
axes and labels, which can be defined separately}

\item{pch}{Integer: Point character.}

\item{cex}{Float: Character expansion factor.}

\item{lwd}{Float: Line width.}

\item{bty}{Character: Box type:  "o", "l", "7", "c", "u", or "]", or "n".
Default = "n" (no box)}

\item{box.col}{Box color if \code{bty != "n"}}

\item{box.alpha}{Float: Box alpha}

\item{box.lty}{Integer: Box line type}

\item{box.lwd}{Float: Box line width}

\item{grid}{Logical: If TRUE, draw grid in plot regions}

\item{grid.nx}{Integer: N of vertical grid lines}

\item{grid.ny}{Integer: N of horizontal grid lines}

\item{grid.col}{Grid color}

\item{grid.alpha}{Float: Grid alpha}

\item{grid.lty}{Integer: Grid line type}

\item{grid.lwd}{Float: Grid line width}

\item{axes.visible}{Logical: If TRUE, draw axes}

\item{axes.col}{Axes colors}

\item{tick.col}{Tick color}

\item{tick.alpha}{Float: Tick alpha}

\item{tick.labels.col}{Tick labels' color}

\item{tck}{\code{graphics::parr}'s tck argument: Tick length, can be negative}

\item{tcl}{\code{graphics::parr}'s tcl argument}

\item{x.axis.side}{Integer: Side to place x-axis. Default = 1 (bottom)}

\item{y.axis.side}{Integer: Side to place y-axis. Default = 2 (left)}

\item{labs.col}{Labels' color}

\item{x.axis.line}{Numeric: \code{graphics::axis}'s \code{line} argument for the x-axis}

\item{x.axis.las}{Numeric: \code{graphics::axis}'s \code{las} argument for the x-axis}

\item{x.axis.padj}{Numeric: x-axis' \code{padj}: Adjustment for the x-axis
tick labels' position}

\item{x.axis.hadj}{Numeric: x-axis' \code{hadj}}

\item{y.axis.line}{Numeric: \code{graphics::axis}'s \code{line} argument for the y-axis}

\item{y.axis.las}{Numeric: \code{graphics::axis}'s \code{las} argument for the y-axis}

\item{y.axis.padj}{Numeric: y-axis' \code{padj}}

\item{y.axis.hadj}{Numeric: y-axis' \code{hadj}}

\item{xlab.line}{Numeric: Line to place \code{xlab}}

\item{ylab.line}{Numeric: Line to place \code{ylab}}

\item{zerolines}{Logical: If TRUE, draw lines on x = 0, y = 0, if within
plot limits}

\item{zerolines.col}{Zerolines color}

\item{zerolines.alpha}{Float: Zerolines alpha}

\item{zerolines.lty}{Integer: Zerolines line type}

\item{zerolines.lwd}{Float: Zerolines line width}

\item{main.line}{Float: How many lines away from the plot region to draw
title.}

\item{main.adj}{Float: How to align title. Default = 0 (left-align)}

\item{main.font}{Integer: 1: Regular, 2: Bold}

\item{main.col}{Title color}

\item{font.family}{Character: Font to be used throughout plot.}
}
\description{
Themes for \code{mplot3} and \code{dplot3} functions
}
