% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorGrad.R
\name{colorGrad}
\alias{colorGrad}
\title{Color Gradient}
\usage{
colorGrad(
  n = 21,
  colors = NULL,
  space = c("rgb", "Lab"),
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  preview = FALSE,
  colorbar = FALSE,
  cb.n = 21,
  cb.mar = c(1, 1, 1, 1),
  cb.add = FALSE,
  cb.add.mar = c(5, 0, 2, 5),
  cb.axis.pos = 1.1,
  cb.axis.las = 1,
  cb.axis.hadj = 0,
  cb.cex = 6,
  bar.min = -1,
  bar.mid = 0,
  bar.max = 1,
  cex = 1.2,
  filename = NULL,
  pdf.width = 3,
  pdf.height = 7,
  theme = getOption("rt.theme", "light"),
  bg = NULL,
  col.text = NULL,
  plotlycb = FALSE,
  plotly.width = 80,
  plotly.height = 500,
  rtrn.plotly = FALSE,
  margins = c(0, 0, 0, 0),
  pad = 0,
  par.reset = TRUE
)
}
\arguments{
\item{n}{Integer: How many distinct colors you want. If not odd, converted to \code{n + 1}
Defaults to 21}

\item{colors}{Character: Acts as a shortcut to defining \code{lo}, \code{mid}, etc for a number of defaults:
"french", "penn", "grnblkred",}

\item{space}{Character: Which colorspace to use. Option: "rgb", or "Lab". Default = "rgb".
Recommendation: If \code{mid} is "white" or "black" (default), use "rgb", otherwise "Lab"}

\item{lo}{Color for low end}

\item{lomid}{Color for low-mid}

\item{mid}{Color for middle of the range or "mean", which will result in \code{colorOp(c(lo, hi), "mean")}.
If \code{mid = NA}, then only \code{lo} and \code{hi} are used to create the color gradient.}

\item{midhi}{Color for middle-high}

\item{hi}{Color for high end}

\item{preview}{Logical: Plot the colors horizontally}

\item{colorbar}{Logical: Create a vertical colorbar}

\item{cb.n}{Integer: How many steps you would like in the colorbar}

\item{cb.mar}{Vector, length 4: Colorbar margins. Default: c(1, 1, 1, 1)}

\item{cb.add}{Logical: If TRUE, colorbar will be added to existing plot}

\item{cb.add.mar}{Vector: Margins for colorbar (See \code{par("mar")})}

\item{cb.axis.pos}{Float: Position of axis (See \code{axis("pos")})}

\item{cb.axis.las}{Integer {0,1,2,3}: Style of axis labels. 0: Always parallel to the axis,
1: Horizontal, 2: Perpendicular, 3: Vertical. Default = 1}

\item{cb.axis.hadj}{Float: Adjustment parallel to the reading direction (See \code{par("adj")})}

\item{cb.cex}{FLoat: Character expansion factor for colorbar (See \code{par("cex")})}

\item{bar.min}{Numeric: Lowest value in colorbar}

\item{bar.mid}{Numeric: Middle value in colorbar}

\item{bar.max}{Numeric: Max value in colorbar}

\item{cex}{Float: Character expansion for axis}

\item{filename}{String (Optional: Path to file to save colorbar}

\item{pdf.width}{Float: Width for PDF output. Default = 3}

\item{pdf.height}{Float: Height for PDF output. Default = 7}

\item{theme}{Character: "light", "dark"}

\item{bg}{Color: Background color}

\item{col.text}{Color: Colorbar text color}

\item{plotlycb}{Logical: Create colorbar using \code{plotly} (instead of base R graphics)}

\item{plotly.width}{Float: Width for plotly colorbar. Default = 80}

\item{plotly.height}{Float: Height for plotly colorbar. Default = 500}

\item{rtrn.plotly}{Logical: If TRUE, return \code{plotly} object}

\item{margins}{Vector: Plotly margins. Default = c(0, 0, 0, 0)}

\item{pad}{Float: Padding for \code{plotly}. Default = 0}

\item{par.reset}{Logical: If TRUE (Default), reset \code{par} settings after running}
}
\value{
Invisible vector of hexadecimal colors / plotly object if \code{rtrn.plotly = TRUE}
}
\description{
Create a gradient of colors and optionally a colorbar
}
\details{
It is best to provide an odd number, so that there is always an equal number of colors on either side
of the midpoint.
For example, if you want a gradient from -1 to 1 or equivalent, an n = 11, will give 5 colors on either
side of 0, each representing a 20\\% change from the next.

\code{colors} can be defined as a sequence of 3-letter color abbreviations of 2, 3, 4, or 5 colors
which will correspond to values: \{"lo","hi"\}; \{"lo", "mid", "hi"\}; \{"lo", "mid", "midhi", "hi"\}, and
\{"lo", "lomid", "mid", "midhi", "hi"\}, respectively.
For example, try \code{colorGrad(21, "blugrnblkredyel", colorbar = TRUE)}
3-letter color abbreviations:
wht: white; blk: black; red; grn: green; blu: blue; yel: yellow; rng: orange; prl: purple
}
\author{
E.D. Gennatas
}
