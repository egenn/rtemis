% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/previewcolor.R
\name{previewcolor}
\alias{previewcolor}
\title{Preview color v2.0}
\usage{
previewcolor(
  x,
  main = NULL,
  bg = "#333333",
  main.col = "#b3b3b3",
  main.x = 0.7,
  main.y = 0.2,
  main.adj = 0,
  main.cex = 0.9,
  main.font = 1,
  width = NULL,
  xlim = NULL,
  ylim = c(0, 2.2),
  asp = 1,
  labels.y = 1.55,
  label.cex = NULL,
  mar = c(0, 0, 0, 1),
  par.reset = TRUE,
  filename = NULL,
  pdf.width = 8,
  pdf.height = 2.5
)
}
\arguments{
\item{x}{Color, vector: One or more colors that R understands}

\item{main}{Character: Title. Default = NULL, which results in
\code{deparse(substitute(x))}}

\item{bg}{Background color.}

\item{main.col}{Color: Title color}

\item{main.x}{Float: x coordinate for \code{main}.}

\item{main.y}{Float: y coordinate for \code{main}.}

\item{main.adj}{Float: \code{adj} argument to mtext for \code{main}.}

\item{main.cex}{Float: character expansion factor for \code{main}. Default = .9}

\item{main.font}{Integer, 1 or 2: Weight of \code{main} 1: regular, 2: bold. Default = 2}

\item{width}{Float: Plot width. Default = NULL, i.e. set automatically}

\item{xlim}{Vector, length 2: x-axis limits. Default = NULL, i.e. set automatically}

\item{ylim}{Vector, length 2: y-axis limits.}

\item{asp}{Float: Plot aspect ratio.}

\item{labels.y}{Float: y coord for labels. Default = 1.55 (rhombi are fixed and range y .5 - 1.5)}

\item{label.cex}{Float: Character expansion for labels. Default = NULL, and is
calculated automatically based on length of \code{x}}

\item{mar}{Numeric vector, length 4: margin size.}

\item{par.reset}{Logical: If TRUE, reset \code{par} settings on exit.}

\item{filename}{Character: Path to save plot as PDF.}

\item{pdf.width}{Numeric: Width of PDF in inches.}

\item{pdf.height}{Numeric: Height of PDF in inches.}
}
\value{
Nothing, prints plot
}
\description{
Preview one or multiple colors using little rhombi with their little labels up top
}
\examples{
colors <- colorgradient.x(seq(-5, 5))
previewcolor(colors)
}
