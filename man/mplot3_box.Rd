% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplot3_box.R
\name{mplot3_box}
\alias{mplot3_box}
\title{\code{mplot3}: Boxplot}
\usage{
mplot3_box(
  x,
  col = NULL,
  alpha = 0.66,
  border = NULL,
  border.alpha = 1,
  group.spacing = 0.25,
  xlim = NULL,
  ylim = NULL,
  xlab = NULL,
  ylab = NULL,
  boxwex = NULL,
  staplewex = 0.5,
  horizontal = FALSE,
  main = NULL,
  groupnames = NULL,
  xnames = NULL,
  xnames.at = NULL,
  xnames.y = NULL,
  xnames.font = 1,
  xnames.adj = NULL,
  xnames.pos = NULL,
  xnames.srt = NULL,
  order.by.fn = NULL,
  legend = FALSE,
  legend.names = NULL,
  legend.position = "topright",
  legend.inset = c(0, 0),
  mar = NULL,
  oma = rep(0, 4),
  pty = "m",
  yaxis = TRUE,
  ylim.pad = 0,
  theme = rtTheme,
  labelify = TRUE,
  autolabel = letters,
  na.rm = TRUE,
  palette = rtPalette,
  par.reset = TRUE,
  pdf.width = 6,
  pdf.height = 6,
  filename = NULL,
  ...
)
}
\arguments{
\item{x}{Vector, data.frame or list: Each data.frame column or list element will be drawn as a box}

\item{col}{Vector of colors to use}

\item{alpha}{Numeric: \code{col} transparency}

\item{border}{Color for lines around boxes}

\item{border.alpha}{Numeric: \code{border} transparency}

\item{group.spacing}{Numeric: Spacing between groups of boxes (when input is data.frame or list)}

\item{xlim}{Float vector, length 2: x-axis limits}

\item{ylim}{Float vector, length 2: y-axis limits}

\item{xlab}{Character: x-axis label}

\item{ylab}{Character: y-axis label}

\item{boxwex}{Numeric: Scale factor for box width. Default = .5}

\item{staplewex}{Numeric: max and min line ("staple") width proportional to box. Default = .5}

\item{horizontal}{Logical: If TRUE, draw horizontal boxplot(s).}

\item{main}{Character: Plot title}

\item{groupnames}{Character vector: Group names}

\item{xnames}{Character vector: Names for individual boxes}

\item{xnames.at}{Numeric: Position of xnames}

\item{order.by.fn}{Character: "mean", "median" or any function that outputs a single number: E
stimate function on each vector and order boxes (when input is data.frame or list) by ascending
order. Default = NULL, i.e. no reordering}

\item{mar}{Float, vector, length 4: Margins; see \code{par("mar")}}

\item{oma}{Float, vector, length 4: Outer margins; see \code{par("oma")}}

\item{pty}{Character: "s" gives a square plot; "m" gives a plot that fills
graphics device size. Default = "m" (See \code{par("pty")})}

\item{theme}{Character: Run \code{themes()} for available themes}

\item{autolabel}{Character vector to be used to generate autolabels when using
\link{rtlayout} with \code{autolabel = TRUE}.}

\item{na.rm}{Logical: If TRUE, remove NA values, otherwise function will give error.
Default = TRUE}

\item{palette}{Vector of colors, or Character defining a builtin palette -
get options with \code{rtpalette()}}

\item{par.reset}{Logical: If TRUE, reset \code{par} setting before exiting.}

\item{pdf.width}{Float: Width in inches for pdf output (if \code{filename}
is set).}

\item{pdf.height}{Float: Height in inches for pdf output.}

\item{filename}{Character: Path to file to save plot. Default = NULL}

\item{...}{Additional arguments to \code{graphics::boxplot}}
}
\description{
Draw boxplots of a vector (single box), data.frame (one box per column) or list
(one box per element - good for variable of different length)
}
\details{
Note that argument \code{xnames} refers to the x-axis labels below each box. If not specified, these
are inferred from the input when possible. Argument \code{xlab} is a single label for the x-axis as
per usual and often omitted if \code{xnames} suffice.
}
\examples{
\dontrun{
## vector
x <- rnorm(500)
mplot3_box(x)

## data.frame - each column one boxplot
x <- data.frame(alpha = rnorm(50), beta = rnorm(50), gamma = rnorm(50))
mplot3_box(x)

## list of vectors - allows different length vectors
x <- list(alpha = rnorm(50),
          beta = rnorm(80, 4, 1.5),
          gamma = rnorm(30, -3, .5))
mplot3_box(x)

## grouped boxplots: input a list of lists. outer list: groups; inner lists: matched data vectors
x <- list(Cases = list(Weight = rnorm(50), Temperature = rnorm(45, 1)),
          Controls = list(Weight = rnorm(80), Temperature = rnorm(72)))
mplot3_box(x)
}
}
\author{
E.D. Gennatas
}
