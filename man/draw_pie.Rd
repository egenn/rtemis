% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_pie.R
\name{draw_pie}
\alias{draw_pie}
\title{Interactive Pie Chart}
\usage{
draw_pie(
  x,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = 0.8,
  bg = NULL,
  plot_bg = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  category_names = NULL,
  textinfo = "label+percent",
  font_size = 16,
  labs_col = NULL,
  legend = TRUE,
  legend_col = NULL,
  sep_col = NULL,
  margin = list(b = 50, l = 50, t = 50, r = 20),
  padding = 0,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1
)
}
\arguments{
\item{x}{data.frame: Input: Either a) 1 numeric column with categories defined by rownames, or
b) two columns, the first is category names, the second numeric or c) a numeric vector with categories defined using
the \code{category.names} argument.}

\item{main}{Character: Plot title. Default = NULL, which results in \code{colnames(x)[1]}.}

\item{xlab}{Character: x-axis label.}

\item{ylab}{Character: y-axis label.}

\item{col}{Character: Colors for the pie slices.}

\item{alpha}{Numeric: Alpha for the pie slices.}

\item{bg}{Character: Background color.}

\item{plot_bg}{Character: Plot background color.}

\item{theme}{Theme object.}

\item{palette}{Character: Color palette to use.}

\item{category_names}{Character, vector, length = NROW(x): Category names. Default = NULL, which uses
either \code{rownames(x)}, or the first column of \code{x} if \code{ncol(x) = 2}.}

\item{textinfo}{Character: Info to show over each slice: "label", "percent", "label+percent".}

\item{font_size}{Integer: Font size for labels.}

\item{labs_col}{Character: Color of labels.}

\item{legend}{Logical: If TRUE, show legend.}

\item{legend_col}{Character: Color for legend.}

\item{sep_col}{Character: Separator color.}

\item{margin}{List: Margin settings.}

\item{padding}{Numeric: Padding between cells.}

\item{displayModeBar}{Logical: If TRUE, display the plotly mode bar.}

\item{modeBar_file_format}{Character: File format for image exports from the mode bar.}

\item{filename}{Character: File name to save plot.}

\item{file_width}{Integer: Width for saved file.}

\item{file_height}{Integer: Height for saved file.}

\item{file_scale}{Numeric: Scale for saved file.}
}
\value{
\code{plotly} object.
}
\description{
Draw interactive pie charts using \code{plotly}.
}
\examples{
\dontrun{
draw_pie(VADeaths[, 1, drop = FALSE])
}
}
\author{
EDG
}
