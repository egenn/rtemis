% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_volcano.R
\name{draw_volcano}
\alias{draw_volcano}
\title{Volcano Plot}
\usage{
draw_volcano(
  x,
  pvals,
  xnames = NULL,
  group = NULL,
  x_thresh = 0,
  p_thresh = 0.05,
  p_adjust_method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
    "none"),
  p_transform = function(x) -log10(x),
  legend = NULL,
  legend_lo = NULL,
  legend_hi = NULL,
  label_lo = "Low",
  label_hi = "High",
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  xlim = NULL,
  ylim = NULL,
  alpha = NULL,
  hline = NULL,
  hline_col = NULL,
  hline_width = 1,
  hline_dash = "solid",
  hline_annotate = NULL,
  hline_annotation_x = 1,
  theme = choose_theme(),
  annotate = TRUE,
  annotate_col = theme[["labs_col"]],
  font_size = 16,
  palette = NULL,
  legend_x_lo = NULL,
  legend_x_hi = NULL,
  legend_y = 0.97,
  annotate_n = 7L,
  ax_lo = NULL,
  ay_lo = NULL,
  ax_hi = NULL,
  ay_hi = NULL,
  annotate_alpha = 0.7,
  hovertext = NULL,
  displayModeBar = "hover",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  verbosity = 1L,
  ...
)
}
\arguments{
\item{x}{Numeric vector: Input values, e.g. log2 fold change, coefficients, etc.}

\item{pvals}{Numeric vector: p-values.}

\item{xnames}{Character vector: \code{x} names.}

\item{group}{Factor: Used to color code points. If NULL, significant points
below \code{x_thresh}, non-significant points, and significant points
above \code{x_thresh} will be plotted with the first, second and third
color of \code{palette}.}

\item{x_thresh}{Numeric x-axis threshold separating low from high.}

\item{p_thresh}{Numeric: p-value threshold of significance.}

\item{p_adjust_method}{Character: p-value adjustment method.
"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
Default = "holm". Use "none" for raw p-values.}

\item{p_transform}{function.}

\item{legend}{Logical: If TRUE, show legend. Will default to FALSE, if
\code{group = NULL}, otherwise to TRUE.}

\item{legend_lo}{Character: Legend to annotate significant points below the
\code{x_thresh}.}

\item{legend_hi}{Character: Legend to annotate significant points above the
\code{x_thresh}.}

\item{label_lo}{Character: label for low values.}

\item{label_hi}{Character: label for high values.}

\item{main}{Character: Main title.}

\item{xlab}{Character: x-axis label.}

\item{ylab}{Character: y-axis label.}

\item{margin}{Named list of plot margins.
Default = \code{list(b = 65, l = 65, t = 50, r = 10, pad = 0)}.}

\item{xlim}{Numeric vector, length 2: x-axis limits.}

\item{ylim}{Numeric vector, length 2: y-axis limits.}

\item{alpha}{Numeric: point transparency.}

\item{hline}{Numeric: If defined, draw a horizontal line at this y value.}

\item{hline_col}{Color for \code{hline}.}

\item{hline_width}{Numeric: Width for \code{hline}.}

\item{hline_dash}{Character: Type of line to draw: "solid", "dot", "dash",
"longdash", "dashdot", or "longdashdot".}

\item{hline_annotate}{Character: Text of horizontal line annotation if
\code{hline} is set.}

\item{hline_annotation_x}{Numeric: x position to place annotation with paper
as reference. 0: to the left of the plot area; 1: to the right of the plot area.}

\item{theme}{Theme object.}

\item{annotate}{Logical: If TRUE, annotate significant points.}

\item{annotate_col}{Color for annotations.}

\item{font_size}{Integer: Font size.}

\item{palette}{Character: Name of \pkg{rtemis} palette to use.}

\item{legend_x_lo}{Numeric: x position of \code{legend_lo}.}

\item{legend_x_hi}{Numeric: x position of \code{legend_hi}.}

\item{legend_y}{Numeric: y position for \code{legend_lo} and \code{legend_hi}.}

\item{annotate_n}{Integer: Number of significant points to annotate.}

\item{ax_lo}{Numeric: Sets the x component of the arrow tail about the arrow head for
significant points below \code{x_thresh}.}

\item{ay_lo}{Numeric: Sets the y component of the arrow tail about the arrow head for
significant points below \code{x_thresh}.}

\item{ax_hi}{Numeric: Sets the x component of the arrow tail about the arrow head for
significant points above \code{x_thresh}.}

\item{ay_hi}{Numeric: Sets the y component of the arrow tail about the arrow head for
significant points above \code{x_thresh}.}

\item{annotate_alpha}{Numeric: Transparency for annotations.}

\item{hovertext}{Character vector: Text to display on hover.}

\item{displayModeBar}{Logical: If TRUE, display plotly mode bar.}

\item{filename}{Character: Path to save the plot image.}

\item{file_width}{Numeric: Width of the saved plot image.}

\item{file_height}{Numeric: Height of the saved plot image.}

\item{file_scale}{Numeric: Scale of the saved plot image.}

\item{verbosity}{Integer: Verbosity level.}

\item{...}{Additional parameters passed to \link{draw_scatter}.}
}
\value{
\code{plotly} object.
}
\description{
Volcano Plot
}
\examples{
\dontrun{
set.seed(2019)
x <- rnormmat(500, 500)
y <- x[, 3] + x[, 5] - x[, 9] + x[, 15] + rnorm(500)
mod <- massGLM(y, x)
draw_volcano(mod$summary$`Coefficient y`, mod$summary$`p_value y`)
}

}
\author{
EDG
}
