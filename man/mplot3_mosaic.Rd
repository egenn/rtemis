% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplot3_mosaic.R
\name{mplot3_mosaic}
\alias{mplot3_mosaic}
\title{Mosaic plot}
\usage{
mplot3_mosaic(
  x,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  border = FALSE,
  theme = rtTheme,
  theme.args = list(),
  palette = rtPalette,
  mar = NULL,
  oma = rep(0, 4),
  par.reset = TRUE,
  new = FALSE,
  autolabel = letters,
  filename = NULL,
  pdf.width = 5,
  pdf.height = 5,
  ...
)
}
\arguments{
\item{x}{contingency table, e.g. output of \code{table()}}

\item{main}{Character: Main title}

\item{xlab}{Character: x-axis label}

\item{ylab}{Character: y-axis label}

\item{border}{Color vector for cell borders or FALSE to turn off. Default = FALSE}

\item{theme}{Character: Run \code{themes()} for available themes}

\item{theme.args}{List of arguments to pass to \code{theme}. Optional, same args can be passed
to theme function}

\item{palette}{Vector of colors, or Character defining a builtin palette - get options with
\code{rtpalette()}}

\item{new}{Logical: If TRUE, add plot to existing plot. See \code{par("new")}}

\item{filename}{Character: Path to file to save plot. Default = NULL}

\item{pdf.width}{Float: Width in inches for PDF output, if \code{filename} is defined}

\item{pdf.height}{Float: Height in inches for PDF output, if \code{filename} is defined}
}
\description{
Plots a mosaic plot using \code{graphics::mosaicplot}
}
\examples{
\dontrun{
party <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(party) <- list(gender = c("F", "M"),
                        party = c("Democrat","Independent", "Republican"))
mplot3_mosaic(party)
}
}
\author{
E.D. Gennatas
}
