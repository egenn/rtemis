% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplot3.pvals.R
\name{dplot3.pvals}
\alias{dplot3.pvals}
\title{Barplot p-values using \link{dplot3.bar}}
\usage{
dplot3.pvals(
  x,
  xnames = NULL,
  yname = NULL,
  p.adjust.method = "none",
  pval.hline = 0.05,
  hline.col = "#FE4AA3",
  hline.dash = "dash",
  ...
)
}
\arguments{
\item{x}{Float, vector: p-values}

\item{xnames}{Character, vector: feature names}

\item{yname}{Character: outcome name}

\item{p.adjust.method}{Character: method for \link{p.adjust}. Default = "none"}

\item{pval.hline}{Float: Significance level at which to plot horizontal line. Default = .05}

\item{hline.col}{Color for \code{pval.hline}. Default = "#FE4AA3"}

\item{hline.dash}{Character: type of line to draw. Default = "dash"}
}
\description{
Plot 1 - p-values as a barplot
}
\author{
E.D. Gennatas
}
