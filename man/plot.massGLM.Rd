% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/massGLM.R
\name{plot.massGLM}
\alias{plot.massGLM}
\title{Plot \code{massGLM} object}
\usage{
\method{plot}{massGLM}(
  x,
  predictor = NULL,
  main = NULL,
  what = c("adjusted", "raw", "coef"),
  p.adjust.method = "holm",
  pval.hline = 0.05,
  hline.col = "#FE4AA3",
  hline.dash = "dash",
  theme = getOption("rt.theme", "lightgrid"),
  displayModeBar = FALSE,
  ...
)
}
\arguments{
\item{x}{\code{massGLM} object}

\item{what}{Character: "adjusted" or "raw" p-values to plot}
}
\description{
Plots a \code{massGLM} object using \link{dplot3.bar}
}
\author{
E.D. Gennatas
}
