% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplot3_table.R
\name{dplot3_table}
\alias{dplot3_table}
\title{Simple HTML table}
\usage{
dplot3_table(
  x,
  .ddSci = TRUE,
  main = NULL,
  main.col = "black",
  main.x = 0,
  main.xanchor = "auto",
  fill.col = "#18A3AC",
  table.bg = "white",
  bg = "white",
  line.col = "white",
  lwd = 1,
  header.font.col = "white",
  table.font.col = "gray20",
  font.size = 14,
  font.family = "Helvetica Neue",
  margin = list(l = 0, r = 5, t = 30, b = 0, pad = 0)
)
}
\arguments{
\item{x}{data.frame: Table to draw}

\item{.ddSci}{Logical: If TRUE, apply \link{ddSci} to numeric columns.}

\item{main}{Character: Table tile.}

\item{main.col}{Color: Title color.}

\item{main.x}{Float [0, 1]: Align title: 0: left, .5: center, 1: right.}

\item{main.xanchor}{Character: "auto", "left", "right": plotly's layout xanchor for
title. Default = "auto"}

\item{fill.col}{Color: Used to fill header with column names and first column with
row names.}

\item{table.bg}{Color: Table background.}

\item{bg}{Color: Background.}

\item{line.col}{Color: Line color.}

\item{lwd}{Float: Line width. Default = 1}

\item{header.font.col}{Color: Header font color.}

\item{table.font.col}{Color: Table font color.}

\item{font.size}{Integer: Font size.}

\item{font.family}{Character: Font family.}

\item{margin}{List: plotly's margins.}
}
\description{
Draw an html table using \code{plotly}
}
\author{
E.D. Gennatas
}
