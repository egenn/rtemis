% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplot3_fret.R
\name{mplot3_fret}
\alias{mplot3_fret}
\title{\code{mplot3}: Guitar Fretboard}
\usage{
mplot3_fret(
  theme = rtTheme,
  useSharps = FALSE,
  strings.col = "auto",
  frets.col = "auto",
  inlays = TRUE,
  inlays.col = "auto",
  inlays.cex = 2,
  par.reset = TRUE,
  ...
)
}
\arguments{
\item{theme}{Character: "light" or "dark"}

\item{useSharps}{Logical: If TRUE, draw sharp instead of flat notes. Default = FALSE}

\item{strings.col}{Color for strings}

\item{frets.col}{Color for frets}

\item{inlays}{Logical: Draw fretboard inlays. Default = TRUE}

\item{inlays.col}{Color for inlays}

\item{inlays.cex}{Numeric: Character expansion factor for inlays. Default = 2}

\item{par.reset}{Logical: If TRUE, reset par before exit}

\item{...}{Additional arguments to theme}
}
\description{
Draw color-coded notes on a guitar fretboard for standard E-A-D-G-B-e tuning
}
\details{
Plot is very wide and short. Adjust plot window accordingly.
Practice every day.
}
\author{
E.D. Gennatas
}
