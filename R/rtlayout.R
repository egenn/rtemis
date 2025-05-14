# rtlayout.R
# ::rtemis::
# 2018 EDG rtemis.org

#' Create multipanel plots with the `mplot3` family
#'
#' Set layout for drawing multiple plots in the same view
#'
#' @param nrows Integer: N of rows
#' @param ncols Integer: N of columns
#' @param byrow Logical: If TRUE, draw add plots by row Default = FALSE
#' @param autolabel Logical: If TRUE, place letter labels on the top left corner of each figure. Default = FALSE
#' @param pdf.width Float: Width of PDF to save, if `filename` is provided.
#' Default = `ncols * 4.5`
#' @param pdf.height Float: Height of PDF to save, if `filename` is provided.
#' Default = `nrows * 4.5`
#' @param filename String, optional: Save multiplot to file. Default = NULL
#'
#' @author E.D. Gennatas
#' @export

rtlayout <- function(
  nrows = NULL,
  ncols = NULL,
  byrow = FALSE,
  autolabel = FALSE,
  pdf.width = NULL,
  pdf.height = NULL,
  filename = NULL
) {
  if (!is.null(nrows) || !is.null(ncols)) {
    if (is.null(nrows)) nrows <- 1
    if (is.null(ncols)) ncols <- 1
    rtpar <- par(no.readonly = TRUE)
    assign("rtpar", rtpar, envir = rtenv)
    if (autolabel) rtenv$autolabel <- 1
    npanels <- nrows * ncols
    if (!is.null(filename)) {
      if (is.null(pdf.width)) pdf.width = ncols * 4.5
      if (is.null(pdf.height)) pdf.height = nrows * 4.5
      pdf(filename, width = pdf.width, height = pdf.height)
      rtenv$fileOpen <- TRUE
    }
    layout(matrix(seq(npanels), nrows, ncols, byrow = byrow))
  } else {
    if (is.null(rtenv$rtpar))
      stop("rtlayout was not previously run in this session")
    par(rtenv$rtpar)
    rm(rtpar, envir = rtenv)
    if (exists("autolabel", envir = rtenv)) rm(autolabel, envir = rtenv)
    if (exists("fileOpen", envir = rtenv)) dev.off()
  }
} # rtemis::rtlayout
