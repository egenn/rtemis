# dplot3.heatmap.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io
# TODO: automatically adjust (lower) margin depending on label length

#' Interactive Heatmaps
#'
#' Draw interactive heatmaps using \code{heatmaply}
#'
#' @inheritParams colorGrad
#' @inheritParams mplot3.xy
#' @param z Input matrix
#' @param Rowv Logical or dendrogram.
#'   If Logical: Compute dendrogram and reorder rows. Defaults to FALSE
#'   If dendrogram: use as is, without reordering
#'   See more at \code{heatmaply::heatmaply("Rowv")}
#' @param Colv Logical or dendrogram.
#'   If Logical: Compute dendrogram and reorder columns. Defaults to FALSE
#'   If dendrogram: use as is, without reordering
#'   See more at \code{heatmaply::heatmaply("Colv")}
#' @param cluster Logical: If TRUE, set \code{Rowv} and \code{Colv} to TRUE
#' @param symm Logical: If TRUE, treat \code{x} symmetrically - \code{x} must be a square matrix. Default = FALSE
#' @param cellnote Matrix with values to be desplayed on hover. Defaults to \code{ddSci(z)}
#' @param k.row Integer: Number of desired number of groups by which to color dendrogram branches in the rows.
#' Default = NA (determined automatically). See \code{heatmaply::heatmaply("k_row")}
#' @param k.col Integer: Number of desired number of groups by which to color dendrogram branches in the columns.
#' Default = NA (determined automatically). See \code{heatmaply::heatmaply("k_col")}
#' @param grid.gap Integer: Space between cells. Default = 0 (no space)
#' @param limits Float, length 2: Determine color range. Default = NULL, which automatically centers values around 0
#' @param margins Float, length 4: Heatmap margins. Default = c(30, 30, 30, 30)
#' @param key.title Character: Title for the color key. Default = NULL (no title)
#' @param ... Additional arguments to be passed to \code{heatmaply::heatmaply}
#' @author Efstathios D. Gennatas
#' @export

dplot3.heatmap <- function(z,
                        Rowv = TRUE,
                        Colv = TRUE,
                        cluster = FALSE,
                        symm = FALSE,
                        cellnote = NULL,
                        colorGrad.n = 101,
                        colors = NULL,
                        space = "rgb",
                        lo = "#18A3AC",
                        lomid = NULL,
                        mid = "white",
                        midhi = NULL,
                        hi = "#F48024",
                        k.row = NA,
                        k.col = NA,
                        # show_grid = FALSE,
                        grid.gap = 0,
                        limits = NULL,
                        # margins = c(50, 50, 50, 50),
                        margins = NULL,
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        key.title = NULL,
                        showticklabels = NULL,
                        ...) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("heatmaply", verbose = FALSE)) {
    cat("\n")
    stop("Please install dependencies and try again")
  }

  # [ COLNAMES ] ====
  if (is.null(colnames(z))) colnames(z) <- 1:NCOL(z) # rtLetters(NCOL(z))
  if (is.null(rownames(z))) rownames(z) <- 1:NROW(z) # rtLetters(NCOL(z), caps = TRUE)

  # [ MARGINS ] ====
  # By default, allow 7 px per character
  if (is.null(margins)) {
    bottom <- max(nchar(colnames(z))) * 7 + 15
    left <- max(nchar(rownames(z))) * 7 + 10
    margins <- c(bottom, left, 50, 50)
  }

  # [ TICK LABELS ] ====
  if (is.null(showticklabels)) {
    showticklabels <- c(ifelse(NCOL(z) < 50, TRUE, FALSE),
                        ifelse(NROW(z) < 50, TRUE, FALSE))
  }

  # [ LIMITS ] ====
  if (is.null(limits)) {
    maxabs <- max(abs(z))
    if (.2 < maxabs & maxabs < 1) maxabs <- 1
    limits <- c(-maxabs, maxabs)
  }

  # [ COLORS ] ====
  colors <- colorGrad(n = colorGrad.n,
                      colors = colors,
                      space = space,
                      lo = lo,
                      lomid = lomid,
                      mid = mid,
                      midhi = midhi,
                      hi = hi)

  # [ CLUSTER ] ====
  if (cluster) Rowv <- Colv <- TRUE

  # [ HOVER INFO ] ====
  if (!is.null(cellnote)) {
    if (cellnote == "values") cellnote <- matrix(ddSci(z), NROW(z), NCOL(z))
  }

  # [ HEATMAP ] ====
  heatmaply::heatmaply(z, Rowv = Rowv, Colv = Colv,
                       symm = symm,
                       cellnote = cellnote,
                       colors = colors,
                       grid_gap = grid.gap,
                       limits = limits,
                       margins = margins,
                       key.title = key.title,
                       xlab = xlab,
                       ylab = ylab,
                       main = main,
                       k_row = k.row,
                       k_col = k.col,
                       showticklabels = showticklabels,
                       ...)

} # rtemis::dplot3.heatmap
