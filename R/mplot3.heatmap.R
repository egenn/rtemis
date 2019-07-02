# mplot3.heatmap.R
# ::rtemis::
# 2016-9 Efstathios D. Gennatas egenn.github.io
# TODO: check dendrogram: compare to heatmap: same hierarchy, different layout, not wrong
# TODO: only center around zero (if autorange) if 0 is within range of z,
#       otherwise range from 0 to max or closest multiple of ten, each depending on range.
# When specifying zlim, make values below zlim[1] same color as zlim[1] and values above zlim[2]
# same color as zlim[2]

#' \code{mplot3} Heatmap (\code{image}; modified \code{heatmap})
#'
#' Customized heatmap with optional colorbar
#'
#' The main difference from the original \code{heatmap} is the addition of a colorbar on the side.
#' This is achieved with \link{colorGrad}.
#' Other differences:
#' - Dendrograms are not drawn by default. Set \code{Rowv = T} and \code{Colv = T} to get them.
#' - Column labels are only drawn perpendicular to the x-axis if any one is
#'   longer than two characters.
#'
#' @param colorGrad.n Integer. Number of distinct colors to generate using \link{colorGrad}
#' @param colorGrad.col Color. See \link{colorGrad}
#' @param colorbar Logical. Should colorbar appear next to heatmap?
#' @param cb.n Integer. Number of steps in colorbar. 21 gives 10 above and 10 below midline.
#'   If midline is zero, this corresponds to 10 percent increments / decrements
#' @author Efstathios D Gennatas modified from original \code{stats::heatmap}
#' by Andy Liaw, R. Gentleman, M. Maechler, W. Huber
#' @export

mplot3.heatmap <- function(x,
                           colorGrad.n = 101,
                           colorGrad.col = NULL,
                           lo = "#18A3AC",
                           lomid = NULL,
                           mid = NULL,
                           midhi = NULL,
                           hi = "#F48024",
                           space = "rgb",
                           theme = getOption("rt.theme", "light"),
                           colorbar = TRUE,
                           cb.n = 21,
                           cb.title = NULL,
                           cb.cex = 1,
                           cb.title.cex = 1,
                           cb.add.mar = NULL,
                           Rowv = NA, # was NA
                           Colv = if (symm) Rowv else NA,
                           distfun = dist,
                           hclustfun = hclust,
                           reorderfun = function(d, w) reorder(d, w),
                           add.expr,
                           symm = FALSE,
                           revC = identical(Colv, "Rowv"),
                           # scale = c("row", "column", "none"),
                           scale = "none",
                           na.rm = TRUE,
                           margins = NULL,
                           ColSideColors,
                           RowSideColors,
                           cexRow = 0.2 + 1/log10(nr),
                           cexCol = 0.2 + 1/log10(nc),
                           labRow = NULL,
                           labCol = NULL,
                           labCol.las = NULL, # rtemis
                           main = "", # affects subsequent plot (CB); never NULL to keep streamlined
                           main.adj = 0,
                           main.line = NA,
                           xlab = NULL,
                           ylab = NULL,
                           xlab.line = NULL,
                           ylab.line = NULL,
                           bg = NULL,
                           col.axis = NULL,
                           keep.dendro = FALSE,
                           trace = 0,
                           zlim = NULL,                     # rtemis
                           autorange = TRUE,
                           filename = NULL,
                           par.reset = TRUE,
                           pdf.width = 7,
                           pdf.height = 7, ...) {

  # [ THEMES ] ====
  theme <- ifelse(substr(theme, 1, 5) == "light", "light", "dark")
  if (theme == "light") {
    if (is.null(bg)) bg = "white"
    if (is.null(mid)) mid <- "white"
    if (is.null(col.axis)) col.axis <- "black"
  } else {
    if (is.null(bg)) bg = "black"
    if (is.null(mid)) mid <- "black"
    if (is.null(col.axis)) col.axis <- "white"
  }

  # [ AUTOMARGINS ] ====
  if (is.null(margins)) {
    names.nchar <- nchar(c(colnames(x), rownames(x)))
    max.nchar <- max(0, names.nchar)
    margins <- rep(2.4 + max.nchar * .3, 2)
  }
  # [ COL ] ====
  col <- colorGrad(n = colorGrad.n,
                   colors = colorGrad.col,
                   space = space,
                   lo = lo,
                   lomid = lomid,
                   mid = mid,
                   midhi = midhi,
                   hi = hi)

  # [ ZLIM ] ====
  if (is.null(zlim)) {
    if (autorange) {
      max.z <- max(abs(x))
      zlim <- c(-max.z, max.z)
    } else {
      zlim <- range(x)
    }
  }

  scale <- if (symm && missing(scale)) "none" else match.arg(scale)
  if (length(di <- dim(x)) != 2 || !is.numeric(x))
    stop("'x' must be a numeric matrix")
  nr <- di[1L]
  nc <- di[2L]
  if (nr <= 1 || nc <= 1)
    stop("'x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2L)
    stop("'margins' must be a numeric vector of length 2")
  doRdend <- !identical(Rowv, NA)
  doCdend <- !identical(Colv, NA)
  if (!doRdend && identical(Colv, "Rowv"))
    doCdend <- FALSE
  if (is.null(Rowv))
    Rowv <- rowMeans(x, na.rm = na.rm)
  if (is.null(Colv))
    Colv <- colMeans(x, na.rm = na.rm)
  if (doRdend) {
    if (inherits(Rowv, "dendrogram"))
      ddr <- Rowv
    else {
      hcr <- hclustfun(distfun(x))
      ddr <- as.dendrogram(hcr)
      if (!is.logical(Rowv) || Rowv)
        ddr <- reorderfun(ddr, Rowv)
    }
    if (nr != length(rowInd <- order.dendrogram(ddr)))
      stop("row dendrogram ordering gave index of wrong length")
  } else rowInd <- 1L:nr
  if (doCdend) {
    if (inherits(Colv, "dendrogram"))
      ddc <- Colv
    else if (identical(Colv, "Rowv")) {
      if (nr != nc)
        stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
      ddc <- ddr
    }
    else {
      hcc <- hclustfun(distfun(if (symm) x else t(x)))
      ddc <- as.dendrogram(hcc)
      if (!is.logical(Colv) || Colv)
        ddc <- reorderfun(ddc, Colv)
    }
    if (nc != length(colInd <- order.dendrogram(ddc)))
      stop("column dendrogram ordering gave index of wrong length")
  } else colInd <- 1L:nc
  x <- x[rowInd, colInd]
  labRow <- if (is.null(labRow)) {
    if (is.null(rownames(x))) {
      (1L:nr)[rowInd]
    } else rownames(x)
  } else labRow[rowInd]

  labCol <- if (is.null(labCol)) {
    if (is.null(colnames(x))) {
      (1L:nc)[colInd]
    } else colnames(x)
  } else labCol[colInd]

  # rtemis
  labCol.las <- if (is.null(labCol.las)) if (max(nchar(labCol)) > 2) 2 else 0

  if (scale == "row") {
    x <- sweep(x, 1L, rowMeans(x, na.rm = na.rm), check.margin = FALSE)
    sx <- apply(x, 1L, sd, na.rm = na.rm)
    x <- sweep(x, 1L, sx, "/", check.margin = FALSE)
  } else if (scale == "column") {
    x <- sweep(x, 2L, colMeans(x, na.rm = na.rm), check.margin = FALSE)
    sx <- apply(x, 2L, sd, na.rm = na.rm)
    x <- sweep(x, 2L, sx, "/", check.margin = FALSE)
  }
  lmat <- rbind(c(NA, 3), 2:1)
  lwid <- c(if (doRdend) 1 else 0.05, 4)
  lhei <- c((if (doCdend) 1 else 0.05) + if (!is.null(main)) 0.2 else 0, 4)
  if (!missing(ColSideColors)) {
    if (!is.character(ColSideColors) || length(ColSideColors) !=
        nc)
      stop("'ColSideColors' must be a character vector of length ncol(x)")
    lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
    lhei <- c(lhei[1L], 0.2, lhei[2L])
  }
  if (!missing(RowSideColors)) {
    if (!is.character(RowSideColors) || length(RowSideColors) !=
        nr)
      stop("'RowSideColors' must be a character vector of length nrow(x)")
    lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1),
                                   1), lmat[, 2] + 1)
    lwid <- c(lwid[1L], 0.2, lwid[2L])
  }
  lmat[is.na(lmat)] <- 0

  if (colorbar) lmat <- cbind(lmat, c(0, 4)) # rtemis adding a column for colorbar

  # [ COLORBAR ] ====
  # if (colorbar) lwid <- c(lwid, .4) # rtemis
  if (colorbar) {
    if (is.na(Colv)) {
      lwid <- c(lwid, 1)
    } else {
      lwid <- c(lwid, 1) # rtemis
    }
  }

  if (trace > 0) {
    cat("layout: widths = ", lwid, ", heights = ", lhei, "; lmat=\n")
    print(lmat)
  }
  dev.hold()
  on.exit(dev.flush())

  # [ par ] ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  op <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(op)))

  layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
  if (!missing(RowSideColors)) {
    par(mar = c(margins[1L], 0, 0, 0), bg = bg) # orig c(margins[1L], 0, 0, 0.5)
    image(rbind(if (revC)
      nr:1L
      else 1L:nr), col = RowSideColors[rowInd], axes = FALSE)
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2L]), bg = bg)
    image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
  }
  par(mar = c(margins[1L], 0, 0, margins[2L]), bg = bg)
  if (!symm || scale != "none")
    x <- t(x)
  if (revC) {
    iy <- nr:1
    if (doRdend)
      ddr <- rev(ddr)
    x <- x[, iy]
  }
  else iy <- 1L:nr
  image(1L:nc, 1L:nr, x,
        xlim = 0.5 + c(0, nc),
        ylim = 0.5 + c(0, nr),
        axes = FALSE,
        xlab = "",
        ylab = "",
        col = col,
        zlim = zlim, ...)
  axis(1, 1L:nc, labels = labCol, las = labCol.las, line = -0.5, tick = 0,
       cex.axis = cexCol, col.axis = col.axis)
  if (!is.null(xlab)) {
    if (is.null(xlab.line)) xlab.line <- margins[1L] - 1.25
    msg("xlab is", xlab,"margins is", margins," and xlab.line is", xlab.line)
    mtext(xlab, side = 1, line = xlab.line)
  }

  axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0,
       cex.axis = cexRow, col.axis = col.axis)
  if (!is.null(ylab)) {
    if (is.null(ylab.line)) ylab.line <- margins[2L] - 1.25
    mtext(ylab, side = 4, line = ylab.line)
  }

  if (!missing(add.expr))
    eval.parent(substitute(add.expr))

  # [ PLOT ] ====
  par(mar = c(margins[1L], 0, 0, 0), bg = bg)
  if (doRdend)
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none",
         edgePar = list(col = col.axis))
  else frame()
  par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2L]), bg = bg)
  if (doCdend)
    plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none",
         edgePar = list(col = col.axis))
  else if (!is.null(main))
    frame()
  if (!is.null(main)) {
    par(xpd = NA, mar = c(0, 0, 0, 0), bg = bg)
    title(main, cex.main = 1.5 * op[["cex.main"]], adj = main.adj, line = main.line)
  }

  # [ COLORBAR ] ====
  if (colorbar) {
    frame()
    # par(xpd = NA)
    if (is.na(Colv) & is.null(main)) {
      if (is.null(cb.add.mar)) cb.add.mar <- c(margins[1], 2, 0, 3)
      cb.add.new <- FALSE
    } else {
      if (is.null(cb.add.mar)) cb.add.mar <- c(margins[1], 0, 0, 3)
      cb.add.new <- TRUE
    }
    colorGrad(colorGrad.n,
              colors = colorGrad.col,
              lo = lo,
              lomid = lomid,
              mid = mid,
              midhi = midhi,
              hi = hi,
              colorbar = TRUE,
              cb.n = cb.n,
              par.reset = TRUE,
              # cb.add = T,
              cex = cb.cex, #1.6,
              cb.axis.pos = 1.2,
              cb.add = cb.add.new,
              cb.add.mar = cb.add.mar,
              bar.min = ddSci(zlim[1]),
              bar.mid = ddSci((zlim[1] + zlim[2]) / 2),
              bar.max = ddSci(zlim[2]),
              col.text = col.axis)

    if (!is.null(cb.title)) mtext(text = cb.title, cex = cb.title.cex)
  }

  if (!is.null(filename)) dev.off()

  # Return if assigned
  invisible(list(rowInd = rowInd,
                 colInd = colInd,
                 Rowv = if (keep.dendro && doRdend) ddr,
                 Colv = if (keep.dendro && doCdend) ddc))

} # rtemis::mplot3.heatmap
