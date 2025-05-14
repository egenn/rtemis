# mplot3_heatmap.R
# ::rtemis::
# 2016-9 E.D. Gennatas rtemis.org
# consider: only center around zero (if autorange) if 0 is within range of z,
#       otherwise range from 0 to max or closest multiple of ten, each depending on range.
# When specifying zlim, make values below zlim[1] same color as zlim[1] and values above zlim[2]
# same color as zlim[2]

#' `mplot3` Heatmap (`image`; modified `heatmap`)
#'
#' Customized heatmap with optional colorbar
#'
#' The main difference from the original `stats::heatmap` is the addition of a colorbar on the side.
#' This is achieved with [colorGrad].
#' Other differences:
#' - Dendrograms are not drawn by default. Set `Rowv = T` and `Colv = T` to get them.
#' - Column labels are only drawn perpendicular to the x-axis if any one is
#'   longer than two characters.
#' Otherwise, the arguments are the same as in `stats::heatmap`
#'
#' @param x Input matrix
#' @param colorGrad.n Integer: Number of distinct colors to generate using [colorGrad]. Default = 101
#' @param colorGrad.col Character: the `colors` argument of [colorGrad]: Character: Acts as a shortcut to defining
#' lo, mid, etc for a number of defaults: "french", "penn", "grnblkred"
#' @param lo Color for low end
#' @param lomid Color for low-mid
#' @param mid Color for middle of the range or "mean", which will result in colorOp(c(lo, hi), "mean"). If mid = NA,
#' then only lo and hi are used to create the color gradient.
#' @param midhi Color for middle-high
#' @param hi Color for high end
#' @param space Character: Which colorspace to use. Option: "rgb", or "Lab". Default = "rgb". Recommendation: If mid is
#' "white" or "black" (default), use "rgb", otherwise "Lab"
#' @param theme Character: Defaults to option "rt.theme", if set, otherwise "light"
#' @param colorbar Logical: If TRUE, plot colorbar next to heatmap. Default = TRUE
#' @param cb.n Integer: Number of steps in colorbar. Default = 21, which gives 10 above and 10 below midline.
#' If midline is zero, this corresponds to 10 percent increments / decrements
#' @param cb.title Character: Title for the colorbar. Default = NULL
#' @param cb.cex Float: Character expansion (`cex`) for colobar. Default = 1
#' @param cb.title.cex Float: `cex` for colorbar title. Default = 1
#' @param cb.mar Float, vector, length 4: Margins for colorbar.  (passed to [colorGrad]'s `cb.add.mar`).
#' Default set automatically
#' @param Rowv Logical OR a dendrogram OR integer vector that determines index for reordering OR NA to suppress.
#' Default = TRUE
#' @param Colv See `Rowv`
#' @param distfun Function: used to compute the distance/dissimilarity matrix between rows and columns.
#' Default = `dist`
#' @param hclustfun Function: used to determined hierarchical clustering when `Rowv` or `Colv` are
#' not dendrograms. Default = `hclust` (Should take as argument a result of distfun and return an object to which
#' `as.dendrogram` can be applied)
#' @param reorderfun Function (d, w): function of dendrogram and weights that determines reordering of row and column
#' dendrograms. Default uses `reorder.dendrogram`
#' @param add.expr Expression: will be evaluated after the call to `image`. Can be used to add components to the
#' plot
#' @param symm Logical: If TRUE, treat `x` symmetrically. Can only be used if `x` is square
#' @param revC Logical: If TRUE, reverse column order for plotting. Default = TRUE, if Rowv and Colv are identical
#' @param scale Character: "row", "column", or "none". Determines whether values are centered and scaled in either the
#' row or column  direction. Default = "none"
#' @param na.rm Logical: If TRUE, NAs are removed. Default = TRUE
#' @param margins Float, vector, length 2: bottom and right side margins. Automatically determined by length of
#' variable names
#' @param ColSideColors Color, vector, length = ncol(x): Colors for a horizontal side bar to annotate columns of `x`
#' @param RowSideColors Color, vector, length = nrow(x): Like `ColSideColors`, but for rows
#' @param cexRow Float: `cex.axis` for rows
#' @param cexCol Float: `cex.axis` for columns
#' @param labRow Character, vector: Row labels to use. Default = `rownames(x)`
#' @param labCol Character, vector: Column labels to use. Default = `colnames(x)`
#' @param labCol.las Integer {0:3}: `par`'s `las` argument. Default set by length of `labCol`
#' @param main Character: Plot title
#' @param main.adj Float: `par`'s `adj` argument for title
#' @param main.line Float: `title`'s `line` argument
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param xlab.line Float: `mtext`'s `line` argument for x-axis label
#' @param ylab.line Float: `mtext`'s `line` argument for y-axis label
#' @param keep.dendro Logical: If TRUE, dedrogram is returned invisibly. Default = FALSE
#' @param trace Integer: If > 0, print diagnostic messages to console. Default = 0
#' @param zlim Float, vector, length 2: Passed to `graphics::image`. Default = +/- max(abs(x)) if `autorange = TRUE`,
#' otherwise = `range(x)`.
#' @param autorange Logical: See `zlim`
#' @param filename Character: If provided, save heatmap to file. Default = NULL
#' @param par.reset Logical: If TRUE, reset `par` before exit. Default = TRUE
#' @param pdf.width Float: Width of PDF output, if `filename` is set
#' @param pdf.height Float: Height of PDF output, if `filename` is set
#' @param ... Additional arguments passed to `graphics::image`
#'
#' @author E.D. Gennatas modified from original `stats::heatmap`
#' by Andy Liaw, R. Gentleman, M. Maechler, W. Huber
#' @examples
#' \dontrun{
#' x <- rnormmat(200, 20)
#' xcor <- cor(x)
#' mplot3_heatmap(xcor)
#' }
#' @export

mplot3_heatmap <- function(
  x,
  colorGrad.n = 101,
  colorGrad.col = NULL,
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  space = "rgb",
  theme = getOption("rt.theme", "white"),
  colorbar = TRUE,
  cb.n = 21,
  cb.title = NULL,
  cb.cex = NULL,
  cb.title.cex = 1,
  cb.mar = NULL,
  Rowv = TRUE,
  Colv = TRUE, # if (symm) Rowv else NA,
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
  group.columns = NULL,
  group.legend = !is.null(group.columns),
  column.palette = rtPalette,
  group.rows = NULL,
  row.palette = rtPalette,
  ColSideColors,
  RowSideColors,
  cexRow = 0.2 + 1 / log10(nr),
  cexCol = 0.2 + 1 / log10(nc),
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
  keep.dendro = FALSE,
  trace = 0,
  zlim = NULL, # rtemis
  autorange = TRUE,
  autolabel = letters,
  filename = NULL,
  par.reset = TRUE,
  pdf.width = 7,
  pdf.height = 7,
  ...
) {
  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  if (is.null(mid)) mid <- theme$bg
  col.axis <- theme$fg
  if (is.null(cb.cex)) cb.cex <- theme$cex

  # Color ----
  col <- colorGrad(
    n = colorGrad.n,
    colors = colorGrad.col,
    space = space,
    lo = lo,
    lomid = lomid,
    mid = mid,
    midhi = midhi,
    hi = hi
  )

  # Row and Col groups ----
  if (!is.null(group.columns) && missing(ColSideColors)) {
    group.columns <- factor(group.columns)
    if (is.character(column.palette))
      column.palette <- rtpalette(column.palette)
    if (length(unique(group.columns)) > length(column.palette))
      stop("Need more colors in column.palette")
    ColSideColors <- unlist(column.palette)[group.columns]
  }

  if (!is.null(group.rows) && missing(RowSideColors)) {
    group.rows <- factor(group.rows)
    if (is.character(row.palette)) row.palette <- rtpalette(row.palette)
    if (length(unique(group.rows)) > length(row.palette))
      stop("Need more colors in column.palette")
    RowSideColors <- unlist(row.palette)[group.rows]
  }

  # zlim ----
  if (is.null(zlim)) {
    if (autorange) {
      max.z <- max(abs(x), na.rm = TRUE)
      zlim <- c(-max.z, max.z)
    } else {
      zlim <- range(x, na.rm = TRUE)
    }
  }

  # Automargins ----
  if (is.null(margins)) {
    # names.nchar <- nchar(c(colnames(x), rownames(x)))
    # max.nchar <- max(0, names.nchar)
    # margins <- rep(1.4 + max.nchar * .45, 2)
    margins <- rep(1.5 + textwidth(c(colnames(x), rownames(x))), 2)
  }

  scale <- if (symm && missing(scale)) "none" else match.arg(scale)
  if (length(di <- dim(x)) != 2 || !is.numeric(x))
    stop("'x' must be a numeric matrix")
  nr <- di[1L]
  nc <- di[2L]
  if (nr <= 1 || nc <= 1) stop("'x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2L)
    stop("'margins' must be a numeric vector of length 2")
  doRdend <- !identical(Rowv, NA)
  doCdend <- !identical(Colv, NA)
  if (!doRdend && identical(Colv, "Rowv")) doCdend <- FALSE
  if (is.null(Rowv)) Rowv <- rowMeans(x, na.rm = na.rm)
  if (is.null(Colv)) Colv <- colMeans(x, na.rm = na.rm)
  if (doRdend) {
    if (inherits(Rowv, "dendrogram")) ddr <- Rowv else {
      hcr <- hclustfun(distfun(x))
      ddr <- as.dendrogram(hcr)
      if (!is.logical(Rowv) || Rowv) ddr <- reorderfun(ddr, Rowv)
    }
    if (nr != length(rowInd <- order.dendrogram(ddr)))
      stop("row dendrogram ordering gave index of wrong length")
  } else rowInd <- 1L:nr
  if (doCdend) {
    if (inherits(Colv, "dendrogram")) ddc <- Colv else if (
      identical(Colv, "Rowv")
    ) {
      if (nr != nc) stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
      ddc <- ddr
    } else {
      hcc <- hclustfun(distfun(if (symm) x else t(x)))
      ddc <- as.dendrogram(hcc)
      if (!is.logical(Colv) || Colv) ddc <- reorderfun(ddc, Colv)
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
    if (!is.character(ColSideColors) || length(ColSideColors) != nc)
      stop("'ColSideColors' must be a character vector of length ncol(x)")
    lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
    lhei <- c(lhei[1L], 0.2, lhei[2L])
  }
  if (!missing(RowSideColors)) {
    if (!is.character(RowSideColors) || length(RowSideColors) != nr)
      stop("'RowSideColors' must be a character vector of length nrow(x)")
    lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1), 1), lmat[, 2] + 1)
    lwid <- c(lwid[1L], 0.2, lwid[2L])
  }
  lmat[is.na(lmat)] <- 0

  # add rtemis colorbar
  if (colorbar) {
    # works with sidecolors
    lmat <- cbind(lmat, c(rep(0, nrow(lmat) - 1), max(lmat) + 1))
    if (is.na(Colv)) {
      lwid <- c(lwid, 1)
    } else {
      lwid <- c(lwid, 1) # rtemis
    }
  }

  # group names
  if (!is.null(group.columns) && group.legend) {
    lmat[1, ncol(lmat)] <- max(lmat) + 1
  }

  if (trace > 0) {
    cat("layout: widths = ", lwid, ", heights = ", lhei, "; lmat=\n")
    print(lmat)
  }
  dev.hold()
  on.exit(dev.flush())

  # par ----
  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE
  op <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(op)))

  layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
  if (!missing(RowSideColors)) {
    par(mar = c(margins[1L], 0, 0, 0.5), bg = theme$bg) # orig c(margins[1L], 0, 0, 0.5)
    image(
      rbind(if (revC) nr:1L else 1L:nr),
      col = RowSideColors[rowInd],
      axes = FALSE
    )
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2L]), bg = theme$bg)
    image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
  }
  par(
    mar = c(margins[1L], 0, 0, margins[2L]),
    bg = theme$bg,
    family = theme$font.family
  )
  if (!symm || scale != "none") x <- t(x)
  if (revC) {
    iy <- nr:1
    if (doRdend) ddr <- rev(ddr)
    x <- x[, iy]
  } else iy <- 1L:nr
  image(
    1L:nc,
    1L:nr,
    x,
    xlim = 0.5 + c(0, nc),
    ylim = 0.5 + c(0, nr),
    axes = FALSE,
    xlab = "",
    ylab = "",
    col = col,
    zlim = zlim,
    ...
  )
  axis(
    1,
    1L:nc,
    labels = labCol,
    las = labCol.las,
    line = -0.5,
    tick = 0,
    cex.axis = cexCol,
    col.axis = col.axis
  )
  if (!is.null(xlab)) {
    if (is.null(xlab.line)) xlab.line <- margins[1L] - 1.25
    if (trace > 0)
      msg2(
        "xlab is",
        xlab,
        "margins is",
        margins,
        " and xlab.line is",
        xlab.line
      )
    mtext(xlab, side = 1, line = xlab.line)
  }

  axis(
    4,
    iy,
    labels = labRow,
    las = 2,
    line = -0.5,
    tick = 0,
    cex.axis = cexRow,
    col.axis = col.axis
  )
  if (!is.null(ylab)) {
    if (is.null(ylab.line)) ylab.line <- margins[2L] - 1.25
    mtext(ylab, side = 4, line = ylab.line)
  }

  if (!missing(add.expr)) eval.parent(substitute(add.expr))

  # Main Title ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  # Plot ----
  par(mar = c(margins[1L], 0, 0, 0), bg = theme$bg)
  if (doRdend)
    plot(
      ddr,
      horiz = TRUE,
      axes = FALSE,
      yaxs = "i",
      leaflab = "none",
      edgePar = list(col = col.axis)
    ) else frame()
  par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2L]), bg = theme$bg)
  if (doCdend)
    plot(
      ddc,
      axes = FALSE,
      xaxs = "i",
      leaflab = "none",
      edgePar = list(col = col.axis)
    ) else if (!is.null(main)) frame()
  if (!is.null(main)) {
    par(xpd = NA, mar = c(0, 0, 0, 0), bg = theme$bg)
    title(
      main,
      cex.main = 1.5 * op[["cex.main"]],
      adj = main.adj,
      line = main.line
    )
  }

  # Colorbar ----
  if (colorbar) {
    frame()
    # par(xpd = NA)
    if (is.na(Colv) && is.null(main)) {
      if (is.null(cb.mar)) cb.mar <- c(margins[1], 2, 0, 3)
      cb.add.new <- FALSE
    } else {
      if (is.null(cb.mar)) cb.mar <- c(margins[1], 0, 0, 3)
      cb.add.new <- TRUE
    }
    colorGrad(
      colorGrad.n,
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
      cex = cb.cex,
      cb.axis.pos = 1.2,
      cb.add = cb.add.new,
      cb.add.mar = cb.mar,
      bar.min = ddSci(zlim[1]),
      bar.mid = ddSci((zlim[1] + zlim[2]) / 2),
      bar.max = ddSci(zlim[2]),
      col.text = col.axis
    )

    if (!is.null(cb.title)) mtext(text = cb.title, cex = cb.title.cex)
  }

  # (column) Group names ----
  if (group.legend) {
    group.names <- levels(group.columns)
    ngroups <- length(group.names)
    frame()
    par(mar = rep(0, 4), oma = rep(0, 4))
    mtext(
      group.names,
      1,
      line = seq(1, ngroups * 1.3, 1.3) - (ngroups + 2),
      col = unlist(column.palette)[seq_len(ngroups)],
      adj = 0,
      xpd = TRUE,
      cex = theme$cex
    )
    # line = seq(-1, -ngroups * 1.3, -1.3)
  }

  if (!is.null(filename)) dev.off()

  invisible(list(
    rowInd = rowInd,
    colInd = colInd,
    Rowv = if (keep.dendro && doRdend) ddr,
    Colv = if (keep.dendro && doCdend) ddc,
    lmat = lmat,
    group.columns = group.columns,
    group.rows = group.rows
  ))
} # rtemis::mplot3_heatmap
