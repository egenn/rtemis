# colorGrad.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Color Gradient
#'
#' Create a gradient of colors and optionally a colorbar
#'
#' It is best to provide an odd number, so that there is always an equal number of colors on either side
#' of the midpoint.
#' For example, if you want a gradient from -1 to 1 or equivalent, an n = 11, will give 5 colors on either
#' side of 0, each representing a 20\% change from the next.
#'
#' `colors` can be defined as a sequence of 3-letter color abbreviations of 2, 3, 4, or 5 colors
#'   which will correspond to values: \{"lo","hi"\}; \{"lo", "mid", "hi"\}; \{"lo", "mid", "midhi", "hi"\}, and
#'   \{"lo", "lomid", "mid", "midhi", "hi"\}, respectively.
#'   For example, try `colorGrad(21, "blugrnblkredyel", colorbar = TRUE)`
#'   3-letter color abbreviations:
#'    wht: white; blk: black; red; grn: green; blu: blue; yel: yellow; rng: orange; prl: purple
#'
#' @param n Integer: How many distinct colors you want. If not odd, converted to `n + 1`
#'   Defaults to 21
#' @param colors Character: Acts as a shortcut to defining `lo`, `mid`, etc for a number of defaults:
#'   "french", "penn", "grnblkred",
#' @param space Character: Which colorspace to use. Option: "rgb", or "Lab". Default = "rgb".
#'   Recommendation: If `mid` is "white" or "black" (default), use "rgb", otherwise "Lab"
#' @param lo Color for low end
#' @param lomid Color for low-mid
#' @param mid Color for middle of the range or "mean", which will result in `colorOp(c(lo, hi), "mean")`.
#' If `mid = NA`, then only `lo` and `hi` are used to create the color gradient.
#' @param midhi Color for middle-high
#' @param hi Color for high end
#' @param preview Logical: Plot the colors horizontally
#' @param colorbar Logical: Create a vertical colorbar
#' @param cb.n Integer: How many steps you would like in the colorbar
#' @param cb.mar Vector, length 4: Colorbar margins. Default: c(1, 1, 1, 1)
#' @param cb.add Logical: If TRUE, colorbar will be added to existing plot
#' @param cb.add.mar Vector: Margins for colorbar (See `par("mar")`)
#' @param cb.axis.pos Float: Position of axis (See `axis("pos")`)
#' @param cb.axis.las Integer {0,1,2,3}: Style of axis labels. 0: Always parallel to the axis,
#' 1: Horizontal, 2: Perpendicular, 3: Vertical. Default = 1
#' @param cb.axis.hadj Float: Adjustment parallel to the reading direction (See `par("adj")`)
#' @param cb.cex FLoat: Character expansion factor for colorbar (See `par("cex")`)
#' @param bar.min Numeric: Lowest value in colorbar
#' @param bar.mid Numeric: Middle value in colorbar
#' @param bar.max Numeric: Max value in colorbar
#' @param cex Float: Character expansion for axis
#' @param filename String (Optional: Path to file to save colorbar
#' @param pdf.width Float: Width for PDF output. Default = 3
#' @param pdf.height Float: Height for PDF output. Default = 7
#' @param theme Character: "light", "dark"
#' @param bg Color: Background color
#' @param col.text Color: Colorbar text color
#' @param plotlycb Logical: Create colorbar using `plotly` (instead of base R graphics)
#' @param plotly.width Float: Width for plotly colorbar. Default = 80
#' @param plotly.height Float: Height for plotly colorbar. Default = 500
#' @param rtrn.plotly Logical: If TRUE, return `plotly` object
#' @param margins Vector: Plotly margins. Default = c(0, 0, 0, 0)
#' @param pad Float: Padding for `plotly`. Default = 0
#' @param par.reset Logical: If TRUE (Default), reset `par` settings after running
#' @return Invisible vector of hexadecimal colors / plotly object if `rtrn.plotly = TRUE`
#' @author E.D. Gennatas
#' @export

colorGrad <- function(
  n = 21,
  colors = NULL,
  space = c("rgb", "Lab"),
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  preview = FALSE,
  colorbar = FALSE,
  cb.n = 21,
  cb.mar = c(1, 1, 1, 1),
  cb.add = FALSE,
  cb.add.mar = c(5, 0, 2, 5),
  cb.axis.pos = 1.1,
  cb.axis.las = 1,
  cb.axis.hadj = 0,
  cb.cex = 6,
  bar.min = -1,
  bar.mid = 0,
  bar.max = 1,
  cex = 1.2,
  filename = NULL,
  pdf.width = 3,
  pdf.height = 7,
  theme = getOption("rt.theme", "light"),
  bg = NULL,
  col.text = NULL,
  plotlycb = FALSE,
  plotly.width = 80,
  plotly.height = 500,
  rtrn.plotly = FALSE,
  margins = c(0, 0, 0, 0),
  pad = 0,
  par.reset = TRUE
) {
  # [ Arguments ] ----
  n <- as.integer(n)
  if (n %% 2 != 1) n <- n + 1
  if (!is.null(filename)) colorbar <- TRUE
  if (rtrn.plotly) plotlycb <- TRUE
  if (is.null(cb.n)) {
    cb.n <- n
    if (cb.n %% 2 != 1) cb.n <- cb.n + 1
  }
  space <- match.arg(space)
  theme <- if (strtrim(theme, 4) == "dark") "dark" else "light"

  # [ Colors ] ----
  if (!is.null(colors)) {
    if (colors == "french") {
      lo <- "#01256E"
      lomid <- NULL
      mid <- "white"
      midhi <- NULL
      hi <- "#95001A"
    } else if (colors == "penn") {
      lo <- "#02CFFF"
      lomid <- NULL
      mid <- "#01256E"
      midhi <- "#95001A"
      hi <- "#F2C100"
    } else if (colors == "blues") {
      lo <- "#01256E"
      mid <- NULL
      hi <- "#82AFD3"
    } else if (colors == "greens") {
      lo <- "#005200"
      mid <- NULL
      hi <- "#80DF80"
    } else {
      cols <- colorvec(cols = colors)
      lo <- cols$lo
      lomid <- cols$lomid
      mid <- cols$mid
      midhi <- cols$midhi
      hi <- cols$hi
    }
  }

  # [ Grad ] ----
  n <- as.integer(n)
  midpoint <- ceiling(n / 2)
  if (is.null(mid)) mid <- ifelse(theme == "light", "white", "black")
  if (!is.na(mid)) {
    if (mid == "mean") mid <- colorOp(c(lo, hi), "mean")
    lo2mid <- colorRampPalette(c(lo, lomid, mid), space = space)
    mid2hi <- colorRampPalette(c(mid, midhi, hi), space = space)
    grad <- c(lo2mid(midpoint), mid2hi(n - midpoint + 1)[-1])
  } else {
    grad <- colorRampPalette(c(lo, hi), space = space)(n)
  }

  if (cb.n != n) {
    cb.n <- as.integer(cb.n)
    cb.midpoint <- ceiling(cb.n / 2)
    # if (is.null(mid)) mid <- colorOp(c(lo, hi), "mean")
    # lo2mid <- grDevices::colorRampPalette(c(lo, lomid, mid), space = space)
    # mid2hi <- grDevices::colorRampPalette(c(mid, midhi, hi), space = space)
    if (!is.na(mid)) {
      cb.grad <- c(lo2mid(cb.midpoint), mid2hi(cb.n - cb.midpoint + 1)[-1])
    } else {
      cb.grad <- colorRampPalette(c(lo, hi), space = space)(cb.n)
    }
  } else {
    cb.grad <- grad
    cb.midpoint <- midpoint
  }

  # [ Preview ] ----
  if (preview) {
    plot(
      rep(1, n),
      col = grad,
      pch = 19,
      cex = 6,
      xlim = c(0.5, n + .5),
      ylim = c(.8, 1.2),
      ann = FALSE,
      axes = FALSE
    )
    text(
      x = 0.25,
      y = 1.05,
      labels = paste0("Color gradient (n = ", n, ")"),
      adj = 0,
      cex = 1.5
    )
    segments(midpoint, .95, midpoint, 1.05, lwd = 2, lty = 2, col = NA)
  }

  # [ Colorbar ] ----
  if (colorbar) {
    if (theme == "light") {
      if (is.null(bg)) bg <- "white"
      if (is.null(col.text)) col.text <- "black"
    } else if (theme == "dark") {
      if (is.null(bg)) bg <- "black"
      if (is.null(col.text)) col.text <- "white"
    }

    par.orig <- par(no.readonly = TRUE)
    if (par.reset && !cb.add) on.exit(suppressWarnings(par(par.orig)))
    if (cb.add) {
      par(new = cb.add, pty = "m", mar = cb.add.mar)
    } else {
      par(bg = bg, mar = cb.mar, pty = "m")
    }

    if (!is.null(filename)) {
      grDevices::pdf(
        filename,
        width = pdf.width,
        height = pdf.height,
        title = "rtemis Graphics"
      )
    }
    plot(
      rep(1, cb.n),
      1:cb.n,
      col = cb.grad,
      pch = 19,
      cex = cb.cex,
      xlim = c(.5, 1.5),
      ylim = c(.5, cb.n + .5),
      ann = FALSE,
      axes = FALSE
    )
    # box() # to visualize position
    # text(1.5, c(1, midpoint, n), labels = c(bar.min, bar.mid, bar.max), col = col.text)
    axis(
      side = 4,
      at = c(1, cb.midpoint, cb.n),
      labels = c(bar.min, bar.mid, bar.max),
      col = colorAdjust("black", 0),
      col.axis = col.text,
      col.ticks = colorAdjust("black", 0),
      pos = cb.axis.pos,
      las = cb.axis.las,
      cex.axis = cex,
      hadj = cb.axis.hadj
    )
    if (!is.null(filename)) grDevices::dev.off()
  }

  # [ Plotly cb ] ----
  if (plotlycb) {
    requireNamespace("plotly")

    m <- list(
      size = 40,
      color = grad,
      opacity = 1,
      symbol = "circle"
    )

    x.ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0.8, 1.4)
    )

    y.ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )

    t <- list(
      family = "Open Sans",
      size = 22,
      color = plotly::toRGB("black")
    )

    a <- list()
    for (i in 1:3) {
      a[[i]] <- list(
        x = 1.3,
        y = c(1, midpoint, n)[i],
        text = as.character(c(bar.min, bar.mid, bar.max))[i],
        xref = "x",
        yref = "y",
        showarrow = FALSE
      )
    }

    hovtext <- ddSci(seq(bar.min, bar.max, (bar.max - bar.min) / (n - 1)))

    margin <- list(
      b = margins[1],
      l = margins[2],
      t = margins[3],
      r = margins[4],
      pad = pad
    )

    p <- plotly::plot_ly(
      x = rep(1, n),
      y = 1:n,
      type = "scatter",
      mode = "markers",
      marker = m,
      hoverinfo = "text",
      text = hovtext
    ) |>
      plotly::layout(
        xaxis = x.ax,
        yaxis = y.ax,
        width = plotly.width,
        height = plotly.height,
        annotations = a,
        font = t,
        margin = margin
      ) |>
      plotly::config(displayModeBar = FALSE)
    if (plotlycb && !rtrn.plotly) print(p)
  }

  # [ out ] ----
  if (rtrn.plotly) {
    return(p)
  }
  invisible(grad)
} # rtemis::colorGrad

# 3-letter Color Name Abbreviations
# wht white
# blk black
# red
# grn green
# blu blue
# yel yellow
# rng orange
# prl purple

colorvec <- function(cols) {
  if (nchar(cols) %% 3 != 0) {
    stop("All colors must be specified by their 3-letter abbreviations")
  }

  cols <- tolower(cols)
  ncols <- nchar(cols) / 3
  cols <- lapply(seq(ncols), function(i) substr(cols, i * 3 - 2, i * 3))

  coldf <- data.frame(
    abbr = c("wht", "red", "grn", "blu", "blk", "yel", "rng", "prl"),
    name = c(
      "white",
      "red",
      "green",
      "blue",
      "black",
      "yellow",
      "orange",
      "purple"
    ),
    stringsAsFactors = FALSE
  )

  cols <- sapply(1:ncols, function(i) coldf[coldf$abbr == cols[i], 2])

  lo <- lomid <- mid <- midhi <- hi <- NULL
  collist <- list(
    twocols = c("lo", "hi"),
    threecols = c("lo", "mid", "hi"),
    fourcols = c("lo", "mid", "midhi", "hi"),
    fivecols = c("lo", "lomid", "mid", "midhi", "hi")
  )

  for (i in seq(ncols)) assign(collist[[ncols - 1]][i], cols[i])
  return(list(lo = lo, lomid = lomid, mid = mid, midhi = midhi, hi = hi))
}
