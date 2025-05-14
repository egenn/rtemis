# mplot3_varimp
# ::rtemis::
# 2017-9 E.D. Gennatas rtemis.org

#' `mplot3`: Variable Importance
#'
#' Draw horizontal barplots for variable importance
#'
#' "NA" values in input are set to zero.
#' @param x Vector, numeric: Input
#' @param error Vector, numeric; length = length(x): Plot error bars with given error.
#' @param names Vector, string; optional: Names of variables in `x`
#' @param plot.top Float or Integer: If <= 1, plot this percent highest absolute values, otherwise plot this many top values.
#' i.e.: `plot.top = .2` will print the top 20% highest values, and `plot.top = 20` will plot the top 20
#' highest values
#' @param labelify Logical: If TRUE convert `names(x)` using [labelify]. Default = TRUE
#' @param col Colors: Gradient to use for barplot fill.
#' @param alpha Float (0, 1): Alpha for `col`
#' @param error.col Color: For error bars
#' @param trace Integer: If `trace > 0` prints out the automatically set `mar` (so you can adjust if needed)
#' `names` provided
#' @return Position of bar centers (invisibly)
#' @author E.D. Gennatas
#' @export

mplot3_varimp <- function(
  x,
  error = NULL,
  names = NULL,
  names.pad = .02,
  plot.top = 1, # 1 or less means plot this percent
  labelify = TRUE,
  col = NULL,
  palette = rtPalette,
  alpha = 1,
  error.col = theme$fg,
  error.lwd = 2,
  beside = TRUE,
  border = NA,
  width = 1,
  space = .75,
  xlim = NULL,
  ylim = NULL,
  xlab = "Variable Importance",
  xlab.line = 1.3,
  ylab = NULL,
  ylab.line = 1.5,
  main = NULL,
  names.arg = NULL,
  axisnames = FALSE,
  sidelabels = NULL,
  mar = NULL,
  pty = "m",
  barplot.axes = FALSE,
  xaxis = TRUE,
  x.axis.padj = -1.2,
  tck = -.015,
  theme = rtTheme,
  zerolines = FALSE,
  par.reset = TRUE,
  autolabel = letters,
  pdf.width = NULL,
  pdf.height = NULL,
  trace = 0,
  filename = NULL,
  ...
) {
  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  if (is.character(palette)) palette <- rtpalette(palette)

  # Data ----
  if (NCOL(x) > 1 && NROW(x) > 1)
    stop("x must be a vector or single row or column")
  x[is.na(x)] <- 0

  # '- Names ----
  if (is.null(names)) {
    if (is.null(names(x))) {
      .names <- if (NCOL(x) == 1) labelify(rownames(x)) else
        labelify(colnames(x))
    } else {
      .names <- labelify(names(x))
    }
  } else {
    .names <- labelify(names)
  }

  x <- as.numeric(x)
  if (length(.names) == 0) {
    .names <- paste("Feature", seq_along(x))
  }

  # '- Index ----
  index <- if (plot.top <= 1) {
    order(abs(x))[(length(x) - plot.top * length(x)):length(x)]
  } else {
    if (plot.top > length(x)) plot.top <- length(x)
    order(abs(x))[(length(x) - plot.top + 1):length(x)]
  }
  x <- x[index]
  if (!is.null(error)) error <- error[index]
  .names <- .names[index]
  # order by value
  index <- order(x)
  x <- x[index]
  .names <- .names[index]
  if (!is.null(error)) error <- error[index]

  if (!is.null(rtenv$rtpar)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # xlim & ylim ----
  if (is.null(ylim)) {
    ylim <- c(0, length(x) + (length(x) + 1) * space)
  }

  if (is.null(xlim)) {
    .error <- if (is.null(error)) 0 else error
    x.range <- max(x + .error) - min(x + .error)
    x.min <- min(x + .error) - .04 * x.range
    x.max <- max(x + .error) + .04 * x.range
    xlim <- range(c(0, x.min, x.max))
  }

  if (is.null(col)) {
    # if (is.null(palette)) palette <- c(theme$fg, "#18A3AC")
    # col <- colorGrad.x(x, palette)
    col <- palette[1]
  }
  cols <- colorAdjust(col, alpha = alpha)

  # Auto-margins ----
  if (is.null(mar)) {
    mar1 <- ifelse(xlab == "", 1.5, 2.5)
    # mar2 <- max(strwidth(.names)) + 2.5 # this freaks out notebooks
    # mar2 <- 1.0741 + 0.4 * max(nchar(.names))
    mar2 <- textwidth(.names)
    mar3 <- if (is.null(main)) .5 else 2
    mar <- c(mar1, mar2, mar3, .8)
    if (trace > 0) cat(gray("mar set to"), mar)
  }

  # Plot ----
  # '- PDF autosize ----
  if (!is.null(filename)) {
    if (is.null(pdf.height)) pdf.height <- length(x) * .2 + .5
    if (is.null(pdf.width)) {
      mar2 <- textwidth(.names)
      pdf.width <- mar2 * .7
    }
  }

  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  par(
    mar = mar,
    bg = theme$bg,
    pty = pty,
    cex = theme$cex,
    family = theme$font.family
  )
  plot(
    NULL,
    NULL,
    xlim = xlim,
    ylim = ylim,
    bty = "n",
    axes = FALSE,
    ann = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # Plot Background ----
  if (theme$plot.bg != "transparent") {
    bg.ylim <- c(min(ylim), max(ylim) + .04 * diff(range(ylim)))
    rect(
      xlim[1],
      bg.ylim[1],
      xlim[2],
      bg.ylim[2],
      border = NA,
      col = theme$plot.bg
    )
  }

  # Grid ----
  if (theme$grid) {
    grid(
      nx = theme$grid.nx,
      ny = 0,
      col = colorAdjust(theme$grid.col, theme$grid.alpha),
      lty = theme$grid.lty,
      lwd = theme$grid.lwd
    )
  }

  # Barplot ----
  barCenters <- barplot(
    x,
    col = cols,
    border = border,
    xlim = xlim,
    ylim = ylim,
    axes = barplot.axes,
    cex.axis = theme$cex,
    cex.names = theme$cex,
    add = TRUE,
    xlab = NULL,
    axisnames = axisnames,
    names.arg = names.arg,
    width = width,
    space = space,
    horiz = TRUE,
    # xpd = FALSE,
    xaxs = "i",
    yaxs = "i",
    ...
  )
  if (min(x) < 0 && max(x) > 0)
    abline(v = 0, col = theme$labs.col, lwd = theme$grid.lwd)

  # Error bars ----
  if (!is.null(error)) {
    if (is.null(error.col)) error.col <- cols
    segments(
      as.vector(x) - as.vector(error),
      as.vector(barCenters),
      as.vector(x) + as.vector(error),
      as.vector(barCenters),
      lwd = error.lwd,
      col = error.col
    )

    arrows(
      x - as.vector(error),
      barCenters,
      x + as.vector(error),
      barCenters,
      lwd = error.lwd,
      angle = 90,
      code = 3,
      length = 0.05,
      col = error.col
    )
  }

  # x-axis ----
  if (xaxis)
    axis(
      1,
      col = theme$axes.col,
      col.axis = theme$labs.col,
      col.ticks = theme$tick.col,
      padj = x.axis.padj,
      tck = tck,
      cex = theme$cex
    )

  # Main Title ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    mtext(
      main,
      line = theme$main.line,
      font = theme$main.font,
      adj = theme$main.adj,
      cex = theme$cex,
      col = theme$main.col,
      family = theme$font.family
    )
  }

  # Names ----
  text(
    x = min(xlim) - names.pad * diff(xlim),
    y = barCenters,
    labels = .names,
    adj = 1,
    xpd = TRUE,
    col = theme$labs.col
  )

  # Axes Labels ----
  if (!is.null(xlab))
    mtext(xlab, 1, cex = theme$cex, line = xlab.line, col = theme$labs.col)
  if (!is.null(ylab))
    mtext(ylab, 2, cex = theme$cex, line = ylab.line, col = theme$labs.col)

  # Side Labels ----
  if (!is.null(sidelabels)) {
    # mtext(sidelabels, 4, at = barCenters)
    text(
      x = max(xlim) * 1.01,
      y = barCenters,
      labels = sidelabels,
      xpd = TRUE,
      pos = 4
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()
  invisible(list(barCenters = barCenters, xlim = xlim, ylim = ylim))
} # rtemis::mplot3_varimp
