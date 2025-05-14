# mplot3_img.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Draw image (False color 2D)
#'
#' Draw a bitmap from a matrix of values.
#'
#' This is also a good way to plot a large heatmap.
#' This function calls `image` which is a lot faster than drawing heatmaps
#'
#' @param z Input matrix
#' @param as.mat Logical: If FALSE, rows and columns of z correspond to x and y coordinates accoridngly.
#'   This is the `image` default.
#'   If TRUE (default), resulting image's cells will be in the same order as values appear when
#'   you print z in the console. This is `t(apply(z, 2, rev))`. In this case, you can think of z
#'   as a table of values you want to pictures with colors. For example, you can convert a correlation table
#'   to a figure. In this case, you might want to add `cell.labs` with the values. Consider first using
#'   [ddSci].
#' @param col Colors to use. Defaults to `colorGrad(100)`
#' @param cell.labs Matrix of same dimensions as z (Optional): Will be printed as strings over cells
#' @param cell.labs.col Color for `cell.labs`. If NULL, the upper and lower quartiles will be
#' set to "white", the rest "black".
#' @param bg Background color
#' @param filename String (Optional): Path to file where image should be saved. R-supported extensions:
#' ".pdf", ".jpeg", ".png", ".tiff".
#' @param file.width Output Width in inches
#' @param file.height Output height in inches
#' @param par.reset Logical: If TRUE, par will be reset to original settings before exit. Default = TRUE
#' @param ... Additional arguments to be passed to `graphics::image`
#'
#' @author E.D. Gennatas
#' @export

mplot3_img <- function(
  z,
  as.mat = TRUE,
  col = NULL,
  xnames = NULL,
  xnames.y = 0,
  ynames = NULL,
  # ynames.x = 0,
  main = NULL,
  main.adj = 0,
  # main.line = 1.5,
  x.axis.side = 3,
  y.axis.side = 2,
  x.axis.line = -.5,
  y.axis.line = -.5,
  x.axis.las = 0,
  y.axis.las = 1,
  x.tick.labs.adj = NULL,
  y.tick.labs.adj = NULL,
  x.axis.font = 1,
  y.axis.font = 1,
  xlab = NULL,
  ylab = NULL,
  xlab.adj = .5,
  ylab.adj = .5,
  xlab.line = 1.7,
  ylab.line = 1.7,
  xlab.padj = 0,
  ylab.padj = 0,
  xlab.side = 1,
  ylab.side = 2,
  main.col = NULL,
  # xnames.col = "black",
  # ynames.col = "black",
  axlab.col = NULL,
  axes.col = NULL,
  labs.col = NULL,
  tick.col = NULL,
  cell.lab.hi.col = NULL,
  cell.lab.lo.col = NULL,
  cex = 1.2,
  cex.ax = NULL,
  cex.x = NULL,
  cex.y = NULL,
  # x.srt = NULL,
  # y.srt = 0,
  zlim = NULL,
  autorange = TRUE,
  pty = "m",
  mar = NULL,
  asp = NULL,
  ann = FALSE,
  axes = FALSE,
  cell.labs = NULL,
  cell.labs.col = NULL,
  cell.labs.autocol = TRUE, # WIP add autocol w cutoffs at abs(.4)
  bg = NULL,
  theme = getOption("rt.theme", "white"),
  autolabel = letters,
  filename = NULL,
  file.width = NULL,
  file.height = NULL,
  par.reset = TRUE,
  ...
) {
  # Arguments ----
  # Compatibility with rtlayout()
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE

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

  # zlim ----
  if (is.null(zlim)) {
    if (autorange) {
      max.z <- max(abs(z))
      zlim <- c(-max.z, max.z)
    } else {
      zlim <- range(z)
    }
  }

  # Autosize cex.ax ----
  # at NROW == 50, cex.ax <- .5
  # at NROW == 20, cex.ax <- 1
  if (is.null(cex.ax)) {
    if (NROW(z) < 20) {
      cex.ax <- 1
    } else {
      cex.ax <- NROW(z) * -.01667 + 1.333
    }
  }
  if (is.null(cex.x)) cex.x <- cex.ax
  if (is.null(cex.y)) cex.y <- cex.ax

  # Themes ----
  if (is.null(cell.lab.lo.col)) cell.lab.lo.col <- theme$fg
  if (is.null(cell.lab.hi.col))
    cell.lab.hi.col <- if (mean(col2rgb(theme$bg)) > 127) theme$bg else theme$fg
  if (is.null(col)) {
    col <- colorGrad(101, lo = "#18A3AC", mid = theme$bg, hi = "#F48024")
  }

  # Image ----
  if (!is.null(filename)) {
    graphics <- gsub(".*\\.", "", filename)
    if (is.null(file.width)) {
      file.width <- file.height <- if (graphics == "pdf") 6 else 500
    }
  }

  .xnames <- xnames
  .ynames <- ynames
  if (is.null(.xnames)) .xnames <- if (!is.null(rownames(z))) rownames(z)
  if (is.null(.ynames)) .ynames <- if (!is.null(colnames(z))) colnames(z)

  if (as.mat) {
    x <- seq_len(NCOL(z))
    y <- seq_len(NROW(z))
    z <- t(apply(z, 2, rev))
    # if (!is.null(.xnames)) .ynames <- rev(.xnames)
    # if (!is.null(.ynames)) .xnames <- .ynames
  } else {
    x <- seq_len(NROW(z))
    y <- seq_len(NCOL(z))
  }

  # pdf() has argument "file", bmp(), jpeg(), png(), and tiff() have "filename";
  # "file" works in all
  if (!is.null(filename))
    do.call(
      graphics,
      args = list(file = filename, width = file.width, height = file.height)
    )

  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Automar
  if (is.null(mar)) {
    mar1maxchar <- if (as.mat) max(1, nchar(.ynames)) else
      max(1, nchar(.xnames))
    mar1 <- 1.5 + .5 * mar1maxchar
    mar2maxchar <- if (as.mat) max(1, nchar(.xnames)) else
      max(1, nchar(.ynames))
    mar2 <- 1.5 + .5 * mar2maxchar
    mar <- c(mar1, mar2, 2., 1)
  }

  par(pty = pty, mar = mar, bg = theme$bg)

  image(
    x,
    y,
    data.matrix(z),
    col = col,
    zlim = zlim,
    asp = asp,
    ann = ann,
    axes = axes,
    ...
  )

  # Tick names ----
  if (!is.null(if (as.mat) .ynames else .xnames)) {
    axis(
      side = theme$x.axis.side,
      at = seq_along(x),
      labels = if (as.mat) .ynames else .xnames,
      col = theme$axes.col,
      tick = FALSE,
      las = x.axis.las,
      col.axis = theme$tick.labels.col,
      cex = theme$cex,
      font = x.axis.font,
      family = theme$font.family
    )
  }
  if (!is.null(if (as.mat) rev(.xnames) else .ynames)) {
    axis(
      side = theme$y.axis.side,
      at = seq_along(y),
      labels = if (as.mat) rev(.xnames) else .ynames,
      col = theme$axes.col,
      tick = FALSE,
      las = y.axis.las,
      col.axis = theme$tick.labels.col,
      cex = theme$cex,
      font = y.axis.font,
      family = theme$font.family
    )
  }

  # Main title ----
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

  # Axes & title ----
  if (!is.null(xlab))
    mtext(
      xlab,
      side = theme$x.axis.side,
      line = xlab.line,
      cex = theme$cex,
      adj = xlab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )
  if (!is.null(ylab))
    mtext(
      ylab,
      side = theme$y.axis.side,
      line = ylab.line,
      cex = theme$cex,
      adj = ylab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )

  # Cell labels ----
  if (is.null(cell.labs.col)) {
    cell.labs.col <- ifelse(
      z >= quantile(zlim)[4],
      cell.lab.hi.col,
      cell.lab.lo.col
    )
  }

  # cell.labs
  if (!is.null(cell.labs)) {
    if (length(cell.labs.col) < length(z)) {
      cell.labs.col <- matrix(
        rep(cell.labs.col, length(z) / length(cell.labs.col)),
        NROW(z)
      )
    }
    if (as.mat) {
      cell.labs <- t(apply(cell.labs, 2, rev))
    }
    text(
      rep(x, length(y)),
      rep(y, times = 1, each = length(x)),
      labels = cell.labs,
      col = cell.labs.col
    )
  }

  if (!is.null(filename)) grDevices::dev.off()

  invisible(list(mar = mar, zlim = zlim))
} # rtemis::mplot3_img
