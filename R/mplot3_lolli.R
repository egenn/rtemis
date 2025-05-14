# mplot3_lolli
# ::rtemis::
# E.D. Gennatas rtemis.org

#' `mplot3` Lollipop Plot
#'
#' @inheritParams mplot3_xy
#' @param x Float, vector: Input data
#' @param order.on.x Logical: If TRUE, order by value of `x`. Default = TRUE
#' @param plot.top Float or Integer: If <= 1, plot this percent highest absolute values, otherwise plot this many top values.
#' i.e.: `plot.top = .2` will print the top 20% highest values, and `plot.top = 20` will plot the top 20
#' highest values
#' @param xnames Character, vector: Names of `x`
#' @param main Character: Main title
#' @param col Color, vector: Lollipop color
#' @param cex Float: Character expansion factor for points. Default = 1.2
#' @param matching.segment.col Logical: If TRUE, color line segments using `col`, i.e. same as
#' @param segment.alpha Float: Transparency for line segments. Default = .5
#' points. Default = FALSE, in which case they are colored with `theme$fg`
#' @param lty Integer: Line type for segment segments. See `par("lty")` Default = 1
#' @param lwd Float: Width for segment segments. See `par("lty")` Default = 1
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- rnorm(12)
#' mplot3_lolli(x)
#' # a "rounded" barplot
#' mplot3_lolli(x, segments = T, points = F,
#'              lty = 1, matching.segment.col = T,
#'              lwd = 10, segment.alpha = 1)
#' }

mplot3_lolli <- function(
  x,
  order.on.x = TRUE,
  plot.top = 1,
  orientation = c("horizontal", "vertical"),
  xnames = NULL,
  points = TRUE,
  segments = TRUE,
  main = NULL,
  col = NULL,
  cex = 1.2,
  matching.segment.col = FALSE,
  segment.alpha = .333,
  lty = 3,
  lwd = 2,
  theme = rtTheme,
  palette = rtPalette,
  autolabel = letters,
  par.reset = TRUE,
  pdf.width = 6,
  pdf.height = 6,
  mar = c(2.5, 3, 2, 1),
  pty = "m",
  pch = 16,
  x.axis.at = NULL,
  y.axis.at = NULL,
  xlab = NULL,
  ylab = NULL,
  label.las = 1,
  label.padj = .5,
  xaxs = "r",
  yaxs = "r",
  xlab.adj = .5,
  ylab.adj = .5,
  filename = NULL,
  ...
) {
  # Arguments ----
  orientation <- match.arg(orientation)
  .horizontal <- orientation == "horizontal"
  if (.horizontal) {
    if (is.null(xlab)) xlab <- labelify(deparse(substitute(x)))
  } else {
    if (is.null(ylab)) ylab <- labelify(deparse(substitute(x)))
  }
  # if (order.on.x) x <- sort(x)
  if (is.null(xnames)) {
    xnames <- if (is.null(names(x))) {
      seq_along(x)
    } else {
      names(x)
    }
  }

  # Index ----
  if (plot.top != 1) {
    index <- if (plot.top <= 1) {
      order(abs(x))[(length(x) - plot.top * length(x)):length(x)]
    } else {
      if (plot.top > length(x)) plot.top <- length(x)
      order(abs(x))[(length(x) - plot.top + 1):length(x)]
    }
    x <- x[index]
    xnames <- xnames[index]
  }

  if (order.on.x) {
    index <- order(x)
    x <- x[index]
    xnames <- xnames[index]
  }

  # Output directory
  if (!is.null(filename) && !dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # Palette ----
  if (is.character(palette)) palette <- rtpalette(palette)
  if (is.null(col)) col <- rep(palette[1], length(x))
  if (length(col) < length(x)) col <- recycle(col, x)

  # Plot ----
  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex, xpd = FALSE)
  if (.horizontal) {
    plot(
      NULL,
      NULL,
      xlim = getlim(x, axs = "i"),
      ylim = c(1, length(x)),
      axes = FALSE
    )
  } else {
    plot(
      NULL,
      NULL,
      ylim = getlim(x, axs = "i"),
      xlim = c(1, length(x)),
      axes = FALSE
    )
  }

  # Plot bg ----
  if (theme$plot.bg != "transparent") {
    if (.horizontal) {
      .xlim <- getlim(x, axs = xaxs)
      .ylim <- getlim(seq_along(x), axs = yaxs)
    } else {
      .ylim <- getlim(x, axs = yaxs)
      .xlim <- getlim(seq_along(x), axs = xaxs)
    }
    rect(
      .xlim[1],
      .ylim[1],
      .xlim[2],
      .ylim[2],
      border = NA,
      col = theme$plot.bg
    )
  }

  # Grid ----
  if (theme$grid) {
    grid(
      nx = if (.horizontal) theme$grid.nx else NA,
      ny = if (.horizontal) NA else theme$grid.ny,
      col = colorAdjust(theme$grid.col, theme$grid.alpha),
      lty = theme$grid.lty,
      lwd = theme$grid.lwd
    )
  }

  # Zero line ----
  .xlim <- getlim(x, axs = xaxs)
  if (theme$zerolines && .xlim[1] < 0 && 0 < .xlim[2]) {
    if (.horizontal) {
      abline(v = 0, col = adjustcolor(theme$fg, theme$zerolines.alpha))
    } else {
      abline(h = 0, col = adjustcolor(theme$fg, theme$zerolines.alpha))
    }
  }

  # Axes ----
  if (theme$axes.visible) {
    axis(
      side = theme$x.axis.side,
      line = theme$x.axis.line,
      at = if (.horizontal) x.axis.at else seq_along(x),
      labels = if (.horizontal) NULL else xnames,
      col = theme$axes.col,
      col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
      col.axis = theme$tick.labels.col,
      las = theme$x.axis.las,
      padj = theme$x.axis.padj,
      hadj = theme$x.axis.hadj,
      tck = theme$tck,
      tcl = theme$tcl,
      cex = theme$cex,
      family = theme$font.family
    )
    axis(
      side = theme$y.axis.side,
      line = theme$y.axis.line,
      at = if (.horizontal) seq_along(x) else y.axis.at,
      labels = if (.horizontal) xnames else NULL,
      col = theme$axes.col,
      col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
      col.axis = theme$tick.labels.col,
      # las = theme$y.axis.las,
      las = label.las,
      # padj = theme$y.axis.padj,
      padj = .5,
      # hadj = theme$y.axis.hadj,
      mgp = c(3, .5, 0),
      hadj = 1,
      tck = theme$tck,
      tcl = theme$tcl,
      cex = theme$cex,
      family = theme$font.family
    )
    mtext(
      xlab,
      side = theme$x.axis.side,
      line = theme$xlab.line,
      cex = theme$cex,
      adj = xlab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )
    mtext(
      ylab,
      side = theme$y.axis.side,
      line = theme$ylab.line,
      cex = theme$cex,
      adj = ylab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )
  }

  # Lollipops ----
  if (segments) {
    if (.horizontal) {
      for (i in seq_along(x)) {
        segments(
          0,
          i,
          x[i],
          i,
          col = if (matching.segment.col)
            adjustcolor(col[[i]], segment.alpha) else
            adjustcolor(theme$fg, segment.alpha),
          lty = lty,
          lwd = lwd
        )
      }
    } else {
      for (i in seq_along(x)) {
        segments(
          i,
          0,
          i,
          x[i],
          col = if (matching.segment.col)
            adjustcolor(col[[i]], segment.alpha) else
            adjustcolor(theme$fg, segment.alpha),
          lty = lty,
          lwd = lwd
        )
      }
    }
  }

  if (points) {
    if (.horizontal) {
      points(x, seq_along(x), col = unlist(col), pch = pch, cex = cex)
    } else {
      points(seq_along(x), x, col = unlist(col), pch = pch, cex = cex)
    }
  }

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

  if (!is.null(filename)) dev.off()
} # rtemis::mplot3_lolli
