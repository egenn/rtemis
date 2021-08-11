# dplot3.bar.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Interactive Barplots
#'
#' Draw interactive barplots using \code{plotly}
#'
#' @param x vector (possibly named), matrix, or data.frame: If matrix or data.frame, rows are groups (can be 1 row), columns are features
#' @param main Character: Plot title. Default = NULL
#' @param xlab Character: x-axis label. Default = NULL
#' @param ylab  Character: y-axis label. Default = NULL
#' @param col Color, vector: Color for bars. Default NULL, which will draw colors from \code{palette}
#' @param alpha Float (0, 1]: Transparency for bar colors. Default = .8
#' @param bg Color: Background color. Default = "white"
#' @param plot.bg Color: Background color for plot area. Default = "white"
#' @param theme Character: THeme to use: "light", "dark", "lightgrid", "darkgrid". Default = "lightgrid"
#' @param palette Character: Name of \pkg{rtemis} palette to use. Default = "rtCol1". Only used if \code{col = NULL}
#' @param barmode Character: Type of bar plot to make: "group", "relative", "stack", "overlay". Default = "group". Use
#' "relative" for stacked bars, wich handles negative values correctly, unlike "stack", as of writing.
#' @param group.names Character, vector, length = NROW(x): Group names. Default = NULL, which uses \code{rownames(x)}
#' @param order.by.val Logical: If TRUE, order bars by increasing value.
#' Only be use for single group data. Default = NULL
#' @param ylim Float, vector, length 2: y-axis limits.
#' @param hovernames Character, vector: Optional character vector to show on hover over each bar.
#' @param feature.names Character, vector, length = NCOL(x): Feature names. Default = NULL, which uses
#' \code{colnames(x)}
#' @param font.size  Float: Font size for all labels. Default = 16
#' @param font.family String: Font family to use. Default = "Helvetica Neue"
#' @param main.col Color: Title color. Default = NULL, determined by theme
#' @param axes.col Color: Axes color. Default = NULL, determined, by theme
#' @param labs.col Color: Labels' color. Default = NULL, determined by theme
#' @param legend Logical: If TRUE, draw legend. Default = NULL, and will be turned on if there is
#' more than one feature present
#' @param legend.col Color: Legend text color. Default = NULL, determined by theme
#' @param hline Float: If defined, draw a horizontal line at this y value. Default = NULL
#' @param hline.col Color for \code{hline}. Default = "#ff0000" (red)
#' @param hline.width Float: Width for \code{hline}. Default = 1
#' @param hline.dash Character: Type of line to draw: "solid", "dot", "dash", "longdash", "dashdot",
#' or "longdashdot"
#' @param margin Named list: plot margins. Default = \code{list(b = 50, l = 50, t = 50, r = 20)}
#' @param padding Integer: N pixels to pad plot. Default = 0
#' @param filename Character: Path to file to save static plot. Default = NULL
#' @param file.width Integer: File width in pixels for when \code{filename} is set. Default = 500
#' @param file.height Integer: File height in pixels for when \code{filename} is set. Default = 500
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.bar(VADeaths)
#' # simple individual bars
#' a <- c(4, 7, 2)
#' dplot3.bar(a)
#' # if input is a data.frame, each row is a group
#' b <- data.frame(x = c(3, 5, 7), y = c(2, 1, 8), z = c(4, 5, 2))
#' rownames(b) <- c("Jen", "Ben", "Ren")
#' dplot3.bar(b)
#' # stacked
#' dplot3.bar(b, barmode = "stack")
#' }

dplot3.bar <-  function(x,
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        col = NULL,
                        alpha = .8,
                        horizontal = FALSE,
                        theme = getOption("rt.theme", "lightgrid"),
                        palette = getOption("rt.palette", "rtCol1"),
                        barmode = c("group", "relative", "stack", "overlay"),
                        group.names = NULL,
                        order.by.val = FALSE,
                        ylim = NULL,
                        hovernames = NULL,
                        feature.names = NULL,
                        font.size = 16,
                        legend = NULL,
                        legend.col = NULL,
                        hline = NULL,
                        hline.col = "#FE4AA3",
                        hline.width = 1,
                        hline.dash = "solid",
                        margin = list(b = 50, l = 50, t = 50, r = 20),
                        automargin.x = TRUE,
                        automargin.y = TRUE,
                        padding = 0,
                        displayModeBar = TRUE,
                        filename = NULL,
                        file.width = 500,
                        file.height = 500,
                        trace = 0, ...) {

  # Dependencies ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  barmode <- match.arg(barmode)
  if (!is.null(main)) main <- paste0("<b>", main, "</b>")

  dat <- as.data.frame(x)
  if (NROW(dat) == 1 & barmode != "stack") dat <- as.data.frame(t(dat))

  # Order by val ====
  if (order.by.val) {
    if (NCOL(dat) > 1) {
      .order <- order(sapply(dat, mean, na.rm = TRUE))
      dat <- dat[, .order]
    } else {
      .order <- order(dat)
      dat <- dat[.order, , drop = FALSE]
    }
    if (!is.null(group.names)) group.names <- group.names[.order]
    if (!is.null(hovernames)) hovernames <- hovernames[.order]
  }

  # Group names ====
  .group.names <- group.names
  if (is.null(group.names)) {
    if (!is.null(rownames(dat))) .group.names <- rownames(dat)
  } else if (is.numeric(group.names)) {
    .group.names <- dat[, group.names]
    rownames(dat) <- .group.names
    dat <- dat[, .group.names]
  }

  if (trace > 0) cat(".group.names:", .group.names, "\n")

  # Feature names ====
  .feature.names <- feature.names
  if (is.null(.feature.names)) {
    if (!is.null(colnames(dat))) {
      .feature.names <- labelify(colnames(dat))
    } else {
      .feature.names <- paste0("Feature", seq(NCOL(dat)))
    }
  }

  if (trace > 0) cat(".feature.names:", .feature.names, "\n")
  if (is.null(legend)) legend <- length(.feature.names) > 1

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  p <- NCOL(dat)
  if (is.null(col)) col <- recycle(palette, seq(p))[seq(p)]

  # Theme ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  bg <- plotly::toRGB(theme$bg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col)
  tick.col <- plotly::toRGB(theme$tick.labels.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)
  axes.col <- plotly::toRGB(theme$axes.col)

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  if (!is.null(hovernames)) {
    hovernames <- matrix(hovernames)
    if (NCOL(hovernames) == 1 & p > 1) {
      hovernames <- matrix(rep(hovernames, p), ncol = p)
    }
  }

  # plotly ====
  .group.names <- factor(.group.names, levels = .group.names)
  if (horizontal) {
    plt <- plotly::plot_ly(x = dat[, 1], y = .group.names,
                           type = 'bar',
                           name = .feature.names[1],
                           text = hovernames[, 1],
                           marker = list(color = plotly::toRGB(col[1], alpha)))
    if (p > 1) {
      for (i in seq_len(p)[-1]) plt <- plotly::add_trace(plt, x = dat[, i],
                                                         name = .feature.names[i],
                                                         text = hovernames[, i],
                                                         marker = list(color = plotly::toRGB(col[i], alpha)))
    }
  } else {
    plt <- plotly::plot_ly(x = .group.names, y = dat[, 1],
                           type = 'bar',
                           name = .feature.names[1],
                           text = hovernames[, 1],
                           marker = list(color = plotly::toRGB(col[1], alpha)))
    if (p > 1) {
      for (i in seq_len(p)[-1]) plt <- plotly::add_trace(plt, y = dat[, i],
                                                         name = .feature.names[i],
                                                         text = hovernames[, i],
                                                         marker = list(color = plotly::toRGB(col[i], alpha)))
    }
  }

  # Layout ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)
  tickfont <- list(family = theme$font.family,
                   size = font.size,
                   color = tick.col)
  .legend <- list(font = list(family = theme$font.family,
                              size = font.size,
                              color = legend.col))

  plt <- plotly::layout(plt,
                        yaxis = list(title = ylab,
                                     # showline = axes.visible,
                                     # mirror = axes.mirrored,
                                     range = ylim,
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE,
                                     automargin = automargin.y),
                        xaxis = list(title = xlab,
                                     # showline = axes.visible,
                                     # mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     automargin = automargin.x),
                        barmode = barmode,  # group works without actual groups too
                        title = list(text = main,
                                     font = list(family = theme$font.family,
                                                 size = font.size,
                                                 color = main.col),
                                     xref = 'paper',
                                     x = theme$main.adj),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend)

  # hline ====
  if (!is.null(hline)) {
    hline.col <- recycle(hline.col, hline)
    hline.width <- recycle(hline.width, hline)
    hline.dash <- recycle(hline.dash, hline)
    hlinel <- lapply(seq_along(hline), function(i) {
      list(type = "line",
           x0 = 0, x1 = 1, xref = "paper",
           y0 = hline[i], y1 = hline[i],
           line = list(color = hline.col[i],
                       width = hline.width[i],
                       dash = hline.dash[i]))
    })
    plt <- plotly::layout(plt, shapes = hlinel)
  }

  # Padding
  plt$sizingPolicy$padding <- padding

  # Config
  plt <- plotly::config(plt,
                        displaylogo = FALSE,
                        displayModeBar = displayModeBar)

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height,
                         format = tools::file_ext(file), out_file = filename)
  }

  plt

} # rtemis::dplot3.bar.R
