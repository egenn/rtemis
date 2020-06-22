# dplot3.box.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Interactive Boxplots & Violin plots
#'
#' Draw interactive boxplots or violin plots using \code{plotly}
#'
#' Any non-numeric variable in \code{x} will be removed
#'
#' @param x Vector or List of vectors: Input
#' @param main Character: Plot title. Default = NULL
#' @param xlab Character: x-axis label. Default = NULL
#' @param ylab  Character: y-axis label. Default = NULL
#' @param col Color, vector: Color for boxes. Default NULL, which will draw colors from \code{palette}
#' @param alpha Float (0, 1]: Transparency for box colors. Default = .8
#' @param bg Color: Background color. Default = "white"
#' @param plot.bg Color: Background color for plot area. Default = "white"
#' @param theme Character: THeme to use: "light", "dark", "lightgrid", "darkgrid". Default = "lightgrid"
#' @param palette Character: Name of `rtemis` palette to use. Default = "rtCol1". Only used if \code{col = NULL}
#' @param boxmode Character: Type of box plot to make: "group", "relative", "stack", "overlay". Default = "group". Use
#' "relative" for stacked boxes, which handles negative values correctly, unlike "stack", as of writing.
#' @param group.names Character, vector, length = NROW(x): Group names. Default = NULL, which uses \code{rownames(x)}
#' @param feature.names Character, vector, length = NCOL(x): Feature names. Default = NULL, which uses
#' \code{colnames(x)}
#' @param font.size  Float: Font size for all labels. Default = 16
#' @param font.alpha Float (0, 1]: Transparency for fonts. Default = .8
#' @param font.col Color: Font color. Default = "black"
#' @param font.family String: Font family to use. Default = "Helvetica Neue"
#' @param main.col Color: Title color. Default = NULL, determined by theme
#' @param axes.col Color: Axes color. Default = NULL, determined, by theme
#' @param labs.col Color: Labels' color. Default = NULL, determined by theme
#' @param grid.col Color: Grid color. Default = "gray85"
#' @param grid.lwd Float: Grid line width. Default = 1
#' @param grid.alpha Float (0, 1]: Transparency for \code{grid.col}. Default = .8
#' @param tick.col Color: Color for ticks and tick labels. Default = NULL, determined, by theme
#' @param legend Logical: If TRUE, draw legend. Default = TRUE
#' @param legend.col Color: Legend text color. Default = NULL, determined by theme
#' @param legend.xy Float, vector, length 2: Relative x, y position for legend. Default = NULL, which places
#' the legend top right beside the plot area. For example, c(0, 1) places the legend top left within the plot area
#' @param margin Named list: plot margins. Default = \code{list(t = 35)}
#'
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.box(VADeaths)
#' }

dplot3.box <-  function(x,
                        groups = "cols",
                        type = c("box", "violin"),
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        col = NULL,
                        alpha = .6,
                        bg = NULL,
                        plot.bg = NULL,
                        theme = getOption("rt.theme", "lightgrid"),
                        palette = getOption("rt.palette", "rtCol1"),
                        boxmode = c("group", "relative", "stack", "overlay"),
                        violin.box = TRUE,
                        group.names = NULL,
                        font.size = 16,
                        font.alpha = .8,
                        font.col = NULL,
                        # font.family = "Helvetica Neue",
                        main.col = NULL,
                        axes.col = NULL,
                        labs.col = NULL,
                        grid.col = NULL,
                        grid.lwd = 1,
                        grid.alpha = .8,
                        tick.col = NULL,
                        legend = FALSE,
                        legend.col = NULL,
                        legend.xy = NULL,
                        margin = list(t = 35),
                        padding = 0,
                        filename = NULL,
                        file.width = 500,
                        file.height = 500, ...) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  x <- as.data.frame(x)
  boxmode <- match.arg(boxmode)
  type <- match.arg(type)
  main <- paste0("<b>", main, "</b>")
  if (!is.list(x)) x <- list(x)

  # Remove non-numeric vectors
  which.nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  if (length(which.nonnum) > 0) x[[which.nonnum]] <- NULL

  n.groups <- length(x)
  .group.names <- group.names
  if (is.null(.group.names)) {
    .group.names <- names(x)
    if (is.null(.group.names)) .group.names <- paste0("Feature", seq(n.groups))
  }

  # if (!is.null(colnames(dat))) {
  #   .feature.names <- colnames(dat)
  # } else {
  #   .feature.names <- paste0("Feature", seq(NCOL(dat)))
  # }

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) col <- palette[seq_len(n.groups)]
  if (length(col) < n.groups) col <- rep(col, n.groups/length(col))

  # [ THEME ] ====
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

  # plotly ====
  args <- list(y = x[[1]],
               color = plotly::toRGB(col[1], alpha),
               type = type,
               name = .group.names[1],
               line = list(color = plotly::toRGB(col[1])),
               fillcolor = plotly::toRGB(col[1], alpha),
               marker = list(color = plotly::toRGB(col[1], alpha)))
  if (type == "violin") args$box <- list(visible = violin.box)
  plt <- do.call(plotly::plot_ly, args)
  if (n.groups > 1) {
    for (i in seq_len(n.groups)[-1]) plt <- plotly::add_trace(plt, y = x[[i]],
                                                              color = plotly::toRGB(col[i], alpha),
                                                              name = .group.names[i],
                                                              line = list(color = plotly::toRGB(col[i])),
                                                              fillcolor = plotly::toRGB(col[i], alpha),
                                                              marker = list(color = plotly::toRGB(col[i], alpha)))
  }

  # '- layout ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)
  tickfont <- list(family = theme$font.family,
                   size = font.size,
                   color = tick.col)
  .legend <- list(x = legend.xy[1],
                  y = legend.xy[2],
                  font = list(family = theme$font.family,
                              size = font.size,
                              color = legend.col))

  plt <- plotly::layout(plt,
                        yaxis = list(title = ylab,
                                     showline = axes.visible,
                                     mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = grid.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE),
                        xaxis = list(title = xlab,
                                     showline = axes.visible,
                                     mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = FALSE,
                                     # gridcolor = grid.col,
                                     # gridwidth = theme$grid.lwd,
                                     tickcolor = grid.col,
                                     tickfont = tickfont),
                        # boxmode = boxmode,  # CHECK: online docs show this, but gives error
                        title = list(text = main,
                                     font = list(family = font.family,
                                                 size = font.size,
                                                 color = main.col),
                                     xref = 'paper',
                                     x = theme$main.adj),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend)

  # Set padding
  plt$sizingPolicy$padding <- padding

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height, out_file = filename)
  }

  plt

} # rtemis::dplot3.box.R
