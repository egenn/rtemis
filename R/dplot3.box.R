# dplot3.box.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Interactive Boxplots & Violin plots
#'
#' Draw interactive boxplots or violin plots using \pkg{plotly}
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
#' @param palette Character: Name of \pkg{rtemis} palette to use. Default = "rtCol1". Only used if \code{col = NULL}
#' @param boxmode Character: Type of box plot to make: "group", "relative", "stack", "overlay". Default = "group". Use
#' "relative" for stacked boxes, which handles negative values correctly, unlike "stack", as of writing.
#' @param quartilemethod Character: "linear", "exclusive", "inclusive"
#' @param boxpoints Character or FALSE: "all", "suspectedoutliers", "outliers"
#' See \url{https://plotly.com/r/box-plots/#choosing-the-algorithm-for-computing-quartiles}
#' @param group.names Character, vector, length = NROW(x): Group names. Default = NULL, which uses \code{rownames(x)}
#' @param order.by.fn Function: If defined, order boxes by increasing value of this function
#' (e.g. median). Default = NULL
#' @param feature.names Character, vector, length = NCOL(x): Feature names. Default = NULL, which uses
#' \code{colnames(x)}
#' @param font.size  Float: Font size for all labels. Default = 16
#' @param legend Logical: If TRUE, draw legend. Default = TRUE
#' @param legend.col Color: Legend text color. Default = NULL, determined by theme
#' @param legend.xy Float, vector, length 2: Relative x, y position for legend. Default = NULL, which places
#' the legend top right beside the plot area. For example, c(0, 1) places the legend top left within the plot area
#' @param margin Named list: plot margins. Default = \code{list(t = 35)}
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.box(iris[, 1:4])
#' }

dplot3.box <-  function(x,
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
                        boxpoints = "outliers",
                        quartilemethod = "linear",
                        width = 0,
                        violin.box = TRUE,
                        group.names = NULL,
                        order.by.fn = NULL,
                        font.size = 16,
                        legend = FALSE,
                        legend.col = NULL,
                        legend.xy = NULL,
                        margin = list(t = 35, pad = 0),
                        automargin.x = TRUE,
                        automargin.y = TRUE,
                        displayModeBar = TRUE,
                        filename = NULL,
                        file.width = 500,
                        file.height = 500, ...) {

  # [ Dependencies ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
  boxmode <- match.arg(boxmode)
  type <- match.arg(type)
  main <- paste0("<b>", main, "</b>")
  # if (!is.list(x)) x <- list(x)
  # Convert vector to
  # convert to list whether data frame or matrix
  if (!is.list(x)) {
    # x is vector
    if (is.numeric(x)) {
      .names <- deparse(substitute(x))
      x <- list(x)
      names(x) <- .names
    } else {
      .names <- colnames(x)
      x <- lapply(seq(NCOL(x)), function(i) x[, i])
      names(x) <- .names
    }

  }

  # [ Order by fn ] ====
  if (!is.null(order.by.fn) && order.by.fn != "none") {
    if (is.list(x)) {
      .order <- order(sapply(x, order.by.fn, na.rm = TRUE))
      if (is.data.frame(x)) {
        x <- x[, .order]
      } else {
        x <- x[names(x)[.order]]
      }
    }
    if (!is.null(group.names)) group.names <- group.names[.order]
  }

  # Remove non-numeric vectors
  which.nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  if (length(which.nonnum) > 0) x[[which.nonnum]] <- NULL

  n.groups <- length(x)
  .group.names <- group.names
  if (is.null(.group.names)) {
    .group.names <- names(x)
    if (is.null(.group.names)) .group.names <- paste0("Feature", seq(n.groups))
  }

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) col <- recycle(palette, seq(n.groups))
  # if (length(palette) < n.groups) col <- rep(col, n.groups/length(col))
  if (length(col) < n.groups) col <- recycle(col, seq(n.groups))
  if (!is.null(order.by.fn) && order.by.fn != "none") {
    col <- col[.order]
  }

  # [ Theme ] ====
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
  # axes.col <- plotly::toRGB(theme$axes.col)

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # plotly ====
  args <- list(y = x[[1]],
               type = type,
               name = .group.names[1],
               line = list(color = plotly::toRGB(col[1])),
               fillcolor = plotly::toRGB(col[1], alpha),
               marker = list(color = plotly::toRGB(col[1], alpha)))
  if (type == "box") {
    args <- c(args, list(quartilemethod = quartilemethod,
                         boxpoints = boxpoints))
  }
  if (type == "violin") args$box <- list(visible = violin.box)
  plt <- do.call(plotly::plot_ly, args)
  if (n.groups > 1) {
    for (i in seq_len(n.groups)[-1]) {
      plt <- plotly::add_trace(plt, y = x[[i]],
                               name = .group.names[i],
                               line = list(color = plotly::toRGB(col[i])),
                               fillcolor = plotly::toRGB(col[i], alpha),
                               marker = list(color = plotly::toRGB(col[i], alpha)))
      }
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
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = grid.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE,
                                     automargin = automargin.y),
                        xaxis = list(title = xlab,
                                     titlefont = f,
                                     showgrid = FALSE,
                                     tickcolor = grid.col,
                                     tickfont = tickfont,
                                     automargin = automargin.x),
                        # boxmode = boxmode,  # CHECK: online docs show this, but gives error
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

  # Config
  plt <- plotly::config(plt,
                        displaylogo = FALSE,
                        displayModeBar = displayModeBar)

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height, out_file = filename)
  }

  plt

} # rtemis::dplot3.box.R
