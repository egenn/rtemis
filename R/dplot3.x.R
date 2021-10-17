# dplot3.x.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org
# TIP: Use plotly::schema() to explore arguments/parameters to plotly functions
# todo: figure out why list is reordered with ridge

#' Interactive Univariate Plots
#'
#' Draw interactive univariate plots using \code{plotly}
#'
#' If input is data.frame, non-numeric variables will be removed
#' @inheritParams dplot3.bar
#' @param x Numeric, vector / data.frame /list: Input. If not a vector, each column of or each element
#' @param group Vector: Will be converted to factor; levels define group members. Default = NULL
#' @param axes.square Logical: If TRUE: draw a square plot to fill the graphic device.
#' Default = FALSE. Note: If TRUE, the device size at time of call is captured and height and width are set so as
#' to draw the largest square available. This means that resizing the device window will not automatically resize the
#' plot.
#' @param legend Logical: If TRUE, draw legend. Default = NULL, which will be set to TRUE if x is a list of more than
#' 1 element
#' @param legend.xy Float, vector, length 2: Relative x, y position for legend. Default = c(0, 1), which places the
#' legend top left within the plot area. Set to NULL to place legend top right beside the plot area
#' @param bargap Float: The gap between adjacent histogram bars in plot fraction. Default = .05
#' @param zerolines Logical: If TRUE: draw lines at y = 0. Default = FALSE
#' @param histnorm Character: NULL, "percent", "probability", "density", "probability density"
#' @param histfunc Character: "count", "sum", "avg", "min", "max". Default = "count"
#' @param hist.n.bins Integer: Number of bins to use if type = "histogram". Default = 20
#' @param ridge.sharex Logical: If TRUE, draw single x-axis when \code{mode = "ridge"}. Default = TRUE
#' @param ridge.y.labs Lofical: If TRUE, show individual y labs when \code{mode = "ridge"}. Default = FALSE
#' @param ridge.order.on.mean Logical: If TRUE, order groups by mean value when \code{mode = "ridge"}. Defaul = TRUE.
#' Turn to FALSE, if, for example, groups are ordered by date or similar.
#' @param vline Float: If defined, draw a vertical line at this x value. Default = NULL
#' @param vline.col Color for \code{vline}. Default = "#ff0000" (red)
#' @param vline.width Float: Width for \code{vline}. Default = 1
#' @param vline.dash Character: Type of line to draw: "solid", "dot", "dash", "longdash", "dashdot",
#' or "longdashdot"
#' @param text Character: If defined, add this text over the plot
#' @param text.x Float: x-coordinate for \code{text}
#' @param text.xref Character: "x": \code{text.x} refers to plot's x-axis; "paper": \code{text.x} refers to plotting area from 0-1
#' @param text.xanchor Character: "auto", "left", "center", "right"
#' @param text.yanchor Character: "auto", "top", "middle", "bottom"
#' @param text.yref Character: "y": \code{text.y} refers to plot's y-axis; "paper": \code{text.y} refers to plotting area from 0-1
#' @param text.col Color for \code{text}
#' @param width Float: Force plot size to this width. Default = NULL, i.e. fill available space
#' @param height Float: Force plot size to this height. Default = NULL, i.e. fill available space
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.x(iris)
#' dplot3.x(split(iris$Sepal.Length, iris$Species), xlab = "Sepal Length")
#' }

dplot3.x <- function(x,
                     type = c("density", "histogram"),
                     mode = c("overlap", "ridge"),
                     group = NULL,
                     main = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     col = NULL,
                     alpha = .75,
                     plot.bg = NULL,
                     theme = getOption("rt.theme", "lightgrid"),
                     palette = getOption("rt.palette", "rtCol1"),
                     axes.square = FALSE,
                     group.names = NULL,
                     font.size = 16,
                     font.alpha = .8,
                     legend = NULL,
                     legend.xy = c(0, 1),
                     legend.col = NULL,
                     legend.bg = "#FFFFFF00",
                     legend.border.col = "#FFFFFF00",
                     bargap = .05,
                     bingroup = 1,
                     vline = NULL,
                     vline.col = "#ff0000",
                     vline.width = 1,
                     text = NULL,
                     text.x = 1,
                     text.xref = "paper",
                     text.xanchor = "left",
                     text.y = 1,
                     text.yref = "paper",
                     text.yanchor = "top",
                     text.col = "#ff0000",
                     margin = list(b = 50, l = 50, t = 50, r = 20, pad = 0),
                     automargin.x = TRUE,
                     automargin.y = TRUE,
                     # padding = 0,
                     zerolines = FALSE,
                     histnorm = c("density", "percent", "probability",
                                  "probability density"),
                     histfunc = c("count", "sum", "avg", "min", "max"),
                     hist.n.bins = 20,
                     barmode = "overlay", # TODO: alternatives
                     ridge.sharex = TRUE,
                     # ridge.sharey = FALSE,
                     ridge.y.labs = FALSE,
                     ridge.order.on.mean = TRUE,
                     # axes.mirrored = FALSE,
                     displayModeBar = TRUE,
                     width = NULL,
                     height = NULL,
                     filename = NULL,
                     file.width = 500,
                     file.height = 500, ...) {

  # [ Dependencies ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
  type <- match.arg(type)
  mode <- match.arg(mode)
  if (!is.null(main)) main <- paste0("<b>", main, "</b>")
  .xname <- labelify(deparse(substitute(x)))

  # [ Data ] ====

  # '- Group ====
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- as.data.frame(x)
    x <- split(x, group)
    x <- sapply(x, as.vector)
    if (is.null(group.names)) group.names <- levels(group)
    names(x) <- .names <- group.names
  }

  if (!is.list(x)) x <- list(x)

  if (!is.list(x)) x <- list(x)
  n.groups <- length(x)

  if (n.groups == 1 & is.null(xlab)) {
    xlab <- .xname
  }

  # Remove non-numeric vectors
  which.nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  if (length(which.nonnum) > 0) x[[which.nonnum]] <- NULL

  if (is.null(legend)) legend <- length(x) > 1
  if (!is.null(group.names)) {
    .names <- group.names
  } else {
    .names <- labelify(names(x))
  }
  if (is.null(.names)) .names <- paste("Feature", seq_along(x))

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  n.groups <- length(x)
  if (is.null(col)) col <- palette[seq_len(n.groups)]

  if (length(col) < n.groups) col <- rep(col, n.groups/length(col))

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
  if (!theme$axes.visible) tick.col <- labs.col <- "transparent"


  # '- Axis font ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)

  # '- Tick font ====
  tickfont <- list(family = theme$font.family,
                   size = font.size,
                   color = tick.col)

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # [ Size ] ====
  if (axes.square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # Ridge ====
  if (mode == "ridge") {
    axis <- list(showline = FALSE,
                 # mirror = axes.mirrored,
                 showgrid = theme$grid,
                 gridcolor = grid.col,
                 gridwidth = theme$grid.lwd,
                 tickcolor = tick.col,
                 tickfont = tickfont,
                 zeroline = zerolines)
    ridge.groups <- if (ridge.order.on.mean) order(sapply(x, mean), decreasing = TRUE) else seq_len(n.groups)
  }

  # [ plotly ] ====
  # z <- if (mode == "overlap") rep(1, n.groups) else seq_len(n.groups)
  # plt <- vector("list", n.groups)

  .text <- lapply(x, function(i) paste("mean =", ddSci(mean(i, na.rm = TRUE)),
                                       "\nsd =", ddSci(sd(i, na.rm = TRUE))))

  # '- { Density } ====
  if (type ==  "density") {
    if (is.null(ylab)) ylab <- "Density"
    xl.density <- lapply(x, density, na.rm = TRUE)

    if (mode == "overlap") {
      # '- Density overlap ====
      plt <- plotly::plot_ly(width = width,
                             height = height)
      for (i in seq_len(n.groups)) {
        plt <- plotly::add_trace(plt, x = xl.density[[i]]$x,
                                 y = xl.density[[i]]$y,
                                 type = "scatter",
                                 mode = "none",
                                 fill = 'tozeroy',
                                 fillcolor = plotly::toRGB(col[[i]], alpha),
                                 name = .names[i],
                                 text = .text[[i]],
                                 hoverinfo = "text",
                                 showlegend = legend)
      }
    } else {
      # '- Density ridge ====
      plt <- lapply(ridge.groups, function(i) plotly::plot_ly(x = xl.density[[i]]$x,
                                                              y = xl.density[[i]]$y,
                                                              type = "scatter",
                                                              mode = "none",
                                                              fill = 'tozeroy',
                                                              fillcolor = plotly::toRGB(col[[i]], alpha),
                                                              name = .names[i],
                                                              text = .text[[i]],
                                                              hoverinfo = "text",
                                                              showlegend = legend,
                                                              width = width,
                                                              height = height) %>%
                      plotly::layout(xaxis = axis,
                                     yaxis = c(list(title = list(text = .names[i],
                                                                 font = f)),
                                               axis)))
    }

  } # End mode == "density"

  # '- { Histogram } ====
  if (type == "histogram") {
    histnorm <- match.arg(histnorm)
    histfunc <- match.arg(histfunc)
    # if (is.null(ylab)) ylab <- "Count"

    if (mode == "overlap") {
      # '-  Histogram overlap ====
      plt <- plotly::plot_ly(width = width,
                             height = height)
      for (i in seq_len(n.groups)) {
        plt <- plotly::add_histogram(plt, x = x[[i]],
                                     marker = list(color = plotly::toRGB(col[i], alpha)),
                                     name = .names[i],
                                     text = .text[[i]],
                                     hoverinfo = "text",
                                     histnorm = histnorm,
                                     histfunc = histfunc,
                                     nbinsx = hist.n.bins,
                                     showlegend = legend,
                                     bingroup = bingroup)
      }
      plt <- plotly::layout(plt,
                            barmode = barmode,
                            bargap = bargap)
    } else {
      # '- Histogram ridge ====
      plt <- lapply(ridge.groups, function(i) plotly::plot_ly(x = x[[i]],
                                                              type = "histogram",
                                                              histnorm = histnorm,
                                                              histfunc = histfunc,
                                                              nbinsx = hist.n.bins,
                                                              marker = list(color = plotly::toRGB(col[i], alpha)),
                                                              name = .names[i],
                                                              text = .text[[i]],
                                                              hoverinfo = "text",
                                                              showlegend = legend,
                                                              width = width,
                                                              height = height,
                                                              bingroup = bingroup) %>%
                      plotly::layout(xaxis = axis,
                                     yaxis = c(list(title = list(text = .names[i],
                                                                 font = f)),
                                               axis),
                                     bargap = bargap))
    }
  }

  if (mode == "ridge") plt <- plotly::subplot(plt, nrows = n.groups,
                                              shareX = ridge.sharex,
                                              # shareY = ridge.sharey,
                                              titleY = ridge.y.labs)

  # [ Layout ] ====
  # '- layout ====
  .legend <- list(x = legend.xy[1],
                  y = legend.xy[2],
                  font = list(family = theme$font.family,
                              size = font.size,
                              color = legend.col),
                  bgcolor = legend.bg,
                  bordercolor = legend.border.col)

  plt <- plotly::layout(plt,
                        xaxis = list(title = list(text = xlab,
                                                  font = f),
                                     showline = FALSE,
                                     # mirror = axes.mirrored,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE,
                                     automargin = automargin.x),
                        title = list(text = main,
                                     font = list(family = theme$font.family,
                                                 size = font.size,
                                                 color = main.col),
                                     xref = "paper",
                                     x = theme$main.adj),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend)

  if (mode == "overlap") {
    plt <- plotly::layout(plt,
                          yaxis = list(title = list(text = ylab,
                                                    font = f),
                                       showline = FALSE,
                                       # mirror = axes.mirrored,
                                       showgrid = theme$grid,
                                       gridcolor = grid.col,
                                       gridwidth = theme$grid.lwd,
                                       tickcolor = tick.col,
                                       tickfont = tickfont,
                                       zeroline = zerolines,
                                       automargin = automargin.y))
  }

  # vline ====
  if (!is.null(vline)) {
    plt <- plotly::layout(plt, shapes = list(list(type = "line",
                                                  x0 = vline, x1 = vline,
                                                  y0 = 0, y1 = 1, yref = "paper",
                                                  line = list(color = vline.col,
                                                              width = vline.width,
                                                              dash = vline.dash))))
  }

  # text ====
  if (!is.null(text)) {
    plt <- plotly::layout(plt,
                          annotations = list(
                            text = text,
                            x = text.x,
                            xref = text.xref,
                            xanchor = text.xanchor,
                            y = text.y,
                            yref = text.yref,
                            yanchor = text.yanchor,
                            font = list(color = text.col,
                                        family = theme$font.family,
                                        size = font.size),
                            showarrow = FALSE))
  }

  # Config
  plt <- plotly::config(plt,
                        displaylogo = FALSE,
                        displayModeBar = displayModeBar)

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height,
                         format = tools::file_ext(filename), out_file = filename)
  }
  plt

}  # rtemis::dplot3.x
