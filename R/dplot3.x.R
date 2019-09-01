# dplot3.x.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

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
#' @param zerolines Logical: If TRUE: draw lines at y = 0. Default = FALSE
#' @param histnorm Character: NULL, "percent", "probability", "density", "probability density"
#' @param histfunc Character: "count", "sum", "avg", "min", "max". Default = "count"
#' @param hist.n.bins Integer: Number of bins to use if type = "histogram". Default = 20
#' @param ridge.sharex Logical: If TRUE, draw single x-axis when \code{mode = "ridge"}. Default = TRUE
#' @param ridge.y.labs Lofical: If TRUE, show individual y labs when \code{mode = "ridge"}. Default = FALSE
#' @param ridge.order.on.mean Logical: If TRUE, order groups by mean value when \code{mode = "ridge"}. Defaul = TRUE.
#' Turn to FALSE, if, for example, groups are ordered by date or similar.
#' @param width Float: Force plot size to this width. Default = NULL, i.e. fill available space
#' @param height Float: Force plot size to this height. Default = NULL, i.e. fill available space
#' @author Efstathios D. Gennatas
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
                     bg = NULL,
                     plot.bg = NULL,
                     theme = getOption("rt.theme", "light"),
                     palette = getOption("rt.palette", "rtCol1"),
                     axes.square = FALSE,
                     zero.lines = TRUE,
                     group.names = NULL,
                     font.size = 16,
                     font.alpha = .8,
                     font.col = NULL,
                     font.family = "Helvetica Neue",
                     main.col = NULL,
                     axes.col = NULL,
                     labs.col = NULL,
                     grid.col = NULL,
                     grid.lwd = 1,
                     grid.alpha = .8,
                     tick.col = NULL,
                     legend = NULL,
                     legend.xy = c(0, 1),
                     legend.col = NULL,
                     legend.bg = "#FFFFFF00",
                     legend.border.col = "#FFFFFF00",
                     margin = list(b = 50, l = 50, t = 50, r = 20),
                     padding = 0,
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
                     width = NULL,
                     height = NULL,
                     filename = NULL,
                     file.width = 500,
                     file.height = 500) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  type <- match.arg(type)
  mode <- match.arg(mode)
  if (!is.null(main)) main <- paste0("<b>", main, "</b>")

  # [ DATA ] ====

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

  # Themes ====
  # Defaults: no box
  axes.visible <- FALSE
  axes.mirrored <- FALSE

  if (theme %in% c("lightgrid", "darkgrid")) {
    grid <- TRUE
  } else {
    grid <- FALSE
  }
  if (theme == "lightgrid") {
    theme <- "light"
    if (is.null(plot.bg)) plot.bg <- plotly::toRGB("gray90")
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "rgba(255,255,255,1)"
    if (is.null(tick.col)) tick.col <- "rgba(0,0,0,1)"
  }
  if (theme == "darkgrid") {
    theme <- "dark"
    if (is.null(plot.bg)) plot.bg <- plotly::toRGB("gray15")
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
    if (is.null(tick.col)) tick.col <- "rgba(255,255,255,1)"
  }
  themes <- c("light", "dark", "lightbox", "darkbox")
  if (!theme %in% themes) {
    warning(paste(theme, "is not an accepted option; defaulting to \"light\""))
    theme <- "light"
  }

  if (theme == "light") {
    if (is.null(bg)) bg <- "white"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray10")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray10")
    if (is.null(main.col)) main.col <- "rgba(0,0,0,1)"
  } else if (theme == "dark") {
    if (is.null(bg)) bg <- "black"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray90")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray90")
    if (is.null(main.col)) main.col <- "rgba(255,255,255,1)"
    if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
    # gen.col <- "white"
  } else if (theme == "lightbox") {
    axes.visible <- axes.mirrored <- TRUE
    if (is.null(bg)) bg <- "rgba(255,255,255,1)"
    if (is.null(plot.bg)) plot.bg <- "rgba(255,255,255,1)"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray10")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray10")
    if (is.null(main.col)) main.col <- "rgba(0,0,0,1)"
    if (is.null(grid.col)) grid.col <- "rgba(255,255,255,1)"
    # gen.col <- "black"
  } else if (theme == "darkbox") {
    axes.visible <- axes.mirrored <- TRUE
    if (is.null(bg)) bg <- "rgba(0,0,0,1)"
    if (is.null(plot.bg)) plot.bg <- "rgba(0,0,0,1)"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray90")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray90")
    if (is.null(main.col)) main.col <- "rgba(255,255,255,1)"
    if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
    # gen.col <- "white"
  }

  # '- Axis font ====
  f <- list(family = font.family,
            size = font.size,
            color = labs.col)

  # '- Tick font ====
  tickfont <- list(family = font.family,
                   size = font.size,
                   color = tick.col)

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # [ SIZE ] ====
  if (axes.square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # Ridge ====
  if (mode == "ridge") {
    axis <- list(showline = axes.visible,
                 mirror = axes.mirrored,
                 showgrid = grid,
                 gridcolor = grid.col,
                 gridwidth = grid.lwd,
                 tickcolor = tick.col,
                 tickfont = tickfont,
                 zeroline = zerolines)
    ridge.groups <- if (ridge.order.on.mean) order(sapply(x, mean), decreasing = TRUE) else seq_len(n.groups)
  }

  # [ plotly ] ====
  # z <- if (mode == "overlap") rep(1, n.groups) else seq_len(n.groups)
  # plt <- vector("list", n.groups)

  .text <- lapply(x, function(i) paste("mean =", ddSci(mean(i)),
                                       "\nsd =", ddSci(sd(i))))

  # '- { Density } ====
  if (type ==  "density") {
    if (is.null(ylab)) ylab <- "Density"
    xl.density <- lapply(x, density)

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
                                     showlegend = legend)
      }
      plt <- plotly::layout(plt, barmode = barmode)
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
                                                              height = height) %>%
                      plotly::layout(xaxis = axis,
                                     yaxis = c(list(title = list(text = .names[i],
                                                                 font = f)),
                                               axis)))
    }
  }

  # return(plt)
  if (mode == "ridge") plt <- plotly::subplot(plt, nrows = n.groups,
                                              shareX = ridge.sharex,
                                              # shareY = ridge.sharey,
                                              titleY = ridge.y.labs)

  # [ Layout ] ====
  # '- layout ====
  .legend <- list(x = legend.xy[1],
                  y = legend.xy[2],
                  font = list(family = font.family,
                              size = font.size,
                              color = legend.col),
                  bgcolor = legend.bg,
                  bordercolor = legend.border.col)

  plt <- plotly::layout(plt,
                        xaxis = list(title = list(text = xlab,
                                                  font = f),
                                     showline = axes.visible,
                                     mirror = axes.mirrored,
                                     showgrid = grid,
                                     gridcolor = grid.col,
                                     gridwidth = grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE),
                        title = list(text = main,
                                     font = list(family = font.family,
                                                 size = font.size,
                                                 color = main.col)),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend)

  if (mode == "overlap") {
    plt <- plotly::layout(plt,
                          yaxis = list(title = list(text = ylab,
                                                    font = f),
                                       showline = axes.visible,
                                       mirror = axes.mirrored,
                                       showgrid = grid,
                                       gridcolor = grid.col,
                                       gridwidth = grid.lwd,
                                       tickcolor = tick.col,
                                       tickfont = tickfont,
                                       zeroline = zerolines))
  }

  # Set padding
  plt$sizingPolicy$padding <- padding

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height, out_file = filename)
  }
  plt

}  # rtemis::dplot3.x
