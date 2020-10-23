# dplot3.xy.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.lambdamd.org

#' Interactive Scatter Plots
#'
#' Draw interactive scatter plots using \code{plotly}
#'
#' use theme$tick.labels.col for both tick color and tick label color - this may change
#' @inheritParams dplot3.bar
#' @inheritParams mplot3.xy
#' @param x Numeric, vector/data.frame/list: x-axis data. If y is NULL and \code{NCOL(x) > 1}, first two columns used as
#' \code{x} and \code{y}, respectively
#' @param y Numeric, vector/data.frame/list: y-axis data
#' @param rsq Logical: If TRUE, print R-squared values in legend if \code{fit} is set
#' @param mode Character, vector: "markers", "lines", "markers+lines". Default = "markers"
#' @param order.on.x Logical: If TRUE, order \code{x} and \code{y} on \code{x}. Default = NULL, which becomes
#' \code{TRUE} if \code{mode} includes lines.
#' @param axes.square Logical: If TRUE: draw a square plot to fill the graphic device.
#' Default = FALSE. Note: If TRUE, the device size at time of call is captured and height and width are set so as
#' to draw the largest square available. This means that resizing the device window will not automatically resize the
#' plot.
#' @param legend Logical: If TRUE, draw legend. Default = NULL, which will be set to TRUE if there are more than 1
#' groups, or \code{fit} is set
#' @param legend.orientation Character: "v": vertical, or "h": horizontal. Default = "v"
#' @param zerolines Logical: If TRUE: draw lines at y = 0. Default = FALSE
#' @param histnorm Character: NULL, "percent", "probability", "density", "probability density"
#' @param histfunc Character: "count", "sum", "avg", "min", "max". Default = "count"
#' @param hist.n.bins Integer: Number of bins to use if type = "histogram". Default = 20
#' @param width Float: Force plot size to this width. Default = NULL, i.e. fill available space
#' @param height Float: Force plot size to this height. Default = NULL, i.e. fill available space
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.xy(iris$Sepal.Length, iris$Petal.Length, fit = "gam", se.fit = TRUE, group = iris$Species)
#' }

dplot3.xy <- function(x, y = NULL,
                      fit = NULL,
                      se.fit = FALSE,
                      se.times = 1.96,
                      cluster =  NULL,
                      cluster.params = list(k = 2),
                      group = NULL,
                      formula = NULL,
                      rsq = TRUE,
                      # rsq.pval = FALSE,
                      mode = "markers",
                      order.on.x = NULL,
                      main = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      col = NULL,
                      alpha = .66,
                      bg = NULL,
                      plot.bg = NULL,
                      theme = getOption("rt.theme", "lightgrid"),
                      palette = getOption("rt.palette", "rtCol1"),
                      axes.square = FALSE,
                      group.names = NULL,
                      font.size = 16,
                      font.alpha = .8,
                      font.col = NULL,
                      marker.col = NULL,
                      fit.col = NULL,
                      fit.alpha = .8,
                      fit.lwd = 2.5,
                      se.col = NULL,
                      se.alpha = .4,
                      legend = NULL,
                      legend.xy = c(0, 1),
                      legend.xanchor = "left",
                      legend.yanchor = "auto",
                      legend.orientation = "v",
                      legend.col = NULL,
                      legend.bg = "#FFFFFF00",
                      legend.border.col = "#FFFFFF00",
                      legend.borderwidth = 0,
                      legend.group.gap = 0,
                      margin = list(t = 35),
                      zerolines = TRUE,
                      mod.params = list(),
                      width = NULL,
                      height = NULL,
                      padding = 0,
                      trace = 0,
                      filename = NULL,
                      file.width = 500,
                      file.height = 500, ...) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) > 1) {
    if (is.null(xlab)) xlab <- labelify(colnames(x)[1])
    if (is.null(ylab)) ylab <- labelify(colnames(x)[2])
    y <- x[, 2]
    x <- x[, 1]
  }
  if (!is.null(fit)) if (fit == "none") fit <- NULL # easier to work with shiny
  if (is.logical(fit)) if (fit) fit <- "GAM"
  if (is.null(fit)) se.fit <- FALSE
  if (!is.null(fit)) fit <- toupper(fit)
  if (!is.null(main)) main <- paste0("<b>", main, "</b>")
  .mode <- mode
  .names <- group.names

  # fit & formula
  if (!is.null(formula)) fit <- "NLS"

  # order.on.x ====
  if (is.null(order.on.x)) {
    order.on.x <- if (!is.null(fit) | any(grepl("lines", mode))) TRUE else FALSE
  }

  # [ CLUSTER ] ====
  if (!is.null(cluster)) {
    group <- suppressWarnings(do.call(clustSelect(cluster),
                                      c(list(x = data.frame(x, y),
                                             verbose = trace > 0),
                                        cluster.params))$clusters.train)
    group <- paste("Cluster", group)
  }

  # [ DATA ] ====
  # xlab, ylab ====
  # The gsubs remove all text up to and including a "$" symbol if present
  if (is.null(xlab)) {
    if (is.list(x)) xlab <- "x" else xlab <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  }
  if (!is.null(y) & is.null(ylab)) {
    if (is.list(y)) ylab <- "y" else ylab <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  }

  # '- Group ====
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group)
    y <- split(y, group)
    if (is.null(group.names)) group.names <- levels(group)
    names(x) <- names(y) <- .names <- group.names
  }

  # Convert to lists ====
  x <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  y <- if (!is.null(y) & !is.list(y)) as.list(as.data.frame(y)) else y
  if (length(x) == 1 & length(y) > 1) {
    x <- rep(x, length(y))
    .names <- names(y)
  }
  if (length(y) == 1 & length(x) > 1) {
    y <- rep(y, length(x))
    .names <- names(x)
  }
  n.groups <- length(x)

  legend <- if (is.null(legend) & n.groups == 1 & is.null(fit)) FALSE else TRUE

  if (length(.mode) < n.groups) .mode <- c(.mode, rep(tail(.mode)[1], n.groups - length(.mode)))

  # if (is.null(legend)) legend <- n.groups > 1
  if (is.null(.names)) {
    if (n.groups > 1) {
      .names <- paste("Group", seq_len(n.groups))
    } else {
      .names <- if (!is.null(fit)) fit else NULL
    }
  }

  # Reorder ====
  if (order.on.x) {
    index <- lapply(x, order)
    x <- lapply(seq(x), function(i) x[[i]][index[[i]]])
    y <- lapply(seq(x), function(i) y[[i]][index[[i]]])
  }

  # s.e. fit ====
  if (se.fit) {
    if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
      warning(paste("Standard error of the fit not available for", fit, "- try LM, LOESS, GAM, or NW"))
      se.fit <- FALSE
    }
  }

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) col <- palette[seq_len(n.groups)]
  if (length(col) < n.groups) col <- rep(col, n.groups/length(col))

  # remove:
  # # Convert inputs to RGB
  # bg <- plotly::toRGB(bg)
  # plot.bg <- plotly::toRGB(plot.bg)
  # font.col <- plotly::toRGB(font.col)
  # # marker.col <- plotly::toRGB(marker.col)
  # # fit.col <- plotly::toRGB(fit.col)
  # # se.col <- plotly::toRGB(se.col)
  # tick.col <- plotly::toRGB(tick.col)
  # grid.col <- plotly::toRGB(grid.col)

  # [ THEME ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  bg <- plotly::toRGB(theme$bg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col, theme$grid.alpha)
  tick.col <- plotly::toRGB(theme$tick.labels.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)
  if (!theme$axes.visible) tick.col <- labs.col <- "transparent"

  # marker.col, se.col ===
  if (is.null(marker.col)) {
    marker.col <- if (!is.null(fit) & n.groups == 1) as.list(rep("gray50", n.groups)) else col
  }

  if (!is.null(fit)) {
    if (is.null(fit.col)) fit.col <- col
  }

  if (se.fit & is.null(se.col)) {
    se.col <- col
  }

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # [ SIZE ] ====
  if (axes.square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # [ fitted & se.fit ] ====
  # If plotting se bands, need to include (fitted +/- se.times * se) in the axis limits
  if (se.fit) se <- list() else se <- NULL
  if (rsq) .rsq <- list() else .rsq <- NULL
  # if (rsq.pval) rsqp <- list() else rsqp <- NULL
  if (!is.null(fit)) {
    learner <- modSelect(fit, fn = FALSE)
    fitted <- list()
    fitted.text <- character()
    for (i in seq_len(n.groups)) {
      x1 <- x[[i]]
      y1 <- y[[i]]
      learner.args <- c(list(x = x1, y = y1, verbose = trace > 0),
                        mod.params,
                        list(...))
      if (fit == "NLS") learner.args <- c(learner.args,
                                          list(formula = formula, save.func = TRUE))
      mod <- do.call(learner, learner.args)
      fitted[[i]] <- fitted(mod)
      if (se.fit) se[[i]] <- se(mod)
      fitted.text[i] <- switch(fit,
                               NLS = mod$extra$model,
                               NLA = mod$mod$formula,
                               fit)
      if (rsq) {
        fitted.text[i] <- paste0(fitted.text[i],
                                 if (n.groups == 1) " (" else " ",
                                 "R<sup>2</sup> = ", ddSci(mod$error.train$Rsq),
                                 if (n.groups == 1) ")")

      }
      # if (rsq.pval) {
      #   if (fit  %in% c("LM", "GLM")) {
      #     rsqp[[i]] <- paste0(ddSci(mod$error.train$Rsq), " (",
      #                          ddSci(summary(mod$mod)$coefficients[2, 4]), ")")
      #   } else if (fit == "GAM") {
      #     rsqp[[i]] <- paste0(ddSci(mod$error.train$Rsq), " (",
      #                          ddSci(summary(mod$mod)$s.pv), ")")
      #   }
      # }
    }
  }

  # [ plotly ] ====
  # if (n.groups > 1) {
  #   legendgroup = .names
  # }

  if (!is.null(fit)) .names <- paste0(.names, " (", fitted.text, ")")

  plt <- plotly::plot_ly(width = width,
                         height = height,)
  for (i in seq_len(n.groups)) {
    # '- { Scatter } ====
    plt <- plotly::add_trace(plt, x = x[[i]],
                             y = y[[i]],
                             type = "scatter",
                             mode = .mode[i],
                             # fillcolor = plotly::toRGB(col[[i]], alpha),
                             name = if (n.groups > 1) .names[i] else "Raw",
                             # text = .text[[i]],
                             # hoverinfo = "text",
                             marker = if (grepl("markers", .mode[i])) list(color = plotly::toRGB(marker.col[[i]], alpha = alpha)) else NULL,
                             line = if (grepl("lines", .mode[i])) list(color = plotly::toRGB(marker.col[[i]], alpha = alpha)) else NULL,
                             legendgroup = if (n.groups > 1) .names[i] else "Raw",
                             showlegend = legend)
    if (se.fit) {
      # '- { SE band } ====
      plt <- plotly::add_trace(plt,
                               x = x[[i]],
                               y = fitted[[i]] + se.times * se[[i]],
                               type = "scatter",
                               mode = "lines",
                               line = list(color = "transparent"),
                               legendgroup = .names[i],
                               showlegend = FALSE,
                               hoverinfo = "none",
                               inherit = FALSE)
      plt <- plotly::add_trace(plt, x = x[[i]],
                               y = fitted[[i]] - se.times * se[[i]],
                               type = "scatter",
                               mode = "lines",
                               fill = "tonexty",
                               fillcolor = plotly::toRGB(se.col[[i]], alpha = se.alpha),
                               line = list(color = "transparent"),
                               # name = shade.name,
                               legendgroup = .names[i],
                               showlegend = FALSE,
                               hoverinfo = "none",
                               inherit = FALSE)
    }
    if (!is.null(fit)) {
      # '- { Fitted line } ====
      lfit = list(color = plotly::toRGB(fit.col[[i]], alpha = fit.alpha),
                  width = fit.lwd)
      plt <- plotly::add_trace(plt, x = x[[i]], y = fitted[[i]],
                               type = "scatter",
                               mode = "lines",
                               line = lfit,
                               name = fitted.text[i],
                               legendgroup = .names[i],
                               showlegend = if (legend & n.groups == 1) TRUE else FALSE,
                               inherit = FALSE)
    }
  }

  # [ Layout ] ====
  # '- layout ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)
  tickfont <- list(family = theme$font.family,
                   size = font.size,
                   color = tick.col)
  .legend <- list(x = legend.xy[1],
                  xanchor = legend.xanchor,
                  y = legend.xy[2],
                  yanchor = legend.yanchor,
                  font = list(family = theme$font.family,
                              size = font.size,
                              color = legend.col),
                  orientation = legend.orientation,
                  bgcolor = plotly::toRGB(legend.bg),
                  bordercolor = plotly::toRGB(legend.border.col),
                  borderwidth = legend.borderwidth,
                  tracegroupgap = legend.group.gap)

  plt <- plotly::layout(plt,
                        yaxis = list(title = ylab,
                                     showline = FALSE,
                                     # mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = theme$zerolines),
                        xaxis = list(title = xlab,
                                     showline = FALSE,
                                     # mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = zerolines),
                        # barmode = barmode,  # group works without actual groups too
                        # title = main,
                        title = list(text = main,
                                     font = list(family = theme$font.family,
                                                 size = font.size,
                                                 color = main.col),
                                     xref = 'paper',
                                     x = theme$main.adj),
                        # titlefont = list(),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend)

  # Padding
  plt$sizingPolicy$padding <- padding

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height, out_file = filename)
  }

  plt

}  # rtemis::dplot3.xy
