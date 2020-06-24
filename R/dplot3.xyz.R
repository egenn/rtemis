# dplot3.xyz.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Interactive 3D Plots
#'
#' Draw interactive 3D plots using \code{plotly}
#'
#' @inheritParams dplot3.xy
#' @param z Numeric, vector/data.frame/list: z-axis data
#' @param zlab Character: z-axis label
#' @param margin Numeric, named list: Margins for top, bottom, left, right.
#' Default = \code{list(t = 30, b = 0, l = 0, r = 0}
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.xyz(iris, group = iris$Species, theme = "darkgrid")
#' }

dplot3.xyz <- function(x, y = NULL, z = NULL,
                       fit = NULL,
                       cluster =  NULL,
                       cluster.params = list(k = 2),
                       group = NULL,
                       formula = NULL,
                       rsq = TRUE,
                       mode = "markers",
                       order.on.x = NULL,
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       zlab = NULL,
                       col = NULL,
                       alpha = .8,
                       bg = NULL,
                       plot.bg = NULL,
                       theme = getOption("rt.theme", "lightgrid"),
                       palette = getOption("rt.palette", "rtCol1"),
                       axes.square = FALSE,
                       group.names = NULL,
                       font.size = 16,
                       font.alpha = .8,
                       font.col = NULL,
                       font.family = "Helvetica Neue",
                       marker.col = NULL,
                       fit.col = NULL,
                       fit.alpha = .7,
                       fit.lwd = 2.5,
                       # se.col = NULL,
                       # se.alpha = .4,
                       main.col = NULL,
                       axes.col = NULL,
                       labs.col = NULL,
                       grid.col = NULL,
                       grid.lwd = 1,
                       grid.alpha = .8,
                       tick.col = NULL,
                       tick.font.size = 12,
                       spike.col = NULL,
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
                       margin = list(t = 30, b = 0, l = 0, r = 0),
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
  if (is.null(y) & is.null(z) & NCOL(x) > 2) {
    .colnames <- labelify(colnames(x))
    y <- x[, 2]
    z <- x[, 3]
    x <- x[, 1]
    if (is.null(xlab)) xlab <- .colnames[1]
    if (is.null(ylab)) ylab <- .colnames[2]
    if (is.null(zlab)) zlab <- .colnames[3]
  }
  if (!is.null(main)) main <- paste0("<b>", main, "</b>")
  if (!is.null(fit)) fit <- toupper(fit)
  .mode <- mode
  .names <- group.names

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
  if (!is.null(z) & is.null(zlab)) {
    if (is.list(z)) zlab <- "z" else zlab <- labelify(gsub(".*\\$", "", deparse(substitute(z))))
  }

  # '- Group ====
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group)
    y <- split(y, group)
    z <- split(z, group)
    if (is.null(group.names)) group.names <- levels(group)
    names(x) <- names(y) <- names(z) <- .names <- group.names
  }

  # Convert to lists ====
  x <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  y <- if (!is.null(y) & !is.list(y)) as.list(as.data.frame(y)) else y
  z <- if (!is.null(z) & !is.list(z)) as.list(as.data.frame(z)) else z
  if (length(x) == 1 & length(y) > 1) {
    x <- rep(x, length(y))
    .names <- names(y)
  }
  if (length(y) == 1 & length(x) > 1) {
    y <- rep(y, length(x))
    .names <- names(x)
  }
  if (length(z) == 1 & length(x) > 1) {
    z <- rep(z, length(x))
    .names <- names(x)
  }
  n.groups <- length(x)

  # legend <- if (is.null(legend) & n.groups == 1 & is.null(fit)) FALSE else TRUE
  legend <- if (is.null(legend) & n.groups == 1) FALSE else TRUE

  if (length(.mode) < n.groups) .mode <- c(.mode, rep(tail(.mode)[1], n.groups - length(.mode)))

  # if (is.null(legend)) legend <- n.groups > 1
  if (is.null(.names)) {
    if (n.groups > 1) {
      .names <- paste("Group", seq_len(n.groups))
    } else {
      .names <- if (!is.null(fit)) fit else NULL
      .names <- NULL
    }
  }

  # Reorder ====
  if (order.on.x) {
    index <- lapply(x, order)
    x <- lapply(seq(x), function(i) x[[i]][index[[i]]])
    y <- lapply(seq(x), function(i) y[[i]][index[[i]]])
    z <- lapply(seq(x), function(i) z[[i]][index[[i]]])
  }

  # s.e. fit ====
  se.fit <- FALSE
  # if (se.fit) {
  #   if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
  #     warning(paste("Standard error of the fit not available for", fit, "- try LM, LOESS, GAM, or NW"))
  #     se.fit <- FALSE
  #   }
  # }

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) col <- palette[seq_len(n.groups)]
  if (length(col) < n.groups) col <- rep(col, n.groups/length(col))

  # Convert inputs to RGB
  bg <- plotly::toRGB(bg)
  plot.bg <- plotly::toRGB(plot.bg)
  font.col <- plotly::toRGB(font.col)
  # marker.col <- plotly::toRGB(marker.col)
  fit.col <- plotly::toRGB(fit.col)
  # se.col <- plotly::toRGB(se.col)
  tick.col <- plotly::toRGB(tick.col)
  grid.col <- plotly::toRGB(grid.col)
  spike.col <- plotly::toRGB(spike.col)

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
    # if (is.null(grid.col)) grid.col <- "rgba(255,255,255,1)"
    if (is.null(grid.col)) grid.col <- plotly::toRGB("gray70")
    if (is.null(tick.col)) tick.col <- "rgba(0,0,0,1)"
  }
  if (theme == "darkgrid") {
    theme <- "dark"
    if (is.null(plot.bg)) plot.bg <- plotly::toRGB("gray15")
    grid <- TRUE
    # if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
    if (is.null(grid.col)) grid.col <- plotly::toRGB("gray35")
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
    if (is.null(spike.col)) spike.col <- plotly::toRGB("gray30")
  } else if (theme == "dark") {
    if (is.null(bg)) bg <- "black"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray90")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray90")
    if (is.null(main.col)) main.col <- "rgba(255,255,255,1)"
    if (is.null(spike.col)) spike.col <-  plotly::toRGB("gray70")
  } else if (theme == "lightbox") {
    axes.visible <- axes.mirrored <- TRUE
    if (is.null(bg)) bg <- "rgba(255,255,255,1)"
    if (is.null(plot.bg)) plot.bg <- "rgba(255,255,255,1)"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray10")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray10")
    if (is.null(main.col)) main.col <- "rgba(0,0,0,1)"
    if (is.null(spike.col)) spike.col <- plotly::toRGB("gray30")
  } else if (theme == "darkbox") {
    axes.visible <- axes.mirrored <- TRUE
    if (is.null(bg)) bg <- "rgba(0,0,0,1)"
    if (is.null(plot.bg)) plot.bg <- "rgba(0,0,0,1)"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray90")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray90")
    if (is.null(main.col)) main.col <- "rgba(255,255,255,1)"
    if (is.null(spike.col)) spike.col <-  plotly::toRGB("gray70")
  }

  # marker.col, se.col ===
  if (is.null(marker.col)) {
    marker.col <- if (!is.null(fit) & n.groups == 1) as.list(rep("gray50", n.groups)) else col
  }

  if (!is.null(fit)) {
    if (is.null(fit.col)) fit.col <- col
  }

  # if (se.fit & is.null(se.col)) {
  #   se.col <- col
  # }

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # [ SIZE ] ====
  if (axes.square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # return(list(x = x, y = y, z = z))
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
      feat1 <- data.frame(x[[i]], y[[i]])
      y1 <- z[[i]]
      learner.args <- c(list(x = feat1, y = y1, verbose = trace > 0),
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
    }
  }

  # [ plotly ] ====

  # if (!is.null(fit)) .names <- paste0(.names, " (", fitted.text, ")")

  plt <- plotly::plot_ly(width = width,
                         height = height,)
  for (i in seq_len(n.groups)) {
    # '- { Scatter } ====
    plt <- plotly::add_trace(plt,
                             x = x[[i]],
                             y = y[[i]],
                             z = z[[i]],
                             type = "scatter3d",
                             mode = .mode[i],
                             # fillcolor = plotly::toRGB(col[[i]], alpha),
                             # name = if (n.groups > 1) .names[i] else "Raw",
                             name = .names[i],
                             # text = .text[[i]],
                             # hoverinfo = "text",
                             marker = if (grepl("markers", .mode[i])) list(color = plotly::toRGB(marker.col[[i]], alpha = alpha)) else NULL,
                             line = if (grepl("lines", .mode[i])) list(color = plotly::toRGB(marker.col[[i]], alpha = alpha)) else NULL,
                             legendgroup = if (n.groups > 1) .names[i] else "Raw",
                             showlegend = legend)
    # if (se.fit) {
    #   # '- { SE band } ====
    #   plt <- plotly::add_trace(plt,
    #                            x = x[[i]],
    #                            y = fitted[[i]] + se.times * se[[i]],
    #                            type = "scatter",
    #                            mode = "lines",
    #                            line = list(color = "transparent"),
    #                            legendgroup = .names[i],
    #                            showlegend = FALSE,
    #                            hoverinfo = "none",
    #                            inherit = FALSE)
    #   plt <- plotly::add_trace(plt, x = x[[i]],
    #                            y = fitted[[i]] - se.times * se[[i]],
    #                            type = "scatter",
    #                            mode = "lines",
    #                            fill = "tonexty",
    #                            fillcolor = plotly::toRGB(se.col[[i]], alpha = se.alpha),
    #                            line = list(color = "transparent"),
    #                            # name = shade.name,
    #                            legendgroup = .names[i],
    #                            showlegend = FALSE,
    #                            hoverinfo = "none",
    #                            inherit = FALSE)
    # }

    # return(list(plt = plt, x = x, y = y, z = z,
    #             fitted = fitted, fitted.text = fitted.txt,
    #             fit.col = fit.col,
    #             legend =  legend,
    #             n.groups = n.groups))
    if (!is.null(fit)) {
      # '- { Fitted mesh } ====
      # lfit = list(color = plotly::toRGB(fit.col[[i]], alpha = fit.alpha),
      #             width = fit.lwd)
      plt <- plotly::add_trace(plt,
                               x = x[[i]],
                               y = y[[i]],
                               z = fitted[[i]],
                               type = "mesh3d",
                               opacity = fit.alpha,
                               name = fitted.text[i],
                               # legendgroup = .names[i],
                               # showlegend = if (legend & n.groups == 1) TRUE else FALSE,
                               inherit = FALSE,
                               showscale = FALSE,
                               intensity =  1,
                               colorscale = list(c(0, plotly::toRGB(fit.col[[i]])), c(1, plotly::toRGB(fit.col[[i]]))))
    }
  }

  # return(plt)
  # [ Layout ] ====
  # '- layout ====
  f <- list(family = font.family,
            size = font.size,
            color = labs.col)
  tickfont <- list(family = font.family,
                   size = tick.font.size,
                   color = tick.col)
  .legend <- list(x = legend.xy[1],
                  xanchor = legend.xanchor,
                  y = legend.xy[2],
                  yanchor = legend.yanchor,
                  font = list(family = font.family,
                              size = font.size,
                              color = legend.col),
                  orientation = legend.orientation,
                  bgcolor = plotly::toRGB(legend.bg),
                  bordercolor = plotly::toRGB(legend.border.col),
                  borderwidth = legend.borderwidth,
                  tracegroupgap = legend.group.gap)

  plt <- plotly::layout(plt,
                        scene = list(
                          yaxis = list(title = ylab,
                                       showline = axes.visible,
                                       mirror = axes.mirrored,
                                       titlefont = f,
                                       showgrid = grid,
                                       gridcolor = grid.col,
                                       gridwidth = grid.lwd,
                                       tickcolor = tick.col,
                                       tickfont = tickfont,
                                       zeroline = FALSE,
                                       spikecolor = spike.col),
                          xaxis = list(title = xlab,
                                       showline = axes.visible,
                                       mirror = axes.mirrored,
                                       titlefont = f,
                                       showgrid = grid,
                                       gridcolor = grid.col,
                                       gridwidth = grid.lwd,
                                       tickcolor = tick.col,
                                       tickfont = tickfont,
                                       zeroline = FALSE,
                                       spikecolor = spike.col),
                          zaxis = list(title = zlab,
                                       showline = axes.visible,
                                       mirror = axes.mirrored,
                                       titlefont = f,
                                       showgrid = grid,
                                       gridcolor = grid.col,
                                       gridwidth = grid.lwd,
                                       tickcolor = tick.col,
                                       tickfont = tickfont,
                                       zeroline = FALSE,
                                       spikecolor = spike.col)
                        ),
                        title = list(text = main,
                                     font = list(family = font.family,
                                                 size = font.size,
                                                 color = main.col)),
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


}
