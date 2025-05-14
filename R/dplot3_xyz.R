# dplot3_xyz.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Interactive 3D Plots
#'
#' Draw interactive 3D plots using `plotly`
#'
#' Note that `dplot3_xyz` uses the theme's `plot.bg` as `grid.col`
#' @inheritParams dplot3_xy
#' @param z Numeric, vector/data.frame/list: z-axis data
#' @param zlab Character: z-axis label
#' @param bg Background color
#' @param plot.bg Plot background color
#' @param tick.font.size Numeric: Tick font size
#' @param spike.col Spike lines color
#' @param margin Numeric, named list: Margins for top, bottom, left, right.
#' Default = `list(t = 30, b = 0, l = 0, r = 0`
#' @param padding Numeric: Graph padding.
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3_xyz(iris, group = iris$Species, theme = "darkgrid")
#' }
dplot3_xyz <- function(
  x,
  y = NULL,
  z = NULL,
  fit = NULL,
  cluster = NULL,
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
  theme = rtTheme,
  palette = rtPalette,
  axes.square = FALSE,
  group.names = NULL,
  font.size = 16,
  marker.col = NULL,
  marker.size = 8,
  fit.col = NULL,
  fit.alpha = .7,
  fit.lwd = 2.5,
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
  fit.params = list(),
  width = NULL,
  height = NULL,
  padding = 0,
  displayModeBar = TRUE,
  modeBar.file.format = "svg",
  trace = 0,
  filename = NULL,
  file.width = 500,
  file.height = 500,
  file.scale = 1,
  ...
) {
  # Dependencies ----
  dependency_check("plotly")

  # Arguments ----
  if (is.null(y) && is.null(z) && NCOL(x) > 2) {
    .colnames <- labelify(colnames(x))
    y <- x[, 2]
    z <- x[, 3]
    x <- x[, 1]
    if (is.null(xlab)) xlab <- .colnames[1]
    if (is.null(ylab)) ylab <- .colnames[2]
    if (is.null(zlab)) zlab <- .colnames[3]
  }
  if (!is.null(main)) main <- paste0("<b>", main, "</b>")
  if (!is.null(fit)) if (fit == "none") fit <- NULL # easier to work with shiny
  if (!is.null(fit)) fit <- toupper(fit)
  .mode <- mode
  .names <- group.names

  # order.on.x ----
  if (is.null(order.on.x)) {
    order.on.x <- if (!is.null(fit) || any(grepl("lines", mode))) TRUE else
      FALSE
  }

  # CLUSTER ----
  if (!is.null(cluster)) {
    group <- suppressWarnings(
      do.call(
        select_clust(cluster),
        c(
          list(
            x = data.frame(x, y),
            verbose = trace > 0
          ),
          cluster.params
        )
      )$clusters.train
    )
    group <- paste("Cluster", group)
  }

  # Data ----
  # xlab, ylab ----
  # The gsubs remove all text up to and including a "$" symbol if present
  if (is.null(xlab)) {
    if (is.list(x)) xlab <- "x" else
      xlab <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  }
  if (!is.null(y) && is.null(ylab)) {
    if (is.list(y)) ylab <- "y" else
      ylab <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  }
  if (!is.null(z) && is.null(zlab)) {
    if (is.list(z)) zlab <- "z" else
      zlab <- labelify(gsub(".*\\$", "", deparse(substitute(z))))
  }

  # '- Group ----
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group, drop = TRUE)
    y <- split(y, group, drop = TRUE)
    z <- split(z, group, drop = TRUE)
    if (is.null(group.names)) group.names <- levels(droplevels(group))
    names(x) <- names(y) <- names(z) <- .names <- group.names
  }

  # Try to get names from list or data frame inputs
  if (is.list(y) || NCOL(y) > 1) {
    if (is.null(.names) && !is.null(names(y))) .names <- names(y)
  }
  if (is.list(x) || NCOL(x) > 1) {
    if (is.null(.names) && !is.null(names(x))) .names <- names(x)
  }
  if (is.list(z) || NCOL(z) > 1) {
    if (is.null(.names) && !is.null(names(z))) .names <- names(z)
  }

  # Convert to lists ----
  x <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  y <- if (!is.null(y) && !is.list(y)) as.list(as.data.frame(y)) else y
  z <- if (!is.null(z) && !is.list(z)) as.list(as.data.frame(z)) else z
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    .names <- names(y)
  }
  if (length(y) == 1 && length(x) > 1) {
    y <- rep(y, length(x))
    .names <- names(x)
  }
  if (length(z) == 1 && length(x) > 1) {
    z <- rep(z, length(x))
    .names <- names(x)
  }
  n.groups <- length(x)

  # legend <- if (is.null(legend) & n.groups == 1 & is.null(fit)) FALSE else TRUE
  legend <- if (is.null(legend) && n.groups == 1) FALSE else TRUE

  if (length(.mode) < n.groups)
    .mode <- c(.mode, rep(tail(.mode)[1], n.groups - length(.mode)))

  # if (is.null(legend)) legend <- n.groups > 1
  if (is.null(.names)) {
    if (n.groups > 1) {
      .names <- paste("Group", seq_len(n.groups))
    } else {
      .names <- if (!is.null(fit)) fit else NULL
      .names <- NULL
    }
  }

  # Reorder ----
  if (order.on.x) {
    index <- lapply(x, order)
    x <- lapply(seq(x), function(i) x[[i]][index[[i]]])
    y <- lapply(seq(x), function(i) y[[i]][index[[i]]])
    z <- lapply(seq(x), function(i) z[[i]][index[[i]]])
  }

  # s.e. fit ----
  se.fit <- FALSE
  # if (se.fit) {
  #   if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
  #     warning(paste("Standard error of the fit not available for", fit, "- try LM, LOESS, GAM, or NW"))
  #     se.fit <- FALSE
  #   }
  # }

  # Colors ----
  if (is.character(palette)) palette <- rtpalette(palette)
  if (is.null(col)) col <- palette[seq_len(n.groups)]
  if (length(col) < n.groups) col <- rep(col, n.groups / length(col))

  # Convert inputs to RGB
  spike.col <- plotly::toRGB(spike.col)

  # Theme ----
  axes.visible <- FALSE
  axes.mirrored <- FALSE
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
  tick.col <- plotly::toRGB(theme$tick.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)
  if (!theme$axes.visible) tick.col <- labs.col <- "transparent"

  # marker.col, se.col ===
  if (is.null(marker.col)) {
    marker.col <- if (!is.null(fit) && n.groups == 1)
      as.list(rep(theme$fg, n.groups)) else col
  }

  if (!is.null(fit)) {
    if (is.null(fit.col)) fit.col <- col
  }

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # Size ----
  if (axes.square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # fitted & se.fit ----
  # If plotting se bands, need to include (fitted +/- se.times * se) in the axis limits
  if (se.fit) se <- list() else se <- NULL
  if (rsq) .rsq <- list() else .rsq <- NULL
  # if (rsq.pval) rsqp <- list() else rsqp <- NULL
  if (!is.null(fit)) {
    learner <- select_learn(fit, fn = FALSE)
    fitted <- list()
    fitted.text <- character()
    for (i in seq_len(n.groups)) {
      feat1 <- data.frame(x[[i]], y[[i]])
      y1 <- z[[i]]
      learner.args <- c(
        list(x = feat1, y = y1, verbose = trace > 0),
        fit.params,
        list(...)
      )
      if (fit == "NLS") {
        learner.args <- c(
          learner.args,
          list(formula = formula, save.func = TRUE)
        )
      }
      mod <- do.call(learner, learner.args)
      fitted[[i]] <- fitted(mod)
      if (se.fit) se[[i]] <- se(mod)
      fitted.text[i] <- switch(
        fit,
        NLS = mod$extra$model,
        NLA = mod$mod$formula,
        fit
      )
      if (rsq) {
        fitted.text[i] <- paste0(
          fitted.text[i],
          if (n.groups == 1) " (" else " ",
          "R<sup>2</sup> = ",
          ddSci(mod$error.train$Rsq),
          if (n.groups == 1) ")"
        )
      }
    }
  }

  # plotly ----
  plt <- plotly::plot_ly(
    width = width,
    height = height
  )
  for (i in seq_len(n.groups)) {
    # '- { Scatter } ----
    marker <- if (grepl("markers", .mode[i])) {
      list(
        color = plotly::toRGB(marker.col[[i]], alpha = alpha),
        size = marker.size
      )
    } else {
      NULL
    }
    plt <- plotly::add_trace(
      plt,
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
      # marker = if (grepl("markers", .mode[i])) list(color = plotly::toRGB(marker.col[[i]], alpha = alpha)) else NULL,
      marker = marker,
      line = if (grepl("lines", .mode[i]))
        list(color = plotly::toRGB(marker.col[[i]], alpha = alpha)) else NULL,
      legendgroup = if (n.groups > 1) .names[i] else "Raw",
      showlegend = legend
    )
    # if (se.fit) {
    #   # '- { SE band } ----
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

    if (!is.null(fit)) {
      # '- { Fitted mesh } ----
      plt <- plotly::add_trace(
        plt,
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
        intensity = 1,
        colorscale = list(
          c(0, plotly::toRGB(fit.col[[i]])),
          c(1, plotly::toRGB(fit.col[[i]]))
        )
      )
    }
  }

  # Layout ----
  # '- layout ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = labs.col
  )
  tickfont <- list(
    family = theme$font.family,
    size = tick.font.size,
    color = theme$tick.labels.col
  )
  .legend <- list(
    x = legend.xy[1],
    xanchor = legend.xanchor,
    y = legend.xy[2],
    yanchor = legend.yanchor,
    font = list(
      family = theme$font.family,
      size = font.size,
      color = legend.col
    ),
    orientation = legend.orientation,
    bgcolor = plotly::toRGB(legend.bg),
    bordercolor = plotly::toRGB(legend.border.col),
    borderwidth = legend.borderwidth,
    tracegroupgap = legend.group.gap
  )

  plt <- plotly::layout(
    plt,
    scene = list(
      yaxis = list(
        title = ylab,
        showline = axes.visible,
        mirror = axes.mirrored,
        titlefont = f,
        showgrid = theme$grid,
        gridcolor = grid.col,
        gridwidth = theme$grid.lwd,
        tickcolor = tick.col,
        tickfont = tickfont,
        zeroline = FALSE,
        spikecolor = spike.col
      ),
      xaxis = list(
        title = xlab,
        showline = axes.visible,
        mirror = axes.mirrored,
        titlefont = f,
        showgrid = theme$grid,
        gridcolor = grid.col,
        gridwidth = theme$grid.lwd,
        tickcolor = tick.col,
        tickfont = tickfont,
        zeroline = FALSE,
        spikecolor = spike.col
      ),
      zaxis = list(
        title = zlab,
        showline = axes.visible,
        mirror = axes.mirrored,
        titlefont = f,
        showgrid = theme$grid,
        gridcolor = grid.col,
        gridwidth = theme$grid.lwd,
        tickcolor = tick.col,
        tickfont = tickfont,
        zeroline = FALSE,
        spikecolor = spike.col
      )
    ),
    title = list(
      text = main,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = main.col
      )
    ),
    # titlefont = list(),
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin,
    showlegend = legend,
    legend = .legend
  )

  # Padding
  plt$sizingPolicy$padding <- padding
  # Config
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar.file.format,
      width = file.width,
      height = file.height
    )
  )

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file = file.path(filename),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  plt
} # rtemis::dplot3_xyz
