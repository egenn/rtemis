# dplot3_xy.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Interactive Scatter Plots
#'
#' Draw interactive scatter plots using `plotly`
#'
#' use theme$tick.labels.col for both tick color and tick label color - this may change
#' @inheritParams dplot3_bar
#' @inheritParams dplot3_x
#' @inheritParams mplot3_xy
#' @param x Numeric, vector/data.frame/list: x-axis data. If y is NULL and
#' `NCOL(x) > 1`, first two columns used as `x` and `y`, respectively
#' @param y Numeric, vector/data.frame/list: y-axis data
#' @param col Color for markers. Default=NULL, which will draw colors from palette
#' @param rsq Logical: If TRUE, print R-squared values in legend if `fit` is set
#' @param mode Character, vector: "markers", "lines", "markers+lines".
#' @param order.on.x Logical: If TRUE, order `x` and `y` on `x`. Default = NULL,
#'  which becomes `TRUE` if `mode` includes lines.
#' @param axes.square Logical: If TRUE: draw a square plot to fill the graphic device.
#' Default = FALSE. Note: If TRUE, the device size at time of call is captured and
#' height and width are set so as to draw the largest square available. This means that
#' resizing the device window will not automatically resize the plot.
#' @param legend Logical: If TRUE, draw legend. Default = NULL, which will be set to
#' TRUE if there are more than 1 groups, or `fit` is set
#' @param hovertext List of character vectors with hovertext to include for each group
#' of markers
#' @param width Numeric: Force plot size to this width. Default = NULL, i.e. fill
#' available space
#' @param height Numeric: Force plot size to this height. Default = NULL, i.e. fill
#' available space
#' @param legend.borderwidth Numeric: Border width for legend
#' @param legend.group.gap Numeric: Gap between legend groups
#' @param x.showspikes Logical: If TRUE, show spikes on x-axis
#' @param y.showspikes Logical: If TRUE, show spikes on y-axis
#' @param spikedash Character: Dash type for spikes
#' @param spikemode Character: "across", "toaxis", "marker", or any combination of those
#' joined by `+`, e.g. "toaxis+across+marker"
#' @param spikesnap Character: "data", "cursor", "hovered data"
#' @param spikecolor Color for spikes
#' @param spikethickness Numeric: Thickness of spikes
#' @param subtitle Character: Subtitle
#' @param main.y Numeric: Y position of main title
#' @param main.yanchor Character: "top", "middle", "bottom"
#' @param subtitle.x Numeric: X position of subtitle relative to paper
#' @param subtitle.y Numeric: Y position of subtitle relative to paper
#' @param subtitle.xref Character: "paper", "x", "y"
#' @param subtitle.yref Character: "paper", "x", "y"
#' @param subtitle.xanchor Character: "left", "center", "right"
#' @param subtitle.yanchor Character: "top", "middle", "bottom"
#' @param scrollZoom Logical: If TRUE, enable scroll zoom
#' @param include.fit.name Logical: If TRUE, include fit name in legend.
#' @param marker.size Numeric: Marker size.
#' @param symbol Character: Marker symbol.
#' @param scatter.type Character: "scatter", "scattergl", "scatter3d", "scatterternary",
#' "scatterpolar", "scattermapbox",
#' @param show.marginal.x Logical: If TRUE, add marginal distribution line markers on x-axis
#' @param show.marginal.y Logical: If TRUE, add marginal distribution line markers on y-axis
#' @param marginal.x Numeric: Data whose distribution will be shown on x-axis. Only
#' specify if different from `x`
#' @param marginal.y Numeric: Data whose distribution will be shown on y-axis. Only
#' specify if different from `y`
#' @param marginal.x.y Numeric: Y position of marginal markers on x-axis
#' @param marginal.y.x Numeric: X position of marginal markers on y-axis
#' @param marginal.col Color for marginal markers
#' @param marginal.alpha Numeric: Alpha for marginal markers
#' @param marginal.size Numeric: Size of marginal markers
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3_xy(iris$Sepal.Length, iris$Petal.Length,
#'   fit = "gam", se.fit = TRUE, group = iris$Species
#' )
#' }
#'
dplot3_xy <- function(
  x,
  y = NULL,
  fit = NULL,
  se.fit = FALSE,
  se.times = 1.96,
  include.fit.name = TRUE,
  cluster = NULL,
  cluster.params = list(k = 2),
  group = NULL,
  formula = NULL,
  rsq = TRUE,
  # rsq.pval = FALSE,
  mode = "markers",
  order.on.x = NULL,
  main = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = NULL,
  # bg = NULL,
  # plot.bg = NULL,
  theme = rtTheme,
  palette = rtPalette,
  axes.square = FALSE,
  group.names = NULL,
  font.size = 16,
  marker.col = NULL,
  marker.size = 8,
  symbol = "circle",
  fit.col = NULL,
  fit.alpha = .8,
  fit.lwd = 2.5,
  se.col = NULL,
  se.alpha = .4,
  scatter.type = "scatter",
  # Marginal plots
  show.marginal.x = FALSE,
  show.marginal.y = FALSE,
  marginal.x = x,
  marginal.y = y,
  marginal.x.y = NULL,
  marginal.y.x = NULL,
  marginal.col = NULL,
  marginal.alpha = .333,
  marginal.size = 10,
  legend = NULL,
  legend.xy = c(0, .98),
  legend.xanchor = "left",
  legend.yanchor = "auto",
  legend.orientation = "v",
  legend.col = NULL,
  legend.bg = "#FFFFFF00",
  legend.border.col = "#FFFFFF00",
  legend.borderwidth = 0,
  legend.group.gap = 0,
  x.showspikes = FALSE,
  y.showspikes = FALSE,
  spikedash = "solid",
  spikemode = "across",
  spikesnap = "hovered data",
  spikecolor = NULL,
  spikethickness = 1,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  main.y = 1,
  main.yanchor = "bottom",
  subtitle.x = 0.02,
  subtitle.y = 0.99,
  subtitle.xref = "paper",
  subtitle.yref = "paper",
  subtitle.xanchor = "left",
  subtitle.yanchor = "top",
  automargin.x = TRUE,
  automargin.y = TRUE,
  xlim = NULL,
  ylim = NULL,
  axes.equal = FALSE,
  diagonal = FALSE,
  diagonal.col = NULL,
  diagonal.alpha = .2,
  fit.params = list(),
  vline = NULL,
  vline.col = theme$fg,
  vline.width = 1,
  vline.dash = "dot",
  hline = NULL,
  hline.col = theme$fg,
  hline.width = 1,
  hline.dash = "dot",
  hovertext = NULL,
  width = NULL,
  height = NULL,
  # config
  displayModeBar = TRUE,
  modeBar.file.format = "svg",
  scrollZoom = TRUE,
  # write to file
  filename = NULL,
  file.width = 500,
  file.height = 500,
  file.scale = 1,
  trace = 0,
  ...
) {
  # Dependencies ----
  dependency_check("plotly")

  # Arguments ----
  xname <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  yname <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  if (is.null(y) && NCOL(x) > 1) {
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

  if (se.fit) {
    if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
      warning(paste(
        "Standard error of the fit not available for",
        fit,
        "- try LM, LOESS, GAM, or NW"
      ))
      se.fit <- FALSE
    }
  }

  # order.on.x ----
  if (is.null(order.on.x)) {
    order.on.x <- if (!is.null(fit) || any(grepl("lines", mode))) TRUE else
      FALSE
  }

  # Cluster ----
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
    if (is.list(x)) xlab <- "x" else xlab <- xname
  }
  if (!is.null(y) && is.null(ylab)) {
    if (is.list(y)) ylab <- "y" else ylab <- yname
  }

  # Group ----
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group, drop = TRUE)
    y <- split(y, group, drop = TRUE)
    if (is.null(group.names)) group.names <- levels(droplevels(group))
    names(x) <- names(y) <- .names <- group.names
    if (!is.null(hovertext)) hovertext <- split(hovertext, group, drop = TRUE)
  }

  # Try to get names from list or data frame inputs
  if (is.list(y) || NCOL(y) > 1) {
    if (is.null(.names) && !is.null(names(y))) .names <- names(y)
  }
  if (is.list(x) || NCOL(x) > 1) {
    if (is.null(.names) && !is.null(names(x))) .names <- names(x)
  }

  # Data to lists ----
  x <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  y <- if (!is.null(y) && !is.list(y)) as.list(as.data.frame(y)) else y
  hovertext <- if (!is.null(hovertext) && !is.list(hovertext)) {
    as.list(as.data.frame(hovertext))
  } else {
    hovertext
  }
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    .names <- names(y)
  }
  if (length(y) == 1 && length(x) > 1) {
    y <- rep(y, length(x))
    .names <- names(x)
  }
  if (!is.null(hovertext) && length(hovertext) == 1 && length(x) > 1) {
    hovertext <- rep(hovertext, length(x))
  }
  n.groups <- length(x)

  if (is.null(legend)) {
    legend <- if (n.groups == 1 && is.null(fit)) FALSE else TRUE
  }

  if (length(.mode) < n.groups)
    .mode <- c(.mode, rep(tail(.mode)[1], n.groups - length(.mode)))

  # if (is.null(legend)) legend <- n.groups > 1
  if (is.null(.names)) {
    if (n.groups > 1) {
      .names <- paste("Group", seq_len(n.groups))
    } else {
      # .names <- if (!is.null(fit)) fit else NULL
      .names <- xname
    }
  }

  # Marginal data ----
  if (show.marginal.x && is.null(marginal.x)) marginal.x <- x
  if (show.marginal.y && is.null(marginal.y)) marginal.y <- y

  # Reorder ----
  if (order.on.x) {
    index <- lapply(x, order)
    x <- lapply(seq(x), \(i) x[[i]][index[[i]]])
    y <- lapply(seq(x), \(i) y[[i]][index[[i]]])
    if (!is.null(hovertext)) {
      hovertext <- lapply(seq(x), \(i) hovertext[[i]][index[[i]]])
    }
  }

  if (!is.null(fit) && fit == "LOESS") {
    id <- !is.na(x)
  }

  # Colors ----
  if (is.character(palette)) palette <- rtpalette(palette)
  if (is.null(col)) col <- palette[seq_len(n.groups)]
  if (length(col) < n.groups) col <- rep(col, n.groups / length(col))
  if (is.null(alpha)) alpha <- autoalpha(max(lengths(x)))

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
  if (diagonal) {
    if (is.null(diagonal.col)) {
      diagonal.col <- theme$fg
    }
    diagonal.col <- adjustcolor(diagonal.col, diagonal.alpha)
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
    marker.col <- if (!is.null(fit) && n.groups == 1) {
      as.list(rep(theme$fg, n.groups))
    } else {
      col
    }
  }

  if (!is.null(fit)) {
    if (is.null(fit.col)) fit.col <- col
  }

  if (se.fit && is.null(se.col)) {
    se.col <- col
  }

  if (is.null(legend.col)) legend.col <- labs.col
  if (is.null(spikecolor)) spikecolor <- theme$fg

  # Size ----
  # if (axes.square) {
  # width <- height <- min(dev.size("px"))
  # }

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
      x1 <- x[[i]]
      y1 <- y[[i]]
      learner.args <- c(
        list(x = x1, y = y1, verbose = trace > 0),
        fit.params
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
      if (include.fit.name) {
        fitted.text[i] <- switch(
          fit,
          NLS = mod$extra$model,
          NLA = mod$mod$formula,
          fit
        )
      } else {
        fitted.text[i] <- ""
      }
      if (rsq) {
        fitted.text[i] <- paste0(
          fitted.text[i],
          if (n.groups == 1) " (" else " ",
          "R<sup>2</sup> = ",
          ddSci(mod$error.train$Rsq),
          if (n.groups == 1) ")"
        )
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

  # Axes Limits ----
  if (axes.equal) {
    if (is.null(xlim)) {
      xlim <- getlim(unlist(x), "r", .06)
    }
    if (is.null(ylim)) {
      ylim <- getlim(unlist(y), "r", .06)
      if (is.list(fitted) && !is.list(se)) {
        ylim.hi <- max(unlist(fitted))
        ylim.lo <- min(unlist(fitted))
        ylim <- range(ylim.lo, ylim.hi, y)
      }
      if (is.list(se)) {
        ylim.hi <- max(unlist(lapply(
          seq_along(fitted),
          function(i) {
            as.data.frame(fitted[[i]]) +
              se.times * as.data.frame(se[[i]])
          }
        )))
        ylim.lo <- min(unlist(lapply(
          seq_along(fitted),
          function(i) {
            as.data.frame(fitted[[i]]) -
              se.times * as.data.frame(se[[i]])
          }
        )))
        ylim <- range(ylim.lo, ylim.hi, y)
      }
      # if (is.list(error.y)) {
      #   error.y.hi <- lapply(seq(y), function(i) yl[[i]] + error.y[[i]])
      #   error.y.lo <- lapply(seq(y), function(i) yl[[i]] - error.y[[i]])
      #   ylim <- range(error.y.lo, error.y.hi, ylim)
      # }
    }

    xlim <- ylim <- range(xlim, ylim)
  } # /axes.equal

  # unlist will coerce Dates to numeric, also don't want padding
  if (is.null(xlim) && !inherits(x[[1]], "Date")) {
    xlim <- getlim(unlist(x), "r", .06)
  }
  if (is.null(ylim) && !inherits(y[[1]], "Date")) {
    ylim <- getlim(unlist(y), "r", .06)
  }

  # plotly ----
  if (!is.null(fit) && rsq) {
    if (!include.fit.name) fitted.text <- gsub("^ ", "", fitted.text)
    if (n.groups > 1) {
      .names <- paste0(.names, " (", fitted.text, ")")
    }
  }

  plt <- plotly::plot_ly(
    width = width,
    height = height
  )

  if (diagonal) {
    lo <- min(xlim[1], ylim[1])
    hi <- max(xlim[2], ylim[2])
    plt <- plotly::layout(
      plt,
      shapes = list(
        type = "line",
        x0 = lo,
        x1 = hi,
        y0 = lo,
        y1 = hi,
        line = list(color = diagonal.col)
      )
    )
  }

  for (i in seq_len(n.groups)) {
    ## { Scatter } ----
    marker <- if (grepl("markers", .mode[i])) {
      list(
        color = plotly::toRGB(marker.col[[i]], alpha = alpha),
        size = marker.size,
        symbol = symbol
      )
    } else {
      NULL
    }
    plt <- plotly::add_trace(
      plt,
      x = x[[i]],
      y = y[[i]],
      type = scatter.type,
      mode = .mode[i],
      # fillcolor = plotly::toRGB(col[[i]], alpha),
      name = .names[i],
      # text = .text[[i]],
      # hoverinfo = "text",
      text = hovertext[[i]],
      marker = marker,
      line = if (grepl("lines", .mode[i])) {
        list(color = plotly::toRGB(marker.col[[i]], alpha = alpha))
      } else {
        NULL
      },
      legendgroup = .names[i],
      showlegend = legend
    )
    # Marginal plots ----
    # Add marginal plots by plotting short vertical markers on the x and y axes
    if (show.marginal.x) {
      if (is.null(marginal.col)) {
        marginal.col <- plotly::toRGB(marker.col, alpha = marginal.alpha)
      }
      if (is.null(marginal.x.y)) marginal.x.y <- ylim[1]
      # Extend ylim to include marginal markers
      ylim[1] <- ylim[1] - 0.02 * diff(ylim)
      for (i in seq_len(n.groups)) {
        plt <- plotly::add_trace(
          plt,
          x = marginal.x[[i]],
          y = rep(marginal.x.y, length(marginal.x[[i]])),
          type = "scatter",
          mode = "markers",
          marker = list(
            color = marginal.col[[i]],
            size = marginal.size,
            symbol = "line-ns-open"
          ),
          showlegend = FALSE,
          hoverinfo = "x"
        )
      }
    } # /show.marginal.x

    if (show.marginal.y) {
      if (is.null(marginal.col)) {
        marginal.col <- plotly::toRGB(marker.col, alpha = marginal.alpha)
      }
      if (is.null(marginal.y.x)) marginal.y.x <- xlim[1]
      # Extend xlim to include marginal markers
      xlim[1] <- xlim[1] - 0.02 * diff(xlim)
      for (i in seq_len(n.groups)) {
        plt <- plotly::add_trace(
          plt,
          x = rep(marginal.y.x, length(marginal.y[[i]])),
          y = marginal.y[[i]],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = marginal.col[[i]],
            size = marginal.size,
            symbol = "line-ew-open"
          ),
          showlegend = FALSE,
          hoverinfo = "y"
          # legendgroup = .names[i]
        )
      }
    } # /show.marginal.y

    ## { SE band } ----
    if (se.fit) {
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = fitted[[i]] + se.times * se[[i]],
        type = scatter.type,
        mode = "lines",
        line = list(color = "transparent"),
        legendgroup = .names[i],
        showlegend = FALSE,
        hoverinfo = "none",
        inherit = FALSE
      )
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = fitted[[i]] - se.times * se[[i]],
        type = scatter.type,
        mode = "lines",
        fill = "tonexty",
        fillcolor = plotly::toRGB(se.col[[i]], alpha = se.alpha),
        line = list(color = "transparent"),
        # name = shade.name,
        legendgroup = .names[i],
        showlegend = FALSE,
        hoverinfo = "none",
        inherit = FALSE
      )
    }
    if (!is.null(fit)) {
      ##  { Fitted line } ----
      lfit <- list(
        color = plotly::toRGB(fit.col[[i]], alpha = fit.alpha),
        width = fit.lwd
      )
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = fitted[[i]],
        type = scatter.type,
        mode = "lines",
        line = lfit,
        name = fitted.text[i],
        legendgroup = .names[i],
        showlegend = if (legend & n.groups == 1) TRUE else FALSE,
        inherit = FALSE
      )
    }
  }

  # Layout ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = labs.col
  )
  tickfont <- list(
    family = theme$font.family,
    size = font.size,
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

  zerocol <- adjustcolor(theme$zerolines.col, theme$zerolines.alpha)
  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = ylab,
      showline = FALSE,
      showspikes = y.showspikes,
      spikecolor = spikecolor,
      spikedash = spikedash,
      spikemode = spikemode,
      spikesnap = spikesnap,
      spikethickness = spikethickness,
      # mirror = axes.mirrored,
      titlefont = f,
      showgrid = theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = tick.col,
      tickfont = tickfont,
      zeroline = theme$zerolines,
      zerolinecolor = zerocol,
      zerolinewidth = theme$zerolines.lwd,
      range = ylim,
      automargin = automargin.y
    ),
    xaxis = list(
      title = list(text = xlab),
      showline = FALSE,
      showspikes = x.showspikes,
      spikecolor = spikecolor,
      spikedash = spikedash,
      spikemode = spikemode,
      spikesnap = spikesnap,
      spikethickness = spikethickness,
      # mirror = axes.mirrored,
      titlefont = f,
      showgrid = theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = tick.col,
      tickfont = tickfont,
      zeroline = theme$zerolines,
      zerolinecolor = zerocol,
      zerolinewidth = theme$zerolines.lwd,
      range = xlim,
      automargin = automargin.x
    ),
    title = list(
      text = main,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = main.col
      ),
      xref = "paper",
      x = theme$main.adj,
      yref = "paper",
      y = main.y,
      yanchor = main.yanchor
    ),
    # titlefont = list(),
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin,
    showlegend = legend,
    legend = .legend
  ) # /layout

  ## vline ----
  if (!is.null(vline)) {
    plt <- plotly::layout(
      plt,
      shapes = plotly_vline(
        vline,
        color = vline.col,
        width = vline.width,
        dash = vline.dash
      )
    )
  }

  ## hline ----
  if (!is.null(hline)) {
    plt <- plotly::layout(
      plt,
      shapes = plotly_hline(
        hline,
        color = hline.col,
        width = hline.width,
        dash = hline.dash
      )
    )
  }

  ## square ----
  if (axes.square) {
    plt <- plt |>
      plotly::layout(
        yaxis = list(
          scaleanchor = "x",
          scaleratio = 1
        )
      )
  }

  # Subtitle ----
  # add annotation at top left with same font as main title
  if (!is.null(subtitle)) {
    plt <- plt |>
      plotly::add_annotations(
        x = subtitle.x,
        y = subtitle.y,
        xref = subtitle.xref,
        yref = subtitle.yref,
        xanchor = subtitle.xanchor,
        yanchor = subtitle.yanchor,
        text = subtitle,
        showarrow = FALSE,
        font = list(
          family = theme$font.family,
          size = font.size,
          color = main.col
        )
      )
  }

  # Config
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar.file.format,
      width = file.width,
      height = file.height
    ),
    scrollZoom = scrollZoom
  )

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file = normalizePath(filename, mustWork = FALSE),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  plt
} # rtemis::dplot3_xy


#' True vs. Predicted Plot
#'
#' A `dplot3_xy` wrapper for plotting true vs. predicted values
#'
#' @inheritParams dplot3_xy
#' @param x Numeric, vector/data.frame/list: True values. If y is NULL and
#' `NCOL(x) > 1`, first two columns used as `x` and `y`, respectively
#' @param y Numeric, vector/data.frame/list: Predicted values
#' @param ... Additional arguments passed to [dplot3_xy]
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(500)
#' y <- x + rnorm(500)
#' dplot3_fit(x, y)
#' }

dplot3_fit <- function(x, y, fit = "gam", se.fit = TRUE, ...) {
  dplot3_xy(x, y, fit = fit, se.fit = se.fit, ...)
} # rtemis::dplot3_fit
