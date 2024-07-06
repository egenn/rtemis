# dplot3_xt.R
# ::rtemis::
# 2024 EDG rtemis.org

# Multiple legends
# https://plotly.com/python/legend/#adding-multiple-legends
# https://plotly.com/r/legend/

#' Plot timeseries data
#'
#' @details
#'
#' We are switching to `palette` being a color vector instead of the name of a built-in palette.
#'
#' @param x Datetime vector
#' @param y Numeric vector
#' @param y2 Numeric vector. If provided, a second y-axis will be added to the right
#' side of the plot
#'
#' @return A plotly object
#' @author EDG
#' @export
dplot3_xt <- function(
    x, y,
    x2 = NULL, y2 = NULL,
    xname = NULL,
    yname = NULL,
    xlab = NULL,
    ylab = NULL,
    y2lab = NULL,
    xunits = NULL,
    yunits = NULL,
    y2units = NULL,
    y2name = NULL,
    yunits.col = NULL,
    y2units.col = NULL,
    main = NULL,
    main.y = 1,
    main.yanchor = "bottom",
    show.rangeslider = FALSE,
    slider.start = NULL,
    slider.end = NULL,
    theme = rtTheme,
    palette = rtpalette(rtPalette),
    font.size = 16,
    yfill = "tozeroy",
    y2fill = "none",
    fill.alpha = .2,
    # line1.col = "#16A0AC",
    line1.width = 2,
    # line1.fill.col = NULL,
    # line2.col = "#FA6E1E",
    # line2.fill.col = NULL,
    line2.width = 2,
    x.showspikes = TRUE,
    spike.dash = "solid",
    spike.col = NULL,
    x.spikethickness = -2,
    tickfont.size = 16,
    # legend
    legend.x = 0,
    legend.y = 1.1,
    legend.xanchor = "left",
    legend.yanchor = "top",
    legend.orientation = "v",
    margin = list(l = 75, r = 75, b = 75, t = 75),
    # axis labels
    x.standoff = 20L,
    y.standoff = 20L,
    y2.standoff = 20L,
    hovermode = "x",
    # config
    displayModeBar = TRUE,
    modeBar.file.format = "svg",
    scrollZoom = TRUE,
    filename = NULL,
    file.width = 960,
    file.height = 500,
    file.scale = 1, ...) {
  # Names ----
  .xname <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  .x2name <- labelify(gsub(".*\\$", "", deparse(substitute(x2))))
  .yname <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  .y2name <- labelify(gsub(".*\\$", "", deparse(substitute(y2))))
  # if (is.null(y2name) && !is.null(y2)) {
  #   y2name <- labelify(gsub(".*\\$", "", deparse(substitute(y2))))
  # }

  # Data to lists ----
  if (!is.null(y2) && is.null(x2)) x2 <- x
  if (!is.list(x)) x <- list(x)
  if (!is.list(y)) y <- list(y)
  if (!is.null(y2) && !is.list(y2)) y2 <- list(y2)
  if (!is.null(y2) && !is.list(x2)) x2 <- list(x2)

  # Recycle x and x2 as needed ----
  if (length(y) > 1 && length(x) == 1) {
    x <- rep(x, length(y))
  }
  if (!is.null(y2) && length(y2) > 1 && length(x2) == 1) {
    x2 <- rep(x2, length(y2))
  }
  if (length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if (!is.null(y2) && length(x2) != length(y2)) {
    stop("x2 and y2 must be the same length")
  }

  # Names ----
  # xnames <- if (is.null(names(x))) {
  #   if (length(x) > 1) {
  #     paste(.xname, seq_along(x), sep = "_")
  #   } else {
  #     .xname
  #   }
  # } else {
  #   names(x)
  # }

  # if (!is.null(x2)) {
  #   x2names <- if (is.null(names(x2))) {
  #     if (length(x) > 1) {
  #       paste(.x2name, seq_along(x2), sep = "_")
  #     } else {
  #       .x2name
  #     }
  #   } else {
  #     names(x2)
  #   }
  # }

  ynames <- if (is.null(names(y))) {
    if (length(y) > 1) {
      paste(.yname, seq_along(y), sep = "_")
    } else {
      .yname
    }
  } else {
    names(y)
  }

  if (!is.null(y2)) {
    y2names <- if (is.null(names(y2))) {
      if (length(y2) > 1) {
        paste(.y2name, seq_along(y2), sep = "_")
      } else {
        .y2name
      }
    } else {
      names(y2)
    }
  }

  # Add units
  if (!is.null(yunits)) {
    if (is.null(yunits.col)) {
      yunits.col <- if (length(y) == 1) {
        palette[[1]]
      } else {
        "#00ff00"
      }
    }
    yunits <- paste0(
      "(",
      '<span style="color:', yunits.col, ';">', yunits, "</span>",
      ")"
    )
    ynames <- paste(ynames, yunits)
  }
  if (!is.null(y2units)) {
    if (is.null(y2units.col)) {
      y2units.col <- if (length(y2) == 1) {
        palette[[length(x) + 1]]
      } else {
        "#ff0000"
      }
    }
    y2units <- paste0(
      "(",
      '<span style="color:', y2units.col, ';">', y2units, "</span>",
      ")"
    )
    y2names <- paste(y2names, y2units)
  }

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
  bg <- plotly::toRGB(theme$bg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col, theme$grid.alpha)
  tick.col <- plotly::toRGB(theme$tick.col)
  legend.col <- labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)
  if (!theme$axes.visible) tick.col <- labs.col <- "transparent"
  if (is.null(spike.col)) spike.col <- theme$fg
  zerocol <- adjustcolor(theme$zerolines.col, theme$zerolines.alpha)

  # Colors ----
  # if (is.null(line1.fill.col)) line1.fill.col <- plotly::toRGB(line1.col, alpha = 0.4)
  # if (is.null(line2.fill.col) && !is.null(y2)) {
  #   line2.fill.col <- plotly::toRGB(line2.col, alpha = 0.4)
  # }
  palette.y <- palette[seq_along(y)]
  palette.y2 <- palette[length(y) + seq_along(y2)]

  if (length(y) > 1 && length(yfill) == 1) {
    yfill <- rep(yfill, length(y))
  }
  if (length(y2) > 1 && length(y2fill) == 1) {
    y2fill <- rep(y2fill, length(y2))
  }
  stopifnot(length(yfill) == length(y))
  stopifnot(length(y2fill) == length(y2))

  # Fonts ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = labs.col
  )
  tickfont <- list(
    family = theme$font.family,
    size = tickfont.size,
    color = theme$tick.labels.col
  )

  # Plot ----
  plt <- plotly::plot_ly()

  for (i in seq_along(y)) {
    plt <- plotly::add_trace(
      plt,
      x = x[[i]],
      y = y[[i]],
      type = "scatter",
      mode = "lines",
      line = list(color = palette.y[[i]], width = line1.width),
      fill = yfill[[i]],
      fillcolor = plotly::toRGB(palette.y[[i]], alpha = fill.alpha),
      name = ynames[[i]]
      # legendgroup = "legend_y"
    )
  }

  if (!is.null(y2)) {
    for (i in seq_along(y)) {
      plt <- plotly::add_trace(
        plt,
        x = x2[[i]],
        y = y2[[i]],
        type = "scatter",
        mode = "lines",
        line = list(color = palette.y2[[i]], width = line2.width),
        fill = y2fill[[i]],
        fillcolor = plotly::toRGB(palette.y2[[i]], alpha = fill.alpha),
        name = y2names[[i]],
        # legendgroup = "legend_y2",
        yaxis = "y2"
      )
    }
  }

  # Labels ----
  if (!is.null(xunits)) {
    xlab <- if (is.null(xlab)) {
      xunits
    } else {
      paste0(xlab, " (", xunits, ")")
    }
  }

  if (!is.null(yunits)) {
    ylab <- if (is.null(ylab)) {
      yunits
    } else {
      paste(ylab, yunits)
    }
  }

  if (!is.null(y2units)) {
    y2lab <- if (is.null(y2lab)) {
      y2units
    } else {
      paste(y2lab, y2units)
    }
  }

  # Layout ----
  plt <- plotly::layout(
    plt,
    xaxis = list(
      title = list(
        text = xlab,
        standoff = x.standoff
      ),
      showspikes = x.showspikes,
      spikedash = spike.dash,
      spikecolor = spike.col,
      spikethickness = x.spikethickness,
      titlefont = f,
      showgrid = theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = tick.col,
      tickfont = tickfont,
      zeroline = theme$zerolines,
      zerolinecolor = zerocol,
      zerolinewidth = theme$zerolines.lwd
    ),
    yaxis = list(
      title = list(
        text = ylab,
        standoff = y.standoff
      ),
      titlefont = f,
      showgrid = theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = tick.col,
      tickfont = tickfont,
      zeroline = theme$zerolines,
      zerolinecolor = zerocol,
      zerolinewidth = theme$zerolines.lwd,
      standoff = y.standoff
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
    legend = list(
      x = legend.x,
      y = legend.y,
      xanchor = legend.xanchor,
      yanchor = legend.yanchor,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = legend.col
      ),
      orientation = legend.orientation,
      bgcolor = "#ffffff00"
    ), # /legend
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin,
    hovermode = hovermode
  )

  if (!is.null(y2)) {
    plt <- plt |>
      plotly::layout(
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = list(
            text = y2lab,
            standoff = y2.standoff
          ),
          titlefont = f,
          tickfont = f
        )
      )
  }

  # Config ----
  plt <- plotly::config(plt,
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

  # Rangeslider ----
  if (show.rangeslider) {
    if (is.null(slider.start)) slider.start <- x[1]
    if (is.null(slider.end)) {
      idi <- min(50, length(x))
      slider.end <- x[idi]
    }
    plt <- plt |>
      plotly::rangeslider(start = slider.start, end = slider.end)
  }

  plt
} # rtemis::dplot3_xt
