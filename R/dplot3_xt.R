# dplot3_xt.R
# ::rtemis::
# 2024 EDG rtemis.org

# Multiple legends
# https://plotly.com/python/legend/#adding-multiple-legends
# https://plotly.com/r/legend/

#' Plot timeseries data
#'
#' @details
#' We are switching to `palette` being a color vector instead of the name of a built-in palette.
#'
#' @inheritParams dplot3_x
#' @param x Datetime vector or list of vectors OR object of class `xt`. If `xt`, `x2`, `y`, `y2`,
#' `xunits`, `yunits`, and `y2units` will be extracted from the object and the corresponding
#' arguments will be ignored.
#' @param y Numeric vector or named list of vectors: y-axis data.
#' @param x2 Datetime vector or list of vectors, optional: must be provided if `y2` does not
#' correspond to values in `x`. A single x-axis will be drawn for all values in `x` and `x2`.
#' @param y2 Numeric vector, optional: If provided, a second y-axis will be added to the right
#' side of the plot
#' @param which.xy Integer vector: Indices of `x` and `y` to plot.
#' If not provided, will select up to the first two x-y traces.
#' @param which.xy2 Integer vector: Indices of `x2` and `y2` to plot.
#' If not provided, will select up to the first two x2-y2 traces.
#' @param shade.bin Integer vector {0, 1}: Time points in `x` to shade on the plot. For example,
#' if there are 10 time points in `x`, and you want to shade time points 3 to 7,
#' `shade.bin = c(0, 0, 1, 1, 1, 1, 1, 0, 0, 0)`. Only set `shade.bin` or `shade.interval`, not
#' both.
#' @param shade.interval List of numeric vectors: Intervals to shade on the plot. Only set
#' `shade.bin` or `shade.interval`, not both.
#' @param shade.col Color: Color to shade intervals.
#' @param shade.x Numeric vector: x-values to use for shading.
#' @param shade.name Character: Name for shaded intervals.
#' @param shade.showlegend Logical: If TRUE, show legend for shaded intervals.
#' @param ynames Character vector, optional: Names for each vector in `y`.
#' @param y2names Character vector, optional: Names for each vector in `y2`.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param y2lab Character: y2-axis label.
#' @param xunits Character: x-axis units.
#' @param yunits Character: y-axis units.
#' @param y2units Character: y2-axis units.
#' @param yunits.col Color for y-axis units.
#' @param y2units.col Color for y2-axis units.
#' @param zt Numeric vector: Zeitgeber time. If provided, will be shown on the x-axis instead of
#' `x`. To be used only with a single `x` vector and no `x2`.
#' @param show.zt Logical: If TRUE, show zt on x-axis, if zt is provided.
#' @param show.zt.every Integer: Show zt every `show.zt.every` ticks. If NULL, will be calculated
#' to be `x.nticks` +/- 1 if `x.nticks` is not 0, otherwise 12 +/- 1.
#' @param zt.nticks Integer: Number of zt ticks to show. Only used if `show.zt.every` is NULL.
#' The actual number of ticks shown will depend on the periodicity of zt, so that zt = 0 is always
#' included.
#' @param main Character: Main title.
#' @param main.y Numeric: Y position of main title.
#' @param main.yanchor Character: "top", "middle", "bottom".
#' @param x.nticks Integer: Number of ticks on x-axis.
#' @param y.nticks Integer: Number of ticks on y-axis.
#' @param show.rangeslider Logical: If TRUE, show a range slider.
#' @param slider.start Numeric: Start of range slider.
#' @param slider.end Numeric: End of range slider.
#' @param theme Character or list: Name of theme or list of plot parameters.
#' @param palette Color list: will be used to draw each vector in `y` and `y2`, in order.
#' @param font.size Numeric: Font size for text.
#' @param yfill Character: Fill type for y-axis: "none", "tozeroy", "tonexty"
#' @param y2fill Character: Fill type for y2-axis: "none", "tozeroy", "tonexty"
#' @param fill.alpha Numeric: Fill opacity for y-axis.
#' @param yline.width Numeric: Line width for y-axis lines.
#' @param y2line.width Numeric: Line width for y2-axis lines.
#' @param x.showspikes Logical: If TRUE, show spikes on x-axis.
#' @param spike.dash Character: Dash type for spikes: "solid", "dot", "dash", "longdash",
#' "dashdot", "longdashdot".
#' @param spike.col Color for spikes.
#' @param x.spike.thickness Numeric: Thickness of spikes. `-2` avoids drawing border around spikes.
#' @param tickfont.size Numeric: Font size for tick labels.
#' @param x.tickmode Character: "auto", "linear", "array".
#' @param x.tickvals Numeric vector: Tick positions.
#' @param x.ticktext Character vector: Tick labels.
#' @param x.tickangle Numeric: Angle of tick labels.
#' @param legend.x Numeric: X position of legend.
#' @param legend.y Numeric: Y position of legend.
#' @param legend.xanchor Character: "left", "center", "right".
#' @param legend.yanchor Character: "top", "middle", "bottom".
#' @param legend.orientation Character: "v" for vertical, "h" for horizontal.
#' @param margin Named list with 4 numeric values: "l", "r", "t", "b" for left, right, top, bottom
#' margins.
#' @param x.standoff Numeric: Distance from x-axis to x-axis label.
#' @param y.standoff Numeric: Distance from y-axis to y-axis label.
#' @param y2.standoff Numeric: Distance from y2-axis to y2-axis label.
#' @param hovermode Character: "closest", "x", "x unified"
#' @param displayModeBar Logical: If TRUE, display plotly mode bar.
#' @param modeBar.file.format Character: "png", "svg", "jpeg", "webp", "pdf": file format for mode
#' bar image export.
#' @param scrollZoom Logical: If TRUE, enable zooming by scrolling.
#' @param ... Additional theme arguments.
#'
#' @return A plotly object
#' @author EDG
#' @export
dplot3_xt <- function(
  x,
  y = NULL,
  x2 = NULL,
  y2 = NULL,
  which.xy = NULL,
  which.xy2 = NULL,
  # Shade intervals
  shade.bin = NULL,
  shade.interval = NULL,
  shade.col = NULL,
  shade.x = NULL,
  shade.name = "",
  shade.showlegend = FALSE,
  ynames = NULL,
  y2names = NULL,
  xlab = NULL,
  ylab = NULL,
  y2lab = NULL,
  xunits = NULL,
  yunits = NULL,
  y2units = NULL,
  yunits.col = NULL,
  y2units.col = NULL,
  zt = NULL,
  show.zt = TRUE,
  show.zt.every = NULL,
  zt.nticks = 18L,
  # zt.start.at = 1L,
  main = NULL,
  main.y = 1,
  main.yanchor = "bottom",
  x.nticks = 0,
  y.nticks = 0,
  show.rangeslider = NULL,
  slider.start = NULL,
  slider.end = NULL,
  theme = rtTheme,
  palette = rtpalette(rtPalette),
  font.size = 16,
  yfill = "none",
  y2fill = "none",
  fill.alpha = .2,
  yline.width = 2,
  y2line.width = 2,
  x.showspikes = TRUE,
  spike.dash = "solid",
  spike.col = NULL,
  x.spike.thickness = -2,
  tickfont.size = 16,
  x.tickmode = "auto",
  x.tickvals = NULL,
  x.ticktext = NULL,
  x.tickangle = NULL,
  # legend
  legend.x = 0,
  legend.y = 1.1,
  legend.xanchor = "left",
  legend.yanchor = "top",
  legend.orientation = "h",
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
  file.scale = 1,
  ...
) {
  # Names ----
  .xname <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  .x2name <- labelify(gsub(".*\\$", "", deparse(substitute(x2))))
  if (!is.null(x2) && .xname != .x2name) {
    .xname <- NULL
  }
  .yname <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  .y2name <- labelify(gsub(".*\\$", "", deparse(substitute(y2))))
  # if (is.null(y2name) && !is.null(y2)) {
  #   y2name <- labelify(gsub(".*\\$", "", deparse(substitute(y2))))
  # }

  # Data ----
  if (inherits(x, "xt")) {
    y <- x$y
    x2 <- x$x2
    y2 <- x$y2
    xunits <- x$xunits
    yunits <- x$yunits
    y2units <- x$y2units
    shade.bin <- x$Shade
    zt <- x$zt
    x <- x$x
    if (!is.null(names(x)) && length(x) == 1) {
      .xname <- names(x)
    } else {
      .xname <- NULL
    }
  } else {
    if (is.null(y)) {
      stop("y must be provided")
    }
  }

  # Data to lists
  if (!is.null(y2) && is.null(x2)) x2 <- x
  if (!is.list(x)) x <- list(x)
  if (!is.list(y)) y <- list(y)
  if (!is.null(y2) && !is.list(y2)) y2 <- list(y2)
  if (!is.null(y2) && !is.list(x2)) x2 <- list(x2)

  # Recycle x and x2 as needed
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

  # Which traces to plot ----
  # By default, plot up to two for each y axis
  if (is.null(which.xy)) {
    if (length(x) > 2) {
      x <- x[1:2]
      y <- y[1:2]
    }
  } else {
    x <- x[which.xy]
    y <- y[which.xy]
  }

  if (is.null(which.xy2)) {
    if (length(x2) > 2) {
      x2 <- x2[1:2]
      y2 <- y2[1:2]
    }
  } else {
    x2 <- x2[which.xy2]
    y2 <- y2[which.xy2]
  }

  # Rangeslider ----
  if (is.null(show.rangeslider)) {
    show.rangeslider <- length(x[[1]]) > 500
  }

  # Check args ----
  if (!is.null(shade.bin) && !is.null(shade.interval)) {
    stop("Only set shade.bin or shade.interval, not both")
  }

  # Names ----
  if (is.null(ynames)) {
    ynames <- if (is.null(names(y))) {
      if (length(y) > 1) {
        paste(.yname, seq_along(y), sep = "_")
      } else {
        .yname
      }
    } else {
      names(y)
    }
  }

  if (!is.null(y2) && is.null(y2names)) {
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
      '<span style="color:',
      yunits.col,
      ';">',
      yunits,
      "</span>",
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
      '<span style="color:',
      y2units.col,
      ';">',
      y2units,
      "</span>",
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
  stopifnot(length(yfill) == length(y))

  if (length(y2) > 1 && length(y2fill) == 1) {
    y2fill <- rep(y2fill, length(y2))
  }

  if (!is.null(y2)) stopifnot(length(y2fill) == length(y2))

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

  # Calculate shade.interval from shade.bin ----
  if (!is.null(shade.bin)) {
    shade.bin_p <- c(0, shade.bin, 0)
    shade.bin_starts <- which(diff(shade.bin_p) == 1)
    shade.bin_ends <- which(diff(shade.bin_p) == -1)
    shade.interval <- lapply(
      seq_along(shade.bin_starts),
      \(i) c(shade.bin_starts[i], shade.bin_ends[i])
    )
  }

  # zt ----
  if (show.zt && !is.null(zt)) {
    x.tickmode <- "array"
    if (is.null(show.zt.every)) {
      # Get periodicity of ZT
      idi0 <- which(zt == 0)
      # Get differences between 0s
      diff_idi0 <- diff(idi0)[1]
      # Pick show.zt.every to be perfect divisor of diff_idi0 so that total length is closest to zt.nticks
      # a) diff_idi0 %% show.zt.every must be 0
      # b) length(zt) / show.zt.every must be closest to zt.nticks
      sze <- round(length(zt) / zt.nticks)
      i <- 0
      # if diff_idi0 %% sze != 0, search for closest integer above or below sze
      sze_high <- sze_low <- sze
      while (diff_idi0 %% sze_low != 0) {
        sze_low <- sze_low - 1
      }
      while (diff_idi0 %% sze_high != 0) {
        sze_high <- sze_high + 1
      }
      show.zt.every <- c(sze_low, sze_high)[which.min(abs(c(
        sze - sze_low,
        sze - sze_high
      )))]
    }
    idi <- seq(1, length(zt), by = show.zt.every)
    # Make sure 0 is included
    while (!0 %in% zt[idi]) {
      idi <- idi + 1
    }
    idi <- idi[idi <= length(zt)]
    x.tickvals <- x[[1]][idi]
    x.ticktext <- zt[idi]
    if (is.null(xlab)) xlab <- "ZT"
  }

  # Plot ----
  # if (!is.null(x.ticktext)) {
  #   stopifnot(!is.null(x.tickvals))
  #   plt <- plotly::plot_ly(
  #     x = x.tickvals,
  #     y = NA,
  #     type = "scatter",
  #     mode = "lines",
  #     text = x.ticktext
  #   )
  # } else {
  #   plt <- plotly::plot_ly(type = "scatter", mode = "lines")
  # }
  plt <- plotly::plot_ly(type = "scatter", mode = "lines")
  # browser()

  # Shade intervals ----
  if (!is.null(shade.interval)) {
    if (is.null(shade.x)) shade.x <- x[[1]]
    if (is.null(shade.col)) shade.col <- plotly::toRGB(theme$fg, 0.15)
    ymax <- max(unlist(y), unlist(y2))
    # Draw shaded rectangles
    for (i in seq_along(shade.interval)) {
      plt <- plotly::add_trace(
        plt,
        x = c(
          shade.x[shade.interval[[i]][1]],
          shade.x[shade.interval[[i]][2]],
          shade.x[shade.interval[[i]][2]],
          shade.x[shade.interval[[i]][1]]
        ),
        y = c(0, 0, ymax, ymax),
        fill = "toself",
        fillcolor = shade.col,
        line = list(color = "transparent"),
        yaxis = "y",
        xaxis = "x",
        name = shade.name,
        legendgroup = if (shade.showlegend) shade.name else NULL,
        showlegend = shade.showlegend && i == 1
      )
    }
  } # /shade.interval

  for (i in seq_along(y)) {
    plt <- plotly::add_trace(
      plt,
      x = x[[i]],
      y = y[[i]],
      line = list(color = palette.y[[i]], width = yline.width),
      fill = yfill[[i]],
      fillcolor = plotly::toRGB(palette.y[[i]], alpha = fill.alpha),
      name = ynames[[i]],
      legendgroup = if (!is.null(y2)) "legend_y" else NULL
    )
  } # /y scatter

  if (!is.null(y2)) {
    for (i in seq_along(y2)) {
      plt <- plotly::add_trace(
        plt,
        x = x2[[i]],
        y = y2[[i]],
        line = list(color = palette.y2[[i]], width = y2line.width),
        fill = y2fill[[i]],
        fillcolor = plotly::toRGB(palette.y2[[i]], alpha = fill.alpha),
        name = y2names[[i]],
        legendgroup = "legend_y2",
        yaxis = "y2"
      )
    }
  } # /y2 scatter

  # Labels ----
  if (is.null(xlab)) xlab <- .xname
  if (!is.null(xunits)) {
    xlab <- paste0(xlab, " (", xunits, ")")
  }

  if (!is.null(yunits)) {
    ylab <- if (is.null(ylab)) {
      if (length(y) == 1) {
        ynames
      } else {
        yunits
      }
    } else {
      paste(ylab, yunits)
    }
  }

  if (!is.null(y2units)) {
    y2lab <- if (is.null(y2lab)) {
      if (length(y2) == 1) {
        y2names
      } else {
        y2units
      }
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
        standoff = x.standoff,
        font = f
      ),
      nticks = x.nticks,
      showspikes = x.showspikes,
      spikedash = spike.dash,
      spikecolor = spike.col,
      spikethickness = x.spike.thickness,
      showgrid = theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickmode = x.tickmode,
      tickvals = x.tickvals,
      ticktext = x.ticktext,
      tickangle = x.tickangle,
      tickcolor = tick.col,
      tickfont = tickfont,
      zeroline = theme$zerolines,
      zerolinecolor = zerocol,
      zerolinewidth = theme$zerolines.lwd
    ), # /layout > xaxis
    yaxis = list(
      title = list(
        text = ylab,
        standoff = y.standoff,
        font = f
      ),
      nticks = y.nticks,
      showgrid = theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = tick.col,
      tickfont = tickfont,
      zeroline = theme$zerolines,
      zerolinecolor = zerocol,
      zerolinewidth = theme$zerolines.lwd,
      standoff = y.standoff
    ), # /layout > yaxis
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
    ), # /layout > legend
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin,
    hovermode = hovermode
  ) # /layout

  if (!is.null(y2)) {
    plt <- plt |>
      plotly::layout(
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = list(
            text = y2lab,
            standoff = y2.standoff,
            font = f
          ),
          tickfont = tickfont
        )
      )
  } # /yaxis2 layout

  # Config ----
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
  ) # /config

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file = normalizePath(filename, mustWork = FALSE),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  } # /save_image

  # Rangeslider ----
  if (show.rangeslider) {
    if (is.null(slider.start)) slider.start <- x[[1]][1]
    if (is.null(slider.end)) {
      idi <- min(500, length(x[[1]]))
      slider.end <- x[[1]][idi]
    }
    plt <- plt |>
      plotly::rangeslider(start = slider.start, end = slider.end)
  } # /rangeslider

  return(plt)
} # rtemis::dplot3_xt

# tickmode = "array", tickvals: placement, ticktext: labels
