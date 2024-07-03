# dplot3_xt.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Plot timeseries data
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
    x, y, y2 = NULL,
    xname = NULL,
    yname = NULL,
    y2name = NULL,
    main = NULL,
    main.y = 1,
    main.yanchor = "bottom",
    show.rangeslider = FALSE,
    slider.start = NULL,
    slider.end = NULL,
    theme = rtTheme,
    font.size = 16,
    yfill = "tozeroy",
    y2fill = "none",
    line1.col = "#16A0AC",
    line1.width = 2,
    line1.fill.col = NULL,
    line2.col = "#FA6E1E",
    line2.fill.col = NULL,
    line2.width = 2,
    x.showspikes = TRUE,
    spike.dash = "solid",
    spike.col = NULL,
    x.spikethickness = -2,
    tickfont.size = 16,
    legend.x = 1,
    legend.y = 1.1,
    legend.xanchor = "right",
    legend.yanchor = "top",
    legend.orientation = "v",
    margin = list(l = 75, r = 75, b = 75, t = 75),
    x.standoff = 20L,
    y.standoff = 20L,
    y2.standoff = 20L,
    displayModeBar = TRUE,
    modeBar.file.format = "svg",
    scrollZoom = TRUE,
    filename = NULL,
    file.width = 960,
    file.height = 500,
    file.scale = 1, ...) {
  # Names ----
  if (is.null(xname)) xname <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  if (is.null(yname)) yname <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  if (is.null(y2name) && !is.null(y2)) {
    y2name <- labelify(gsub(".*\\$", "", deparse(substitute(y2))))
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
  if (is.null(line1.fill.col)) line1.fill.col <- plotly::toRGB(line1.col, alpha = 0.4)
  if (is.null(line2.fill.col) && !is.null(y2)) {
    line2.fill.col <- plotly::toRGB(line2.col, alpha = 0.4)
  }
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
  plt <- plotly::plot_ly() |>
    plotly::add_trace(
      x = x,
      y = y,
      type = "scatter",
      mode = "lines",
      line = list(color = line1.col, width = line1.width),
      fill = yfill,
      fillcolor = line1.fill.col,
      name = yname
    )

  if (!is.null(y2)) {
    plt <- plt |>
      plotly::add_trace(
        x = x,
        y = y2,
        type = "scatter",
        mode = "lines",
        line = list(color = line2.col, width = line2.width),
        fill = y2fill,
        fillcolor = line2.fill.col,
        name = y2name,
        yaxis = "y2"
      )
  }

  # Layout ----
  plt <- plt |>
    plotly::layout(
      xaxis = list(
        title = list(
          text = xname,
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
          text = yname,
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
      ),
      paper_bgcolor = bg,
      plot_bgcolor = plot.bg,
      margin = margin
    )

  if (!is.null(y2)) {
    plt <- plt |>
      plotly::layout(
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = list(
            text = y2name,
            standoff = y2.standoff
          ),
          titlefont = f,
          tickfont = f
        )
      )
  }

  # Config
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
