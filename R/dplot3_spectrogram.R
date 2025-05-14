# dplot3_spectrogram.R
# ::rtemis::
# 2023 EDG rtemis.org
# https://plotly.com/r/heatmaps/

#' Interactive Spectrogram
#'
#' Draw interactive spectrograms using `plotly`
#'
#' To set custom colors, use a minimum of `lo` and `hi`, optionnaly also
#' `lomid`, `mid`, `midhi` colors and set `colorscale = NULL`.
#'
#' @inheritParams colorGrad
#' @inheritParams mplot3_heatmap
#' @param x Numeric: Time
#' @param y Numeric: Frequency
#' @param z Numeric: Power
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param zlab Character: z-axis label
#' @param hover.xlab Character: x-axis label for hover
#' @param hover.ylab Character: y-axis label for hover
#' @param hover.zlab Character: z-axis label for hover
#' @param zmin Numeric: Minimum value for color scale
#' @param zmax Numeric: Maximum value for color scale
#' @param zauto Logical: If TRUE, automatically set zmin and zmax
#' @param hoverlabel.align Character: Alignment of hover labels
#' @param colorscale Character: Color scale. Default = "Jet"
#' @param colorbar.y Numeric: Y position of colorbar
#' @param colorbar.yanchor Character: Y anchor of colorbar
#' @param colorbar.xpad Numeric: X padding of colorbar
#' @param colorbar.ypad Numeric: Y padding of colorbar
#' @param colorbar.len Numeric: Length of colorbar
#' @param colorbar.title.side Character: Side of colorbar title
#' @param showgrid Logical: If TRUE, show grid
#' @param grid.gap Integer: Space between cells. Default = 0 (no space)
#' @param limits Numeric, length 2: Determine color range. Default = NULL, which automatically centers values around 0
#' @param key.title Character: Title of the key
#' @param showticklabels Logical: If TRUE, show tick labels
#' @param font.size Numeric: Font size
#' @param padding Numeric: Padding between cells
#' @param displayModeBar Logical: If TRUE, display the plotly mode bar
#' @param modeBar.file.format Character: File format for image exports from the mode bar
#' @param file.width Numeric: Width of exported image
#' @param file.height Numeric: Height of exported image
#' @param file.scale Numeric: Scale of exported image
#' @param ... Additional arguments to be passed to `heatmaply::heatmaply`
#'
#' @author E.D. Gennatas
#' @export

dplot3_spectrogram <- function(
  x,
  y,
  z,
  colorGrad.n = 101,
  colors = NULL,
  xlab = "Time",
  ylab = "Frequency",
  zlab = "Power",
  hover.xlab = xlab,
  hover.ylab = ylab,
  hover.zlab = zlab,
  zmin = NULL,
  zmax = NULL,
  zauto = TRUE,
  hoverlabel.align = "right",
  colorscale = "Jet",
  colorbar.y = .5,
  colorbar.yanchor = "middle",
  colorbar.xpad = 0,
  colorbar.ypad = 0,
  colorbar.len = .75,
  colorbar.title.side = "bottom",
  showgrid = FALSE,
  space = "rgb",
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  grid.gap = 0,
  limits = NULL,
  main = NULL,
  key.title = NULL,
  showticklabels = NULL,
  theme = rtTheme,
  font.size = NULL,
  padding = 0,
  displayModeBar = TRUE,
  modeBar.file.format = "svg",
  filename = NULL,
  file.width = 500,
  file.height = 500,
  file.scale = 1,
  ...
) {
  # Dependencies ----
  dependency_check("plotly")

  # Tick Labels ----
  if (is.null(showticklabels)) {
    showticklabels <- c(
      ifelse(NCOL(x) < 50, TRUE, FALSE),
      ifelse(NROW(x) < 50, TRUE, FALSE)
    )
  }

  if (is.null(font.size)) font.size <- 17.0769 - 0.2692 * ncol(x)

  # Limits ----
  if (is.null(limits)) {
    maxabs <- max(abs(x), na.rm = TRUE)
    if (.2 < maxabs && maxabs < 1) maxabs <- 1
    limits <- c(-maxabs, maxabs)
  }

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq_along(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  bg <- plotly::toRGB(theme$bg)
  fg <- plotly::toRGB(theme$fg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col)
  # tick_col <- plotly::toRGB(theme$tick.col)
  tick.labels.col <- plotly::toRGB(theme$tick.labels.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)

  # Colors ----
  if (is.null(mid)) mid <- theme$bg
  colors <- colorGrad(
    n = colorGrad.n,
    colors = colors,
    space = space,
    lo = lo,
    lomid = lomid,
    mid = mid,
    midhi = midhi,
    hi = hi
  )

  # Plot ----
  plt <- plotly::plot_ly()
  plt <- plt |>
    plotly::add_trace(
      x = x,
      y = y,
      z = z,
      type = "heatmap",
      zauto = zauto,
      zmin = zmin,
      zmax = zmax,
      colorscale = colorscale,
      colors = colors,
      hovertemplate = paste0(
        hover.xlab,
        ":<b> %{x:.3f}</b><br>",
        hover.ylab,
        ":<b> %{y:.3f}</b><br>",
        hover.zlab,
        ":<b> %{z:.3f}</b><extra></extra>"
      ),
      showlegend = FALSE
    )

  # Layout ----
  # '- layout ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = labs.col
  )
  tickfont <- list(
    family = theme$font.family,
    size = font.size,
    color = tick.labels.col
  )
  .legend <- list(
    font = list(
      family = theme$font.family,
      size = font.size,
      color = bg
    )
  )

  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = list(
        text = ylab,
        font = f
      ),
      titlefont = f,
      showgrid = showgrid,
      tickcolor = bg,
      showline = FALSE,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickfont = tickfont
    ),
    xaxis = list(
      title = list(
        text = xlab,
        font = f
      ),
      titlefont = f,
      showgrid = showgrid,
      tickcolor = bg,
      showline = FALSE,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickfont = tickfont
    ),
    title = list(
      text = main,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = main.col
      ),
      xref = "paper",
      x = theme$main.adj
    ),
    paper_bgcolor = bg,
    plot_bgcolor = bg,
    legend = .legend,
    hoverlabel = list(align = hoverlabel.align)
  )

  # Manual theme colors

  ## y axis tick label colors
  # plt[["x"]][["layoutAttrs"]][[2]][["yaxis2"]][["tickfont"]][["color"]]
  ## x axis tick label colors
  # plt[["x"]][["layoutAttrs"]][[2]][["xaxis"]][["tickfont"]][["color"]] <- "rgba(255, 0, 0, 1)"
  ## edge lines must be invisible
  plt$x$layout$yaxis$linecolor <- plt$x$layout$xaxis2$linecolor <- theme$bg

  # Manual layout ----
  # Set padding
  plt$sizingPolicy$padding <- padding

  # Colorbar ----
  # https://plotly.com/r/reference/#scatter-marker-colorbar
  plt <- plt |>
    plotly::colorbar(
      y = colorbar.y,
      yanchor = colorbar.yanchor,
      title = list(
        text = zlab,
        font = f,
        side = colorbar.title.side
      ),
      tickfont = tickfont,
      xpad = colorbar.xpad,
      ypad = colorbar.ypad,
      len = colorbar.len
    )

  # Config ----
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
      file.path(filename),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  plt
} # rtemis::dplot3_spectrogram
