# dplot3_heatmap.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Interactive Heatmaps
#'
#' Draw interactive heatmaps using `heatmaply`
#'
#' @inheritParams colorGrad
#' @inheritParams mplot3_heatmap
#' @param x Input matrix
#' @param Rowv Logical or dendrogram.
#'   If Logical: Compute dendrogram and reorder rows. Defaults to FALSE
#'   If dendrogram: use as is, without reordering
#'   See more at `heatmaply::heatmaply("Rowv")`
#' @param Colv Logical or dendrogram.
#'   If Logical: Compute dendrogram and reorder columns. Defaults to FALSE
#'   If dendrogram: use as is, without reordering
#'   See more at `heatmaply::heatmaply("Colv")`
#' @param cluster Logical: If TRUE, set `Rowv` and `Colv` to TRUE
# @param k Integer: Cut dendogram to this many clusters, displayed by
# different colors
#' @param symm Logical: If TRUE, treat `x` symmetrically - `x` must be a square matrix. Default = FALSE
#' @param cellnote Matrix with values to be displayed on hover. Defaults to `ddSci(x)`
#' @param k_row Integer: Number of desired number of groups by which to color dendrogram branches in the rows.
#' Default = NA (determined automatically). See `heatmaply::heatmaply("k_row")`
#' @param k_col Integer: Number of desired number of groups by which to color dendrogram branches in the columns.
#' Default = NA (determined automatically). See `heatmaply::heatmaply("k_col")`
#' @param grid.gap Integer: Space between cells. Default = 0 (no space)
#' @param limits Float, length 2: Determine color range. Default = NULL, which automatically centers values around 0
#' @param margins Float, length 4: Heatmap margins.
#' @param key.title Character: Title for the color key.
#' @param showticklabels Logical: If TRUE, show tick labels.
#' @param colorbar_len Numeric: Length of the colorbar.
#' @param row_side_colors Data frame: Column names will be label names, cells
#' should be label colors. See `heatmaply::heatmaply("row_side_colors")`
#' @param col_side_colors Data frame: Column names will be label names, cells
#' @param row_side_palette Color palette function:
#' See `heatmaply::heatmaply("row_side_palette")`
#' @param col_side_palette Color palette function:
#' See `heatmaply::heatmaply("col_side_palette")`
#' @param font.size Numeric: Font size
#' @param padding Numeric: Padding between cells
#' @param displayModeBar Logical: If TRUE, display the plotly mode bar
#' @param modeBar.file.format Character: File format for image exports from the mode bar
#' @param file.width Numeric: Width of exported image
#' @param file.height Numeric: Height of exported image
#' @param file.scale Numeric: Scale of exported image
#' @param plot_method Character: Update February 2021: "ggplot" causes R session
#' to hang on MacOS but "plotly" seems to work
#' @param ... Additional arguments to be passed to `heatmaply::heatmaply`
#'
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' x <- rnormmat(200, 20)
#' xcor <- cor(x)
#' dplot3_heatmap(xcor)
#' }
#' @export

dplot3_heatmap <- function(
  x,
  Rowv = TRUE,
  Colv = TRUE,
  cluster = FALSE,
  #    k = 1,
  symm = FALSE,
  cellnote = NULL,
  colorGrad.n = 101,
  colors = NULL,
  space = "rgb",
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  k_row = 1,
  k_col = 1,
  # show_grid = FALSE,
  grid.gap = 0,
  limits = NULL,
  # margins = c(50, 50, 50, 50),
  margins = NULL,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  key.title = NULL,
  showticklabels = NULL,
  colorbar_len = .7,
  plot_method = "plotly",
  theme = rtTheme,
  #    palette = rtPalette,
  row_side_colors = NULL, # x[["row_side_colors"]],
  row_side_palette = NULL,
  col_side_colors = NULL, # x[["col_side_colors"]],
  col_side_palette = NULL,
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
  dependency_check("heatmaply")

  # Colnames ----
  if (is.null(colnames(x))) colnames(x) <- seq_len(NCOL(x)) # rtLetters(NCOL(x))
  if (is.null(rownames(x))) rownames(x) <- seq_len(NROW(x)) # rtLetters(NCOL(x), caps = TRUE)

  # Margins ----
  # By default, allow 7 px per character
  if (is.null(margins)) {
    bottom <- max(nchar(colnames(x))) * 7 + 15
    left <- max(nchar(rownames(x))) * 7 + 10
    margins <- c(bottom, left, 50, 50)
  }

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

  # Cluster ----
  if (cluster) Rowv <- Colv <- TRUE

  # Cellnote ----
  if (!is.null(cellnote)) {
    if (cellnote == "values") cellnote <- matrix(ddSci(x), NROW(x), NCOL(x))
  }

  # heatmaply ----
  ggp2text <- ggplot2::element_text(
    family = theme$font.family,
    color = theme$tick.labels.col
  )
  ggp2theme <- ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = theme$bg),
    plot.background = ggplot2::element_rect(fill = theme$bg),
    legend.text = ggplot2::element_text(color = theme$fg),
    legend.background = ggplot2::element_rect(fill = theme$bg),
    text = ggp2text,
    title = ggp2text,
    axis.text = ggp2text,
    axis.text.x = ggp2text,
    axis.text.y = ggp2text,
    axis.title.x = ggp2text,
    axis.title.y = ggp2text,
    plot.subtitle = ggp2text,
    plot.caption = ggp2text
  )

  # if (is.character(palette)) palette <- rtpalette(palette)

  # Dendrogram ----
  # for now, set to theme$fg
  Rowv <- x |>
    dist() |>
    hclust() |>
    as.dendrogram() |>
    dendextend::set("branches_k_color", k = 1) |>
    dendextend::set("branches_lwd", 1) |>
    dendextend::set("branches_col", theme$fg) |>
    dendextend::ladderize()
  #    rotate_DendSer(ser_weight = dist(x))
  Colv <- x |>
    t() |>
    dist() |>
    hclust() |>
    as.dendrogram() |>
    dendextend::set("branches_k_color", k = 1) |>
    dendextend::set("branches_lwd", 1) |>
    dendextend::set("branches_col", theme$fg) |>
    dendextend::ladderize()

  plt <- suppressWarnings(heatmaply::heatmaply(
    x,
    Rowv = Rowv,
    Colv = Colv,
    symm = symm,
    cellnote = cellnote,
    colors = colors,
    grid_gap = grid.gap,
    limits = limits,
    margins = margins,
    key.title = key.title,
    xlab = xlab,
    ylab = ylab,
    # main = main,
    k_row = k_row,
    k_col = k_col,
    plot_method = plot_method,
    colorbar_len = colorbar_len,
    showticklabels = showticklabels,
    heatmap_layers = ggp2theme,
    row_side_colors = row_side_colors,
    row_side_palette = row_side_palette,
    col_side_colors = col_side_colors,
    col_side_palette = col_side_palette
    # side_color_layers = ggp2theme,
    # file = filename
  ))

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
    yaxis2 = list(
      title = list(
        # text = ylab,
        font = f
      ), # gets assigned to dendrogram
      titlefont = f,
      # showgrid = FALSE,
      tickcolor = bg,
      showline = FALSE,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickfont = tickfont
    ),
    xaxis = list(
      title = list(
        # text = xlab,
        font = f
      ),
      titlefont = f,
      # showgrid = FALSE,
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
    legend = .legend
    # margin = margin
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
} # rtemis::dplot3_heatmap
