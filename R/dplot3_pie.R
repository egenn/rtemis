# dplot3_pie.R
# ::rtemis::
# 2019 EDG rtemis.org

#' Interactive Pie Chart
#'
#' Draw interactive pie charts using `plotly`
#'
#' @inheritParams dplot3_bar
#' @param x data.frame: Input: Either a) 1 numeric column with categories defined by rownames, or
#' b) two columns, the first is category names, the second numeric or c) a numeric vector with categories defined using
#' the `category.names` argument
#' @param category.names Character, vector, length = NROW(x): Category names. Default = NULL, which uses
#' either `rownames(x)`, or the first column of `x` if `ncol(x) = 2`
#' @param textinfo Character: Info to show over each slince: "label", "percent", "label+percent" Default = "label+percent"
#' @param main Character: Plot title. Default = NULL, which results in colnames(x)\[1\],
#' @param theme Character: "light", "dark". Default = `getOption("rt.theme", "light")`
#' @param sep.col Separator color
#' @param bg Background color
#' @param plot.bg Plot background color
#' @param labs.col Color of labels
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3_pie(VADeaths[, 1, drop = F])
#' }
dplot3_pie <- function(
  x,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = .8,
  bg = NULL,
  plot.bg = NULL,
  theme = getOption("rt.theme", "black"),
  palette = rtPalette,
  category.names = NULL,
  textinfo = "label+percent",
  font.size = 16,
  labs.col = NULL,
  legend = TRUE,
  legend.col = NULL,
  sep.col = NULL,
  margin = list(b = 50, l = 50, t = 50, r = 20),
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

  # Names ----
  .input.name <- deparse(substitute(x))
  .rownames <- rownames(x)
  .colnames <- colnames(x)
  x <- as.data.frame(x)

  .cat.names <- category.names

  if (NCOL(x) == 2) {
    .cat.names <- as.character(x[, 1])
    x <- x[, 2, drop = FALSE]
    if (is.null(main)) main <- .colnames[2]
  }

  if (is.null(.cat.names)) {
    if (!is.null(.rownames)) {
      .cat.names <- .rownames
    } else {
      .cat.names <- LETTERS[seq_len(NROW(x))]
    }
  }

  if (is.null(main)) {
    if (!is.null(.colnames)) {
      main <- labelify(.colnames[1])
    } else {
      main <- labelify(.input.name)
    }
  }

  if (!is.null(main)) main <- paste0("<b>", main, "</b>")

  # Colors ----
  if (is.character(palette)) palette <- rtpalette(palette)
  p <- NROW(x)
  if (is.null(col)) col <- palette[seq_len(p)]
  if (length(col) < p) col <- rep(col, p / length(col))

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  bg <- plotly::toRGB(theme$bg)
  # plot.bg <- plotly::toRGB(theme$plot.bg)
  # grid.col <- plotly::toRGB(theme$grid.col)
  # tick.col <- plotly::toRGB(theme$tick.labels.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)

  if (is.null(legend.col)) legend.col <- labs.col
  sep.col <- if (is.null(sep.col)) bg else plotly::toRGB(sep.col)

  # plotly ----
  plt <- plotly::plot_ly(
    labels = .cat.names,
    values = x[, 1],
    type = "pie",
    textinfo = textinfo,
    insidetextfont = list(color = "#FFFFFF"),
    outsidetextfont = list(color = labs.col),
    marker = list(
      colors = unlist(col),
      line = list(color = sep.col, width = 1)
    )
  )

  ## layout ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = labs.col
  )
  .legend <- list(
    font = list(
      family = theme$font.family,
      size = font.size,
      color = legend.col
    )
  )
  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = ylab,
      showline = FALSE,
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    xaxis = list(
      title = xlab,
      showline = FALSE,
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    title = list(
      text = main,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = main.col
      )
    ),
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
      file.path(filename),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  plt
} # rtemis::dplot3_pie.R
