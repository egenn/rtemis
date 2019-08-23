# dplot3.box.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Interactive Boxplots
#'
#' Draw a interactive boxplot using \code{plotly}
#'
#' @param x data.frame: Input where rows are groups (can be 1 row), columns are features
#' @param main Character: Plot title. Default = NULL
#' @param xlab Character: x-axis label. Default = NULL
#' @param ylab  Character: y-axis label. Default = NULL
#' @param col Color, vector: Color for boxes. Default NULL, which will draw colors from \code{palette}
#' @param alpha Float (0, 1]: Transparency for box colors. Default = .8
#' @param bg Color: Background color. Default = "white"
#' @param plot.bg Color: Background color for plot area. Default = "white"
#' @param theme Character: THeme to use: "light", "dark", "lightgrid", "darkgrid". Default = "lightgrid"
#' @param palette Character: Name of `rtemis` palette to use. Default = "rtCol1". Only used if \code{col = NULL}
#' @param boxmode Character: Type of box plot to make: "group", "relative", "stack", "overlay". Default = "group". Use
#' "relative" for stacked boxes, which handles negative values correctly, unlike "stack", as of writing.
#' @param group.names Character, vector, length = NROW(x): Group names. Default = NULL, which uses \code{rownames(x)}
#' @param feature.names Character, vector, length = NCOL(x): Feature names. Default = NULL, which uses
#' \code{colnames(x)}
#' @param font.size  Float: Font size for all labels. Default = 16
#' @param font.alpha Float (0, 1]: Transparency for fonts. Default = .8
#' @param font.col Color: Font color. Default = "black"
#' @param font.family String: Font family to use. Default = "Helvetica Neue"
#' @param main.col Color: Title color. Default = NULL, determined by theme
#' @param axes.col Color: Axes color. Default = NULL, determined, by theme
#' @param labs.col Color: Labels' color. Default = NULL, determined by theme
#' @param grid.col Color: Grid color. Default = "gray85"
#' @param grid.lwd Float: Grid line width. Default = 1
#' @param grid.alpha Float (0, 1]: Transparency for \code{grid.col}. Default = .8
#' @param tick.col Color: Color for ticks and tick labels. Default = NULL, determined, by theme
#' @param legend Logical: If TRUE, draw legend. Default = TRUE
#' @param legend.col Color: Legend text color. Default = NULL, determined by theme
#' @param margin Named list: plot margins. Default = \code{list(t = 35)}
#'
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.box(VADeaths)
#' }

dplot3.box <-  function(x,
                        groups = "cols",
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        col = NULL,
                        alpha = .6,
                        bg = NULL,
                        plot.bg = NULL,
                        theme = getOption("rt.theme", "light"),
                        palette = getOption("rt.palette", "rtCol1"),
                        boxmode = c("group", "relative", "stack", "overlay"),
                        group.names = NULL,
                        # feature.names = NULL,
                        font.size = 16,
                        font.alpha = .8,
                        font.col = NULL,
                        font.family = "Helvetica Neue",
                        main.col = NULL,
                        axes.col = NULL,
                        labs.col = NULL,
                        grid.col = NULL,
                        grid.lwd = 1,
                        grid.alpha = .8,
                        tick.col = NULL,
                        legend = FALSE,
                        legend.col = NULL,
                        margin = list(t = 35)) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  boxmode <- match.arg(boxmode)
  main <- paste0("<b>", main, "</b>")

  dat <- as.data.frame(x)

  # Feature names ====
  # .feature.names <- feature.names
  # if (is.null(.feature.names)) {
  #   if (!is.null(colnames(dat))) {
  #     .feature.names <- colnames(dat)
  #   } else {
  #     .feature.names <- paste0("Feature", seq(NCOL(dat)))
  #   }
  # }
  if (!is.null(colnames(dat))) {
    .feature.names <- colnames(dat)
  } else {
    .feature.names <- paste0("Feature", seq(NCOL(dat)))
  }

  # Colors ====
  # plot.bg <- plotly::toRGB(plot.bg)
  # font.col <- plotly::toRGB(font.col, font.alpha)
  # grid.col <- plotly::toRGB(grid.col, grid.alpha)

  if (is.character(palette)) palette <- rtPalette(palette)
  p <- NCOL(dat)
  if (is.null(col)) {
    if (p == 1) {
      col <- palette[1]
    } else {
      col <- palette[seq(p)]
    }
  }

  if (length(col) < p) col <- rep(col, p/length(col))

  # Themes ====
  # Defaults
  # no box
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
    if (is.null(grid.col)) grid.col <- "rgba(255,255,255,1)"
    if (is.null(tick.col)) tick.col <- "rgba(0,0,0,1)"
  }
  if (theme == "darkgrid") {
    theme <- "dark"
    if (is.null(plot.bg)) plot.bg <- plotly::toRGB("gray15")
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
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
  } else if (theme == "dark") {
    if (is.null(bg)) bg <- "black"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray90")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray90")
    if (is.null(main.col)) main.col <- "rgba(255,255,255,1)"
    if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
    # gen.col <- "white"
  } else if (theme == "lightbox") {
    axes.visible <- axes.mirrored <- TRUE
    if (is.null(bg)) bg <- "rgba(255,255,255,1)"
    if (is.null(plot.bg)) plot.bg <- "rgba(255,255,255,1)"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray10")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray10")
    if (is.null(main.col)) main.col <- "rgba(0,0,0,1)"
    if (is.null(grid.col)) grid.col <- "rgba(255,255,255,1)"
    # gen.col <- "black"
  } else if (theme == "darkbox") {
    axes.visible <- axes.mirrored <- TRUE
    if (is.null(bg)) bg <- "rgba(0,0,0,1)"
    if (is.null(plot.bg)) plot.bg <- "rgba(0,0,0,1)"
    if (is.null(tick.col)) tick.col <- plotly::toRGB("gray90")
    if (is.null(labs.col)) labs.col <- plotly::toRGB("gray90")
    if (is.null(main.col)) main.col <- "rgba(255,255,255,1)"
    if (is.null(grid.col)) grid.col <- "rgba(0,0,0,1)"
    # gen.col <- "white"
  }

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # plotly ====
  plt <- plotly::plot_ly(y = dat[, 1],
                         color = plotly::toRGB(col[1], alpha),
                         # color = "red",
                         type = 'box',
                         name = .feature.names[1],
                         line = list(color = plotly::toRGB(col[1])),
                         fillcolor = plotly::toRGB(col[1], alpha),
                         marker = list(color = plotly::toRGB(col[1], alpha)))
  if (p > 1) {
    for (i in seq_len(p)[-1]) plt <- plotly::add_trace(plt, y = dat[, i],
                                                       color = plotly::toRGB(col[i], alpha),
                                                       name = .feature.names[i],
                                                       line = list(color = plotly::toRGB(col[i])),
                                                       fillcolor = plotly::toRGB(col[i], alpha),
                                                       marker = list(color = plotly::toRGB(col[i], alpha)))
  }

  # '- layout ====
  f <- list(family = font.family,
            size = font.size,
            color = labs.col)
  tickfont <- list(family = font.family,
                   size = font.size,
                   color = tick.col)
  .legend <- list(font = list(family = font.family,
                              size = font.size,
                              color = legend.col))

  plt <- plotly::layout(plt,
                        yaxis = list(title = ylab,
                                     showline = axes.visible,
                                     mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = grid,
                                     gridcolor = grid.col,
                                     gridwidth = grid.lwd,
                                     tickcolor = grid.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE),
                        xaxis = list(title = xlab,
                                     showline = axes.visible,
                                     mirror = axes.mirrored,
                                     titlefont = f,
                                     showgrid = FALSE,
                                     gridcolor = grid.col,
                                     gridwidth = grid.lwd,
                                     tickcolor = grid.col,
                                     tickfont = tickfont),
                        # boxmode = boxmode,  # CHECK: online docs show this, but gives error
                        # title = main,
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

  # Remove padding
  plt$sizingPolicy$padding <- 0

  plt

} # rtemis::dplot3.box.R
