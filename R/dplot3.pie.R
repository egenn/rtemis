# dplot3.pie.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Interactive Pie Chart
#'
#' Draw interactive pie charts using \code{plotly}
#'
#' @inheritParams dplot3.bar
#' @param x data.frame: Input: Either a) 1 numeric column with categories defined by rownames, or
#' b) two columns, the first is category names, the second numeric or c) a numeric vector with categories defined using
#' the \code{category.names} argument
#' @param category.names Character, vector, length = NROW(x): Category names. Default = NULL, which uses
#' either \code{rownames(x)}, or the first column of \code{x} if \code{ncol(x) = 2}
#' @param textinfo Character: Info to show over each slince: "label", "percent", "label+percent" Default = "label+percent"
#' @param main Character: Plot title. Default = NULL, which results in colnames(x)[1],
#' @param theme Character: "light", "dark". Default = \code{getOption("rt.theme", "light")}
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' dplot3.pie(VADeaths[, 1, drop = F])
#' }

dplot3.pie <-  function(x,
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        col = NULL,
                        alpha = .8,
                        bg = NULL,
                        plot.bg = NULL,
                        theme = getOption("rt.theme", "black"),
                        palette = getOption("rt.palette", "rtCol1"),
                        category.names = NULL,
                        textinfo = "label+percent",
                        font.size = 16,
                        # font.alpha = .8,
                        # font.col = NULL,
                        # main.col = NULL,
                        labs.col = NULL,
                        legend = TRUE,
                        legend.col = NULL,
                        sep.col = NULL,
                        margin = list(b = 50, l = 50, t = 50, r = 20),
                        padding = 0,
                        displayModeBar = TRUE,
                        filename = NULL,
                        file.width = 500,
                        file.height = 500, ...) {

  # Dependencies ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Names ====
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
      .cat.names <- LETTERS[seq(NROW(x))]
    }
  }

  if  (is.null(main)) {
    if (!is.null(.colnames)) {
      main <- labelify(.colnames[1])
    } else {
      main  <- labelify(.input.name)
    }
  }

  if (!is.null(main)) main <- paste0("<b>", main, "</b>")

  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  p <- NROW(x)
  if (is.null(col)) col <- palette[seq_len(p)]
  if (length(col) < p) col <- rep(col, p/length(col))

  # Theme ====
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

  # plotly ====
  plt <- plotly::plot_ly(labels = .cat.names,
                         values = x[, 1],
                         type = 'pie',
                         textinfo = textinfo,
                         insidetextfont = list(color = '#FFFFFF'),
                         outsidetextfont = list(color = labs.col),
                         marker = list(colors = unlist(col),
                                       line = list(color = sep.col, width = 1)))

  ## layout ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)
  .legend <- list(font = list(family = theme$font.family,
                              size = font.size,
                              color = legend.col))
  plt <- plotly::layout(plt,
                        yaxis = list(title = ylab,
                                     showline = FALSE,
                                     titlefont = f,
                                     showgrid = FALSE,
                                     zeroline = FALSE),
                        xaxis = list(title = xlab,
                                     showline = FALSE,
                                     titlefont = f,
                                     showgrid = FALSE,
                                     zeroline = FALSE),
                        title = list(text = main,
                                     font = list(family = theme$font.family,
                                                 size = font.size,
                                                 color = main.col)),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend)

  # Padding
  plt$sizingPolicy$padding <- padding
  # Config
  plt <- plotly::config(plt,
                        displaylogo = FALSE,
                        displayModeBar = displayModeBar)

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height,
                         format = tools::file_ext(file), out_file = filename)
  }

  plt

} # rtemis::dplot3.pie.R
