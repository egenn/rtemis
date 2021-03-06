# dplot3.varimp.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org
# TODO: Forgot this existed, not have to add theme support

#' Interactive Variable Importance Plot
#'
#' Plot variable importance using \code{plotly}
#'
#' A simple \code{plotly} wrapper to plot horizontal barplots, sorted by value, which can be used
#' to visualize variable importance, model coefficients, etc.
#' @inheritParams mplot3.varimp
#' @param names Vector, string: Names of features
#' @param col Vector, colors: Single value, or multiple values to define bar (feature) color(s)
#' @param mar Vector, numeric, length 4: Plot margins in pixels (NOT inches).
#' Default = c(50, 110, 50, 50)
#' @param pad Integer: Pad plot by this many pixels. Default = 10
#' @param font.family Character: Font to use. Default = "Open Sans"
#' @param font.size Integer: Overall font size to use (essentially for the title at this point).
#' Default = 14
#' @param axis.font.size Integer: Font size to use for axis labels and tick labels
#' (Seems not to be in same scale as \code{font.size} for some reason. Experiment!)
#' @param font.color Color for all text
#' @param showlegend Logical: If TRUE, show legend
#' @author E.D. Gennatas
#' @export

dplot3.varimp <- function(x,
                          # error = NULL,
                          names = NULL,
                          main = NULL,
                          xlab = "Variable Importance",
                          ylab = "",
                          # col = colorAdjust(colorGrad(length(names), "penn"), .8),
                          plot.top = 1, # 1 or less means plot this percent
                          labelify = TRUE,
                          col = NULL,
                          alpha = 1,
                          palette = NULL,
                          mar = NULL,
                          # pad = 0,
                          # font.family = "Helvetica Neue",
                          font.size = 16,
                          axis.font.size = 14,
                          # font.color = "000",
                          theme = getOption("rt.theme", "lightgrid"),
                          showlegend = TRUE, ...) {

  # [ Dependencies ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
  if (is.null(mar)) {
    mar <- if (is.null(main)) c(40, 0, 0, 20) else c(40, 0, 40, 20)
  }


  # [ Theme ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  bg <- plotly::toRGB(theme$bg)
  # fg <- plotly::toRGB(theme$fg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col)
  tick.col <- plotly::toRGB(theme$tick.labels.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)

  # '- Axis font ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)

  # '- Tick font ====
  tickfont <- list(family = theme$font.family,
                   size = font.size,
                   color = tick.col)

  # [ Data ] ====
  if (NCOL(x) > 1 && NROW(x) > 1) stop("x must be a vector or single row or column")

  # '- Names ====
  if (is.null(names)) {
    if (is.null(names(x))) {
      .names <- if (NCOL(x) == 1) labelify(rownames(x)) else labelify(colnames(x))
    } else {
      .names <- labelify(names(x))
    }
  } else {
    .names <- labelify(names)
  }

  x <- as.numeric(x)
  if (length(.names) == 0) {
    .names <- paste("Feature", 1:length(x))
  }

  # '- Index ====
  index <- if (plot.top <= 1) {
    order(abs(x))[(length(x) - plot.top * length(x)):length(x)]
  } else {
    if (plot.top > length(x)) plot.top <- length(x)
    order(abs(x))[(length(x) - plot.top + 1):length(x)]
  }
  x <- x[index]
  .names <- .names[index]
  # reorder to arrange negative to positive
  index <- order(x)
  x <- x[index]
  y <- factor(.names[index], levels = .names[index])
  # if (!is.null(error)) error <- error[index]

  # [ Colors ] ====
  # Default to a fg to teal gradient
  if (is.null(col)) {
    if (is.null(palette)) palette <- c(theme$fg, "#18A3AC")
    col <- colorGrad.x(x, palette)
  }
  col <- colorAdjust(col, alpha = alpha)

  # [ plotly ] ====
  plt <- plotly::plot_ly(x = x, y = y,
                         type = 'bar',
                         orientation = 'h',
                         marker = list(color = col),
                         showlegend = FALSE)

  # Layout ====
  plt <- plotly::layout(plt,
                        margin = list(l = mar[2],
                                      t = mar[3],
                                      r = mar[4],
                                      b = mar[1],
                                      pad = 0), # inner plot area padding
                        xaxis = list(title = list(text = xlab,
                                                  font = f),
                                     # showline = axes.visible,
                                     # mirror = axes.mirrored,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE),
                        yaxis = list(title = list(text = ylab,
                                                  font = f),
                                     # showline = axes.visible,
                                     # mirror = axes.mirrored,
                                     showgrid = FALSE,
                                     # gridcolor = grid.col,
                                     # gridwidth = theme$grid.lwd,
                                     tickcolor = tick.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE),
                        title = list(text = main,
                                     font = list(family = theme$font.family,
                                                 size = font.size,
                                                 color = main.col),
                                     xref = 'paper',
                                     x = theme$main.adj),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg)

  # Remove padding
  plt$sizingPolicy$padding <- 0

  plt
} # dplot3.varimp
