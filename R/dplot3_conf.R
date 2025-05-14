# dplot3_conf.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Plot confusion matrix
#'
#' @inheritParams dplot3_x
#' @param x Confusion matrix where rows are the reference and columns are the estimated classes or
#' rtemis `class_error` object produced by [mod_error]
#' @param true.col Color for true positives & true negatives
#' @param false.col Color for false positives & false negatives
#' @param pos.class Integer: Index of factor level to treat as the positive class
#' @param font.size Integer: font size
#' @param main Character: plot title
#' @param main.y Numeric: y position of the title
#' @param main.yanchor Character: y anchor of the title
#' @param theme List or Character: Either the output of a `theme_*()` function or the name of  a
#' theme. Use `themes()` to get available theme names. Theme functions are of the form
#' `theme_<name>`.
#' @param margin List: Plot margins
#'
#' @author EDG
#' @export
#' @return A plotly object
#'
#' @examples
#' \dontrun{
#' true <- factor(c("a", "a", "a", "a", "b", "b", "b", "b", "b", "b", "b", "b"))
#' predicted <- factor(c("a", "a", "b", "a", "b", "b", "a", "a", "b", "b", "a", "a"))
#' predicted.prob <- c(0.7, 0.55, 0.45, 0.62, 0.41, 0.32, 0.59, .63, .32, .21, .52, .58)
#' error <- mod_error(true, predicted, predicted.prob)
#' dplot3_conf(error)
#' }
dplot3_conf <- function(
  x,
  true.col = "#72CDF4",
  false.col = "#FEB2E0",
  pos.class = rtenv$binclasspos,
  font.size = 18,
  main = NULL,
  main.y = 1,
  main.yanchor = "bottom",
  theme = rtTheme,
  margin = list(l = 20, r = 5, b = 5, t = 20),
  # write to file
  filename = NULL,
  file.width = 500,
  file.height = 500,
  file.scale = 1,
  ...
) {
  # Input ----
  if (inherits(x, "class_error")) {
    x <- x$ConfusionMatrix
  }

  # Metrics ----
  nclasses <- ncol(x)
  total <- sum(x)
  class.totals <- rowSums(x)
  condition.negative <- total - class.totals
  predicted.totals <- colSums(x)
  hits <- diag(x)
  misses <- class.totals - hits
  class.sensitivity <- hits / class.totals
  true.negative <- total - predicted.totals - (class.totals - hits)
  class.specificity <- true.negative / condition.negative
  class.balancedAccuracy <- .5 * (class.sensitivity + class.specificity)
  # PPV = true positive / predicted condition positive
  class.ppv <- hits / predicted.totals
  # NPV  = true negative / predicted condition negative
  class.npv <- true.negative / (total - predicted.totals)

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

  # Colors ----
  pos_color <- colorRamp(colors = c(theme$bg, true.col))
  neg_color <- colorRamp(colors = c(theme$bg, false.col))

  # Fonts ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = theme$labs.col
  )

  # Plot ----
  plt <- plotly::plot_ly(
    type = "scatter",
    mode = "lines"
  )

  # Add colored tiles & counts ----
  for (i in seq_len(nclasses)) {
    for (j in seq_len(nclasses)) {
      plt <- make_plotly_conf_tile(
        plt,
        x,
        i,
        j,
        pos_color,
        neg_color,
        font.size,
        theme
      )
    }
  }

  # Layout ----
  plt <- plotly::layout(
    plt,
    xaxis = list(
      side = "above",
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      autorange = "reversed",
      scaleanchor = "x",
      scaleratio = 1
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
    showlegend = FALSE,
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin
  ) # /layout

  # Class labels ----
  # Add class labels above and to the left of the plot
  # Left
  plt <- plotly::add_annotations(
    plt,
    x = rep(-0.125, nclasses),
    y = seq_len(nclasses) - 0.5,
    text = colnames(x),
    # textposition = "middle right",
    font = f,
    showarrow = FALSE,
    textangle = -90
  )
  # Above
  plt <- plotly::add_annotations(
    plt,
    x = seq_len(nclasses) - 0.5,
    y = rep(-0.125, nclasses),
    text = colnames(x),
    # textposition = "bottom center",
    font = f,
    showarrow = FALSE
  )

  # x-axis label
  plt <- plotly::add_annotations(
    plt,
    x = nclasses / 2,
    y = ifelse(nclasses == 2, -.3, -0.5),
    text = "Predicted",
    font = f,
    showarrow = FALSE
  )

  # y-axis label
  plt <- plotly::add_annotations(
    plt,
    x = ifelse(nclasses == 2, -.3, -0.5),
    y = nclasses / 2,
    text = "Reference",
    font = f,
    showarrow = FALSE,
    textangle = -90
  )

  # Metrics ----
  if (nclasses == 2) {
    # Sens./Spec. ----
    # Rect: Sens./Spec. bg
    plt <- plotly::add_trace(
      plt,
      x = c(nclasses, nclasses + 0.3, nclasses + 0.3, nclasses),
      y = c(0, 0, nclasses, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme$fg, alpha = .075),
      showlegend = FALSE
    )

    # Text: Sens. & Spec.
    plt <- plotly::add_annotations(
      plt,
      x = rep(nclasses + 0.15, 2),
      y = c(.5, 1.5),
      text = paste0(
        c("Sensitivity\n", "Specificity\n"),
        c(
          ddSci(class.sensitivity[pos.class], 3),
          ddSci(class.specificity[pos.class], 3)
        )
      ),
      font = f,
      showarrow = FALSE,
      textangle = -90
    )

    # PPV/NPV ----
    # Rect: PPV/NPV bg
    plt <- plotly::add_trace(
      plt,
      x = c(0, nclasses, nclasses, 0, 0),
      y = c(nclasses, nclasses, nclasses + .3, nclasses + .3, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme$fg, alpha = .075),
      showlegend = FALSE
    )

    # Text: PPV & NPV
    plt <- plotly::add_annotations(
      plt,
      x = c(.5, 1.5),
      y = rep(nclasses + 0.15, 2),
      text = paste0(
        c("PPV\n", "NPV\n"),
        c(ddSci(class.ppv[pos.class], 3), ddSci(class.npv[pos.class], 3))
      ),
      font = f,
      showarrow = FALSE
    )
  } else {
    # PPV ----
    # Text: "PPV" at bottom left corner
    plt <- plotly::add_annotations(
      plt,
      x = -0.05,
      y = nclasses + .1,
      xanchor = "right",
      yanchor = "middle",
      text = "PPV",
      font = f,
      showarrow = FALSE
    )

    # Rect: PPV bg
    plt <- plotly::add_trace(
      plt,
      x = c(0, nclasses, nclasses, 0, 0),
      y = c(nclasses, nclasses, nclasses + 0.2, nclasses + 0.2, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme$fg, alpha = .075),
      showlegend = FALSE
    )

    # Text: Per-class PPV
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = i - 0.5,
        y = nclasses + .1,
        text = ddSci(class.ppv[i], 3),
        font = f,
        showarrow = FALSE
      )
    }

    # NPV ----
    # Label: "NPV" at bottom left corner
    plt <- plotly::add_annotations(
      plt,
      x = -0.05,
      y = nclasses + .3,
      xanchor = "right",
      yanchor = "middle",
      text = "NPV",
      font = f,
      showarrow = FALSE
    )

    # Rect: NPV bg
    plt <- plotly::add_trace(
      plt,
      x = c(0, nclasses, nclasses, 0, 0),
      y = c(
        nclasses + 0.2,
        nclasses + 0.2,
        nclasses + 0.4,
        nclasses + 0.4,
        nclasses + 0.2
      ),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme$fg, alpha = .05),
      showlegend = FALSE
    )

    # Text: Per-class NPV
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = i - 0.5,
        y = nclasses + .3,
        text = ddSci(class.npv[i], 3),
        font = f,
        showarrow = FALSE
      )
    }

    # Sensitivity ----
    # Label: "Sens." top right vertically
    plt <- plotly::add_annotations(
      plt,
      x = nclasses + 0.1,
      y = -.05,
      yanchor = "bottom",
      text = "Sens.",
      font = f,
      showarrow = FALSE,
      textangle = -90
    )

    # Rect: Sens. bg
    plt <- plotly::add_trace(
      plt,
      x = c(nclasses, nclasses + 0.2, nclasses + 0.2, nclasses),
      y = c(0, 0, nclasses, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme$fg, alpha = .075),
      showlegend = FALSE
    )

    # Text: Per-class Sens.
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = nclasses + 0.1,
        y = i - 0.5,
        text = ddSci(class.sensitivity[i], 3),
        font = f,
        showarrow = FALSE,
        textangle = -90
      )
    }

    # Specificity ----
    # Label: "Spec." top right vertically
    plt <- plotly::add_annotations(
      plt,
      x = nclasses + 0.3,
      y = -.05,
      yanchor = "bottom",
      text = "Spec.",
      font = f,
      showarrow = FALSE,
      textangle = -90
    )

    # Rect: Spec. bg
    plt <- plotly::add_trace(
      plt,
      x = c(nclasses + 0.2, nclasses + 0.4, nclasses + 0.4, nclasses + 0.2),
      y = c(0, 0, nclasses, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme$fg, alpha = .05),
      showlegend = FALSE
    )

    # Text: Per-class Spec.
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = nclasses + 0.3,
        y = i - 0.5,
        text = ddSci(class.specificity[i], 3),
        font = f,
        showarrow = FALSE,
        textangle = -90
      )
    }
  }

  # Balanced Accuracy ----
  # Rect: BA bg
  ba_pad <- ifelse(nclasses == 2, 0.3, 0.4)
  plt <- plotly::add_trace(
    plt,
    x = c(nclasses, nclasses + ba_pad, nclasses + ba_pad, nclasses),
    y = c(nclasses, nclasses, nclasses + ba_pad, nclasses + ba_pad),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme$fg, alpha = .025),
    showlegend = FALSE
  )

  # Text: Balanced accuracy
  ba_pad <- ifelse(nclasses == 2, 0.15, 0.2)
  ba <- ifelse(
    nclasses == 2,
    class.balancedAccuracy[pos.class],
    mean(class.balancedAccuracy)
  )
  plt <- plotly::add_annotations(
    plt,
    x = nclasses + ba_pad,
    y = nclasses + ba_pad,
    xanchor = "center",
    yanchor = "middle",
    text = paste0("BA\n", ddSci(ba, 3)),
    font = f,
    showarrow = FALSE
  )

  # Disable hoverinfo
  plt <- plotly::style(plt, hoverinfo = "none")

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

  return(plt)
} # rtemis::dplot3_conf

make_plotly_conf_tile <- function(
  p,
  x,
  i,
  j,
  pos_color,
  neg_color,
  font.size,
  theme,
  xref = "x",
  yref = "y"
) {
  val <- x[i, j] / sum(x[i, ])
  col <- if (i == j) {
    pos_color(val)
  } else {
    neg_color(val)
  }
  col <- rgb(col[1], col[2], col[3], maxColorValue = 255)
  # Add colored tile
  p <- plotly::add_trace(
    p,
    x = c(j - 1, j - 1, j, j, j - 1),
    y = c(i, i - 1, i - 1, i, i),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = col
  )
  # Add text
  p <- plotly::add_trace(
    p,
    x = j - 0.5,
    y = i - 0.5,
    mode = "text",
    text = paste0("<b>", x[i, j], "</b>"),
    textposition = "middle center",
    textfont = list(
      family = theme$font.family,
      color = ifelse(val > 0.5, theme$bg, theme$fg),
      size = font.size
    ),
    showlegend = FALSE
  )

  return(p)
} # make_plotly_conf_tile
