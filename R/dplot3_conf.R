# dplot3_conf.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Plot confusion matrix
#'
#' @inheritParams dplot3_x
#' @param x Confusion matrix
#' @param true.col Color for true positives & true negatives
#' @param false.col Color for false positives & false negatives
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
#' true_labels <- factor(c("a", "b", "a", "a", "b", "b", "b", "b"))
#' predicted_labels <- factor(c("a", "a", "b", "a", "b", "b", "a", "a"))
#' error <- mod_error(true_labels, predicted_labels)
#' dplot3_conf(error$ConfusionMatrix)
#' }
dplot3_conf <- function(
    x,
    true.col = "#72CDF4",
    false.col = "#FEB2E0",
    font.size = 18,
    main = NULL,
    main.y = 1,
    main.yanchor = "bottom",
    theme = rtTheme,
    margin = list(l = 75, r = 75, b = 75, t = 75), ...) {
  
  # Metrics ----
  nclasses <- ncol(x)
  class.totals <- colSums(x)
  predicted.totals <- rowSums(x)
  total <- sum(x)
  hits <- diag(x)
  misses <- class.totals - hits
  class.sensitivity <- hits / class.totals
  condition.negative <- total - class.totals
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
        plt, x, i, j, pos_color, neg_color, font.size
      )
    }
  }

  # Layout ----
  plt <- plotly::layout(
    plt,
    xaxis = list(
      # title = list(
      #   text = "Reference",
      #   font = f
      # ),
      side = "above",
      showticklabels = FALSE,
      # tickvals = seq_len(nclasses) - 0.5,
      # ticktext = colnames(x),
      # tickfont = f,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      # title = list(
      #   text = "Estimated",
      #   font = f
      # ),
      showticklabels = FALSE,
      # tickvals = seq_len(nclasses) - 0.5,
      # ticktext = colnames(x),
      # tickfont = f,
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
    title = main,
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin
  )

  # Add class labels above and to the left of the plot
  for (i in seq_len(nclasses)) {
    plt <- plotly::add_trace(
      plt,
      x = -0.125,
      y = i - 0.5,
      mode = "text",
      text = colnames(x)[i],
      textposition = "middle right",
      textfont = f,
      showlegend = FALSE
    )
    plt <- plotly::add_trace(
      plt,
      x = i - 0.5,
      y = -0.125,
      mode = "text",
      text = colnames(x)[i],
      textposition = "bottom center",
      textfont = f,
      showlegend = FALSE
    )
  }

  # Add x- and y-axis labels as annotations
  plt <- plotly::add_annotations(
    plt,
    x = nclasses / 2,
    y = -0.25,
    text = "Reference",
    font = f,
    showarrow = FALSE
  )
  plt <- plotly::add_annotations(
    plt,
    x = -0.25,
    y = nclasses / 2,
    text = "Estimated",
    font = f,
    showarrow = FALSE,
    textangle = -90
  )

  # For binary classification, add sensitivity and specificity below each column
  # at +0.25 below the plot
  # For multiclass classification, add class sensitivity below each column
  # at height = 0.25
  # if (nclasses == 2) {
  #   # Sensitivity
  #   plt <- plotly::add_annotations(
  #     plt,
  #     x = 0.5,
  #     y = 2.125,
  #     text = paste0("Sensitivity = ", ddSci(x[1, 1] / sum(x[1, ]), 3)),
  #     font = f,
  #     showarrow = FALSE
  #   )
  #   # Specificity
  #   plt <- plotly::add_annotations(
  #     plt,
  #     x = 1.5,
  #     y = 2.125,
  #     text = paste0("Specificity\n", ddSci(x[2, 2] / sum(x[2, ]), 3)),
  #     font = f,
  #     showarrow = FALSE
  #   )
  # } else {
  #   # Class sensitivities
  #   for (i in seq_len(nclasses)) {
  #     plt <- plotly::add_annotations(
  #       plt,
  #       x = i - 0.5,
  #       y = nclasses + .125,
  #       text = paste0("Sensitivity\n", ddSci(x[i, i] / sum(x[i, ]), 3)),
  #       font = f,
  #       showarrow = FALSE
  #     )
  #   }
  # }

  # Class sensitivities
  # "Sens" at bottom left corner offset by .125
  plt <- plotly::add_annotations(
    plt,
    x = -0.05,
    y = nclasses + .1,
    xanchor = "right",
    yanchor = "middle",
    text = "Sens.",
    font = f,
    showarrow = FALSE
  )

  # "Spec" below "Sens"
  plt <- plotly::add_annotations(
    plt,
    x = -0.05,
    y = nclasses + .3,
    xanchor = "right",
    yanchor = "middle",
    text = "Spec.",
    font = f,
    showarrow = FALSE
  )

  # Add gray rectangle background for class sensitivities
  plt <- plotly::add_trace(
    plt,
    x = c(0, nclasses, nclasses, 0, 0),
    y = c(nclasses, nclasses, nclasses + 0.2, nclasses + 0.2, nclasses),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme$fg, alpha = .05),
    showlegend = FALSE
  )

  # Per-class sensitivities
  for (i in seq_len(nclasses)) {
    plt <- plotly::add_annotations(
      plt,
      x = i - 0.5,
      y = nclasses + .1,
      text = ddSci(class.sensitivity[i], 3),
      font = f,
      showarrow = FALSE
    )
  }

  # Add gray rectangle background for class specificities
  plt <- plotly::add_trace(
    plt,
    x = c(0, nclasses, nclasses, 0, 0),
    y = c(nclasses + 0.2, nclasses + 0.2, nclasses + 0.4, nclasses + 0.4, nclasses + 0.2),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme$fg, alpha = .025),
    showlegend = FALSE
  )

  # Per-class specificities
  for (i in seq_len(nclasses)) {
    plt <- plotly::add_annotations(
      plt,
      x = i - 0.5,
      y = nclasses + .3,
      text = ddSci(class.specificity[i], 3),
      font = f,
      showarrow = FALSE
    )
  }

  # Add PPV label top right vertically
  plt <- plotly::add_annotations(
    plt,
    x = nclasses + 0.1,
    y = -.05,
    yanchor = "bottom",
    text = "PPV",
    font = f,
    showarrow = FALSE,
    textangle = -90
  )

  # NPV label right of PPV
  plt <- plotly::add_annotations(
    plt,
    x = nclasses + 0.3,
    y = -.05,
    yanchor = "bottom",
    text = "NPV",
    font = f,
    showarrow = FALSE,
    textangle = -90
  )

  # Add rectangle background for PPV 
  plt <- plotly::add_trace(
    plt,
    x = c(nclasses, nclasses + 0.2, nclasses + 0.2, nclasses),
    y = c(0, 0, nclasses, nclasses),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme$fg, alpha = .075),
    showlegend = FALSE
  )

  # Add rectangle background for NPV
  plt <- plotly::add_trace(
    plt,
    x = c(nclasses + 0.2, nclasses + 0.4, nclasses + 0.4, nclasses + 0.2),
    y = c(0, 0, nclasses, nclasses),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme$fg, alpha = .05),
    showlegend = FALSE
  )

  # Per-class PPV
  for (i in seq_len(nclasses)) {
    plt <- plotly::add_annotations(
      plt,
      x = nclasses + 0.1,
      y = i - 0.5,
      text = ddSci(class.ppv[i], 3),
      font = f,
      showarrow = FALSE,
      textangle = -90
    )
  }

  # Per-class NPV
  for (i in seq_len(nclasses)) {
    plt <- plotly::add_annotations(
      plt,
      x = nclasses + 0.3,
      y = i - 0.5,
      text = ddSci(class.npv[i], 3),
      font = f,
      showarrow = FALSE,
      textangle = -90
    )
  }

  # Add rectangle background for balanced accuracy
  plt <- plotly::add_trace(
    plt,
    x = c(nclasses, nclasses + 0.4, nclasses + 0.4, nclasses),
    y = c(nclasses, nclasses, nclasses + 0.4, nclasses + 0.4),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme$fg, alpha = .025),
    showlegend = FALSE
  )

  # Balanced accuracy at bottom right corner
  plt <- plotly::add_annotations(
    plt,
    x = nclasses + 0.2,
    y = nclasses + .2,
    xanchor = "center",
    yanchor = "middle",
    text = paste0("BA\n", ddSci(mean(diag(x) / colSums(x)), 3)),
    font = f,
    showarrow = FALSE
  )

  # Disable hoverinfo
  plt <- plotly::style(plt, hoverinfo = "none")

  return(plt)
} # rtemis::dplot3_conf

make_plotly_conf_tile <- function(
    p, x, i, j, pos_color, neg_color,
    font.size,
    xref = "x", yref = "y") {
  val <- x[i, j] / class.totals[i]
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
      color = ifelse(val > 0.5, "black", "white"),
      size = font.size
    ),
    showlegend = FALSE
  )

  return(p)
} # make_plotly_conf_tile
