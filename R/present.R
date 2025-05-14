# present.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' Present elevate models
#'
#' Plot training and testing performance boxplots of multiple `rtModCV``
#' objects created by [train_cv] using [dplot3_box]
#'
#' @inheritParams dplot3_box
#' @param ... rtModCV objects created with [train_cv]
#' @param mod.names Character: Names of models being plotted.
#' @param which.repeat Integer: which `rtModCV` repeat to plot.
#' @param metric Character: which metric to plot.
#' @param plot.train Logical: If TRUE, plot training performance.
#' @param plot.test Logical: If TRUE, plot testing performance.
#' @param subplot.margin Numeric: margin between subplots.
#'
#' @author E.D. Gennatas
#' @export

present <- function(
  ...,
  mod.names = NULL,
  which.repeat = 1,
  metric = NULL,
  plot.train = TRUE,
  plot.test = TRUE,
  boxpoints = "all",
  annotate_meansd = TRUE,
  main = NULL,
  ylim = NULL,
  htest = "none",
  htest.annotate.y = NULL,
  col = NULL,
  theme = rtTheme,
  margin = list(b = 65, l = 100, t = 60, r = 18, pad = 0),
  subplot.margin = .0666,
  filename = NULL,
  file.width = 500,
  file.height = 550,
  file.scale = 1
) {
  mods <- list(...)
  if (is.null(htest.annotate.y)) {
    htest.annotate.y <- if ((plot.train && plot.test)) {
      -.105
    } else {
      -.05
    }
  }

  # Check types ----
  types <- sapply(mods, \(m) m$type)
  if (length(unique(types)) > 1) {
    stop("All models must be of the same type")
  }
  type <- mods[[1]]$type
  if (is.null(metric)) {
    metric <- if (type == "Classification") {
      "Balanced Accuracy"
    } else {
      "R sq"
    }
  }

  # Get error ----
  if (is.null(mod.names)) {
    mod.names <- sapply(mods, \(m) m$mod.name)
  }
  train.error <- lapply(mods, \(m) m$error.train.res[[which.repeat]][[metric]])
  test.error <- lapply(mods, \(m) m$error.test.res[[which.repeat]][[metric]])
  names(train.error) <- names(test.error) <- mod.names

  # Plot ----
  if (plot.train) {
    plot_train <- dplot3_box(
      train.error,
      main = main,
      ylab = paste("Training", metric),
      col = col,
      boxpoints = boxpoints,
      ylim = ylim,
      htest = htest,
      htest.annotate = htest.annotate.y,
      annotate_meansd = annotate_meansd,
      margin = margin,
      theme = theme
    )
  }

  if (plot.test) {
    plot_test <- dplot3_box(
      test.error,
      ylab = paste("Testing", metric),
      col = col,
      boxpoints = boxpoints,
      ylim = ylim,
      htest = htest,
      htest.annotate.y = htest.annotate.y,
      annotate_meansd = annotate_meansd,
      margin = margin,
      theme = theme
    )
  }

  if (plot.train && plot.test) {
    plt <- plotly::subplot(
      plot_train,
      plot_test,
      nrows = 2,
      shareX = TRUE,
      titleY = TRUE,
      margin = subplot.margin
    )
  } else if (plot.test) {
    plt <- plot_test
  } else {
    plt <- plot_train
  }

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
  plt
} # rtemis::present
