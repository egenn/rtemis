# present.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Present multiple SupervisedRes objects
#'
#' Plot training and testing performance boxplots of multiple `SupervisedRes` objects
#'
#' @param x List of SupervisedRes objects.
#' @param metric Character: Metric to plot.
#' @param model_names Character: Names of models being plotted.
#' @param ylim Numeric vector of length 2: y-axis limits for the boxplots.
#' @param theme Theme object.
#' @param boxpoints Character: "all", "outliers", or "suspectedoutliers". Determines how points are
#' displayed in the boxplot.
#'
#' @return plotly object
#'
#' @author EDG
#' @export
method(present, class_list) <- function(
  x,
  metric = NULL,
  model_names = NULL,
  ylim = NULL,
  theme = choose_theme(),
  boxpoints = "all"
) {
  # Check that all elements of x are either Supervised or SupervisedRes objects
  all_supervised <- all(sapply(x, function(m) {
    S7_inherits(m, Supervised)
  }))
  all_supervisedres <- all(sapply(x, function(m) {
    S7_inherits(m, SupervisedRes)
  }))

  if (!(all_supervised || all_supervisedres)) {
    stop("Input must be a list of Supervised or SupervisedRes objects.")
  }

  # Check all models are of the same type
  type <- unique(sapply(x, function(m) m@type))
  if (length(type) > 1) {
    cli::cli_abort("All models must be of the same type")
  }

  # Get names
  if (is.null(model_names)) {
    model_names <- sapply(x, function(m) {
      m@algorithm
    })
  }

  # If any names are duplicated, append a number
  if (any(duplicated(model_names))) {
    model_names <- make.unique(model_names, sep = "_")
  }

  # Metric
  if (is.null(metric)) {
    metric <- switch(
      type,
      Classification = "Balanced_Accuracy",
      Regression = "Rsq"
    )
  }

  # Plots
  if (all_supervisedres) {
    # Get all res_metrics for each SupervisedRes
    xl_training <- lapply(x, function(m) {
      get_metrics(m, set = "training", metric = metric)
    })
    xl_test <- lapply(x, function(m) {
      get_metrics(m, set = "test", metric = metric)
    })
    names(xl_training) <- names(xl_test) <- model_names

    # Get ylim
    if (is.null(ylim)) {
      ylim <- range(c(xl_training, xl_test), na.rm = TRUE)
    }
    plot_training <- draw_box(
      xl_training,
      ylab = labelify(paste("Training", metric)),
      ylim = ylim,
      theme = theme,
      boxpoints = boxpoints
    )
    plot_test <- draw_box(
      xl_test,
      ylab = labelify(paste("Test", metric)),
      ylim = ylim,
      theme = theme,
      boxpoints = boxpoints
    )
  } else {
    plot_train <- draw_bar(
      xl_training
    )
  }

  plotly::subplot(
    plot_training,
    plot_test,
    nrows = 2L,
    shareX = TRUE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )
} # /rtemis::present
