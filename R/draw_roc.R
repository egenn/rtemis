# draw_roc.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Draw ROC curve
#'
#' @param true_labels Factor: True outcome labels.
#' @param predicted_prob Numeric vector \[0, 1\]: Predicted probabilities for the positive class (i.e. second level of outcome).
#' @param main Character: Main title for the plot.
#' @param theme Theme object.
#' @param col Color vector.
#' @param legend Logical: If TRUE, draw legend.
#' @param legend_title Character: Title for the legend.
#' @param legend_xy Numeric vector: Position of the legend in the form c(x, y).
#' @param legend_xanchor Character: X anchor for the legend.
#' @param legend_yanchor Character: Y anchor for the legend.
#' @param auc_dp Integer: Number of decimal places for AUC values.
#' @param xlim Numeric vector: Limits for the x-axis.
#' @param ylim Numeric vector: Limits for the y-axis.
#' @param diagonal Logical: If TRUE, draw diagonal line.
#' @param diagonal_col Character: Color for the diagonal line.
#' @param axes_square Logical: If TRUE, make axes square.
#' @param filename Character: If provided, save the plot to this file.
#' @param ... Additional arguments passed to [draw_scatter].
#'
#' @return plotly object
#'
#' @author EDG
#' @export

draw_roc <- function(
  true_labels,
  predicted_prob,
  main = NULL,
  theme = choose_theme(),
  col = rtpalette(rtemis_palette),
  legend = TRUE,
  legend_title = "Group (AUC)",
  legend_xy = c(1, 0),
  legend_xanchor = "right",
  legend_yanchor = "bottom",
  auc_dp = 3L,
  xlim = c(0, 1),
  ylim = c(0, 1),
  diagonal = TRUE,
  diagonal_col = NULL,
  axes_square = TRUE,
  filename = NULL,
  ...
) {
  # List of probabilities
  probl <- if (!is.list(predicted_prob)) {
    list(predicted_prob)
  } else {
    predicted_prob
  }
  labelsl <- if (!is.list(true_labels)) {
    list(true_labels)
  } else {
    true_labels
  }
  # Check N sets
  if (length(probl) != length(labelsl)) {
    cli::cli_abort(
      "You must have the same N of sets of `predicted_prob` and `true_labels`."
    )
  }
  # Check lengths of corresponding sets
  for (i in seq_along(probl)) {
    if (length(probl[[i]]) != length(labelsl[[i]])) {
      cli::cli_abort(
        "You must have the same N of `predicted_prob` and `true_labels`."
      )
    }
  }

  .roc <- lapply(seq_along(probl), \(i) {
    pROC::roc(
      response = labelsl[[i]],
      predictor = probl[[i]],
      levels = levels(labelsl[[i]]),
      direction = "<"
    )
  })

  .names <- names(probl)
  TPR <- lapply(.roc, \(r) r[["sensitivities"]])
  FPR <- lapply(.roc, \(r) 1 - r[["specificities"]])
  AUC <- lapply(.roc, \(r) r[["auc"]])
  names(TPR) <- names(FPR) <- names(AUC) <- .names
  theme@parameters[["zerolines"]] <- FALSE
  draw_scatter(
    x = FPR,
    y = TPR,
    xlab = "False Positive Rate",
    ylab = "True Positive Rate",
    main = main,
    theme = theme,
    col = col,
    mode = "lines",
    group_names = paste0(.names, " (", ddSci(unlist(AUC), auc_dp), ")"),
    legend = legend,
    legend_title = legend_title,
    legend_xy = legend_xy,
    legend_xanchor = legend_xanchor,
    legend_yanchor = legend_yanchor,
    xlim = xlim,
    ylim = ylim,
    diagonal = diagonal,
    diagonal_col = diagonal_col,
    axes_square = axes_square,
    order_on_x = FALSE,
    filename = filename,
    ...
  )
} # /rtemis::draw_roc
