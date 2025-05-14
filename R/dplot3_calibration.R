# dplot3_calibration.R
# ::rtemis::
# 2023 EDG rtemis.org

#' Draw calibration plot
#'
#' @inheritParams dplot3_xy
#' @param true.labels Factor or list of factors with true class labels
#' @param predicted.prob Numeric vector or list of numeric vectors with predicted probabilities
#' @param bin.method Character: "quantile" or "equidistant": Method to bin the estimated
#' probabilities.
#' @param n.bins Integer: Number of windows to split the data into
#' @param pos.class Integer: Index of the positive class
#' @param main Character: Main title
#' @param subtitle Character: Subtitle, placed bottom right of plot
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param show.marginal.x Logical: Add marginal plot of distribution of estimated probabilities
#' @param mode Character: "lines", "markers", "lines+markers": How to plot.
#' @param show.brier Logical: If TRUE, add Brier scores to trace names.
#' @param filename Character: Path to save output.
#' @param ... Additional arguments passed to [dplot3_xy]
#'
#' @return NULL
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' data(segment_logistic, package = "probably")
#'
#' # Plot the calibration curve of the original predictions
#' dplot3_calibration(
#'   true.labels = segment_logistic$Class,
#'   predicted.prob = segment_logistic$.pred_poor,
#'   n.bins = 10,
#'   pos.class = 2
#' )
#'
#' # Plot the calibration curve of the calibrated predictions
#' dplot3_calibration(
#'   true.labels = segment_logistic$Class,
#'   predicted.prob = calibrate(
#'     segment_logistic$Class,
#'     segment_logistic$.pred_poor
#'   )$fitted.values,
#'   n.bins = 10,
#'   pos.class = 2
#' )
#' }
dplot3_calibration <- function(
  true.labels,
  predicted.prob,
  n.bins = 10,
  bin.method = c("quantile", "equidistant"),
  pos.class = NULL,
  main = NULL,
  subtitle = NULL,
  xlab = "Mean predicted probability",
  ylab = "Empirical risk",
  show.marginal.x = TRUE,
  marginal.x.y = -.02,
  marginal.col = NULL,
  marginal.size = 10,
  #  show.bins = TRUE,
  #    conf_level = .95,
  mode = "markers+lines",
  show.brier = TRUE,
  theme = rtTheme,
  filename = NULL,
  ...
) {
  # Arguments ----
  bin.method <- match.arg(bin.method)
  if (is.null(pos.class)) {
    pos.class <- rtenv$binclasspos
  }
  if (!is.list(true.labels)) {
    true.labels <- list(true_labels = true.labels)
  }
  if (!is.list(predicted.prob)) {
    predicted.prob <- list(estimated_prob = predicted.prob)
  }
  # Ensure same number of inputs
  stopifnot(length(true.labels) == length(predicted.prob))

  # Theme ----
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), list())
  }
  pos_class <- lapply(true.labels, \(x) {
    levels(x)[pos.class]
  })

  # Ensure same positive class
  stopifnot(length(unique(unlist(pos_class))) == 1)

  # Create windows
  if (bin.method == "equidistant") {
    breaks <- lapply(seq_along(predicted.prob), \(x) {
      seq(0, 1, length.out = n.bins + 1)
    })
  } else if (bin.method == "quantile") {
    breaks <- lapply(predicted.prob, \(x) {
      quantile(x, probs = seq(0, 1, length.out = n.bins + 1))
    })
  }

  # Calculate the mean probability in each window
  mean_bin_prob <- lapply(seq_along(predicted.prob), \(i) {
    sapply(seq_len(n.bins), \(j) {
      mean(predicted.prob[[i]][
        predicted.prob[[i]] >= breaks[[i]][j] &
          predicted.prob[[i]] < breaks[[i]][j + 1]
      ])
    })
  })
  names(mean_bin_prob) <- names(predicted.prob)

  # Calculate the proportion of condition positive cases in each window
  window_empirical_risk <- lapply(seq_along(predicted.prob), \(i) {
    sapply(seq_len(n.bins), \(j) {
      idl <- predicted.prob[[i]] >= breaks[[i]][j] &
        predicted.prob[[i]] < breaks[[i]][j + 1]
      sum(true.labels[[i]][idl] == pos_class[[i]]) / sum(idl)
    })
  })
  names(window_empirical_risk) <- names(predicted.prob)

  # Add Brier score
  if (show.brier) {
    .brier.score <- sapply(seq_along(predicted.prob), \(i) {
      brier_score(
        true = labels2int(true.labels[[i]], pos.class),
        estimated.prob = predicted.prob[[i]]
      )
    })
    # names(mean_bin_prob) <- paste0(names(mean_bin_prob), "(", round(.brier.score, 3), ")")
    names(window_empirical_risk) <- paste0(
      names(window_empirical_risk),
      " (Brier=",
      round(.brier.score, 3),
      ")"
    )
  }

  # Calculate confidence intervals
  # confint <- sapply(seq_len(n.bins), \(i) {
  #     events <- length(true.labels[true.labels == pos_class & predicted.prob >= breaks[i] & predicted.prob < breaks[i + 1]])
  #     total <- length(predicted.prob >= breaks[i] & predicted.prob < breaks[i + 1])
  #     suppressWarnings(pt <- prop.test(
  #         events, total,
  #         conf.level = conf_level
  #     ))
  #     pt$conf.int
  # })

  # Plot
  if (is.null(subtitle)) {
    subtitle <- paste(
      "using",
      n.bins,
      if (bin.method == "quantile") "quantiles" else "equidistant bins"
    )
  }
  # if (is.null(subtitle) && !is.na(subtitle)) .subtitle <- paste0(subtitle, "\n", .subtitle)
  plt <- dplot3_xy(
    x = mean_bin_prob,
    y = window_empirical_risk,
    main = main,
    # subtitle = paste("<i>", .subtitle, "</i>"),
    subtitle = subtitle,
    subtitle.x = 1,
    subtitle.y = 0,
    subtitle.yref = "y",
    subtitle.xanchor = "right",
    subtitle.yanchor = "bottom",
    xlab = xlab,
    ylab = ylab,
    show.marginal.x = show.marginal.x,
    marginal.x = predicted.prob,
    marginal.x.y = marginal.x.y,
    marginal.size = marginal.size,
    axes.square = TRUE,
    diagonal = TRUE,
    xlim = c(0, 1),
    ylim = c(0, 1),
    mode = mode,
    theme = theme,
    filename = filename,
    ...
  )

  # Add marginal.x ----
  # Using estimated probabilities
  # if (marginal.x) {
  #   if (is.null(marginal.col)) marginal.col <- plotly::toRGB(theme$fg, alpha = .5)
  #   for (i in seq_along(mean_bin_prob)) {
  #     plt <- plotly::add_trace(
  #       plt,
  #       x = predicted.prob[[i]],
  #       y = rep(-.02, length(predicted.prob[[i]])),
  #       type = "scatter",
  #       mode = "markers",
  #       marker = list(
  #         color = marginal.col,
  #         size = marginal.size,
  #         symbol = "line-ns-open"
  #       ),
  #       showlegend = FALSE,
  #       hoverinfo = "x"
  #     )
  #   }
  # } # /marginal.x

  plt
} # rtemis::dplot3_calibration
