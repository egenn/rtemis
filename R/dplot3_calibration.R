# dplot3_calibration.R
# ::rtemis::
# 2023 EDG lambdamd.original

#' Draw calibration plot
#'
#' @param true.labels Factor or list of factors with true class labels
#' @param est.prob Numeric vector or list of numeric vectors with predicted probabilities
#' @param bin.method Character: "quantile" or "equidistant": Method to bin the estimated
#' probabilities.
#' @param n.bins Integer: Number of windows to split the data into
#' @param pos.class Integer: Index of the positive class
#' @param main Character: Main title
#' @param subtitle Character: Subtitle, placed bottom right of plot
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param mode Character: Plot mode
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
#'   est.prob = segment_logistic$.pred_poor,
#'   n.bins = 10,
#'   pos.class = 2
#' )
#'
#' # Plot the calibration curve of the calibrated predictions
#' dplot3_calibration(
#'   true.labels = segment_logistic$Class,
#'   est.prob = calibrate(
#'     segment_logistic$Class,
#'     segment_logistic$.pred_poor
#'   )$fitted.values,
#'   n.bins = 10,
#'   pos.class = 2
#' )
#' }
dplot3_calibration <- function(true.labels, est.prob,
                               n.bins = 10,
                               bin.method = c("quantile", "equidistant"),
                               pos.class = NULL,
                               main = NULL,
                               subtitle = NULL,
                               xlab = "Mean estimated probability",
                               ylab = "Empirical risk",
                               #    conf_level = .95,
                               mode = "markers+lines",
                               print.brier = TRUE,
                               filename = NULL, ...) {
  bin.method <- match.arg(bin.method)
  if (is.null(pos.class)) {
    pos.class <- rtenv$binclasspos
  }
  if (!is.list(true.labels)) {
    true.labels <- list(true_labels = true.labels)
  }
  if (!is.list(est.prob)) {
    est.prob <- list(estimated_prob = est.prob)
  }
  # Ensure same number of inputs
  stopifnot(length(true.labels) == length(est.prob))

  pos_class <- lapply(true.labels, \(x) {
    levels(x)[pos.class]
  })

  # Ensure same positive class
  stopifnot(length(unique(unlist(pos_class))) == 1)

  # Create windows
  if (bin.method == "equidistant") {
    breaks <- lapply(seq_along(est.prob), \(x) {
      seq(0, 1, length.out = n.bins + 1)
    })
  } else if (bin.method == "quantile") {
    breaks <- lapply(est.prob, \(x) {
      quantile(x, probs = seq(0, 1, length.out = n.bins + 1))
    })
  }

  # Calculate the mean probability in each window
  mean_bin_prob <- lapply(seq_along(est.prob), \(i) {
    sapply(seq_len(n.bins), \(j) {
      mean(est.prob[[i]][est.prob[[i]] >= breaks[[i]][j] & est.prob[[i]] < breaks[[i]][j + 1]])
    })
  })
  names(mean_bin_prob) <- names(est.prob)

  # Calculate the proportion of condition positive cases in each window
  window_empirical_risk <- lapply(seq_along(est.prob), \(i) {
    sapply(seq_len(n.bins), \(j) {
      idl <- est.prob[[i]] >= breaks[[i]][j] & est.prob[[i]] < breaks[[i]][j + 1]
      sum(true.labels[[i]][idl] == pos_class[[i]]) / sum(idl)
    })
  })
  names(window_empirical_risk) <- names(est.prob)

  # Add Brier score
  if (print.brier) {
    .brier.score <- sapply(seq_along(est.prob), \(i) {
      brier_score(
        true = labels2int(true.labels[[i]], pos.class),
        estimated.prob = est.prob[[i]]
      )
    })
    # names(mean_bin_prob) <- paste0(names(mean_bin_prob), "(", round(.brier.score, 3), ")")
    names(window_empirical_risk) <- paste0(names(window_empirical_risk), " (Brier=", round(.brier.score, 3), ")")
  }

  # Calculate confidence intervals
  # confint <- sapply(seq_len(n.bins), \(i) {
  #     events <- length(true.labels[true.labels == pos_class & est.prob >= breaks[i] & est.prob < breaks[i + 1]])
  #     total <- length(est.prob >= breaks[i] & est.prob < breaks[i + 1])
  #     suppressWarnings(pt <- prop.test(
  #         events, total,
  #         conf.level = conf_level
  #     ))
  #     pt$conf.int
  # })

  # Plot
  if (is.null(subtitle)) {
    subtitle <- paste(
      "<i> using", n.bins,
      if (bin.method == "quantile") "quantiles" else "equidistant bins",
      "</i>"
    )
  }
  # if (is.null(subtitle) && !is.na(subtitle)) .subtitle <- paste0(subtitle, "\n", .subtitle)
  dplot3_xy(
    x = mean_bin_prob,
    y = window_empirical_risk,
    main = main,
    # subtitle = paste("<i>", .subtitle, "</i>"),
    subtitle = subtitle,
    subtitle.x = 1,
    subtitle.y = .01,
    subtitle.xanchor = "right",
    subtitle.yanchor = "bottom",
    xlab = xlab,
    ylab = ylab,
    axes.square = TRUE, diagonal = TRUE,
    xlim = c(0, 1), ylim = c(0, 1),
    mode = mode,
    filename = filename, ...
  )

} # rtemis::dplot3_calibration
