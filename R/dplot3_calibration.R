# dplot3_calibration.R
# ::rtemis::
# 2023 EDG lambdamd.original

#' Draw calibration plot
#'
#' @param true_labels Factor with true class labels
#' @param est_prob Numeric vector with predicted probabilities
#' @param n_windows Integer: Number of windows to split the data into
#' @param pos_class_idi Integer: Index of the positive class
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param mode Character: Plot mode
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
#'   true_labels = segment_logistic$Class,
#'   est_prob = segment_logistic$.pred_poor,
#'   n_windows = 10,
#'   pos_class_idi = 2
#' )
#'
#' # Plot the calibration curve of the calibrated predictions
#' dplot3_calibration(
#'   true_labels = segment_logistic$Class,
#'   est_prob = calibrate(
#'     segment_logistic$Class,
#'     segment_logistic$.pred_poor
#'   )$fitted.values,
#'   n_windows = 10,
#'   pos_class_idi = 2
#' )
#' }
dplot3_calibration <- function(true_labels, est_prob,
                               n_windows = 10,
                               pos_class_idi = 1,
                               xlab = "Mean estimated probability",
                               ylab = "Empirical risk",
                               #    conf_level = .95,
                               mode = "markers+lines", ...) {
  if (!is.list(est_prob)) {
    est_prob <- list(estimated_prob = est_prob)
  }
  pos_class <- levels(true_labels)[pos_class_idi]

  # Create windows
  breaks <- seq(0, 1, length.out = n_windows + 1)

  # Calculate the mean probability in each window
  # mean_window_prob <- sapply(seq_len(n_windows), \(i) {
  #     mean(est_prob[est_prob >= breaks[i] & est_prob < breaks[i + 1]])
  # })
  mean_window_prob <- lapply(seq_along(est_prob), \(i) {
    sapply(seq_len(n_windows), \(j) {
      mean(est_prob[[i]][est_prob[[i]] >= breaks[j] & est_prob[[i]] < breaks[j + 1]])
    })
  })

  # Calculate the proportion of condition positive cases in each window
  # window_empirical_risk <- sapply(seq_len(n_windows), \(i) {
  #     idl <- est_prob >= breaks[i] & est_prob < breaks[i + 1]
  #     sum(true_labels[idl] == pos_class) / sum(idl)
  # })
  window_empirical_risk <- lapply(seq_along(est_prob), \(i) {
    sapply(seq_len(n_windows), \(j) {
      idl <- est_prob[[i]] >= breaks[j] & est_prob[[i]] < breaks[j + 1]
      sum(true_labels[idl] == pos_class) / sum(idl)
    })
  })

  # Calculate confidence intervals
  # confint <- sapply(seq_len(n_windows), \(i) {
  #     events <- length(true_labels[true_labels == pos_class & est_prob >= breaks[i] & est_prob < breaks[i + 1]])
  #     total <- length(est_prob >= breaks[i] & est_prob < breaks[i + 1])
  #     suppressWarnings(pt <- prop.test(
  #         events, total,
  #         conf.level = conf_level
  #     ))
  #     pt$conf.int
  # })

  # Plot
  dplot3_xy(
    mean_window_prob, window_empirical_risk,
    xlab = xlab,
    ylab = ylab,
    axes.square = TRUE, diagonal = TRUE,
    xlim = c(0, 1), ylim = c(0, 1),
    mode = mode, ...
  )
} # rtemis::dplot3_calibration
