# dplot3_calibration.R
# ::rtemis::
# 2023 EDG lambdamd.original

#' Draw calibration plot
#'
#' @param true.labels Factor with true class labels
#' @param est.prob Numeric vector with predicted probabilities
#' @param n.windows Integer: Number of windows to split the data into
#' @param pos.class.idi Integer: Index of the positive class
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
#'   true.labels = segment_logistic$Class,
#'   est.prob = segment_logistic$.pred_poor,
#'   n.windows = 10,
#'   pos.class.idi = 2
#' )
#'
#' # Plot the calibration curve of the calibrated predictions
#' dplot3_calibration(
#'   true.labels = segment_logistic$Class,
#'   est.prob = calibrate(
#'     segment_logistic$Class,
#'     segment_logistic$.pred_poor
#'   )$fitted.values,
#'   n.windows = 10,
#'   pos.class.idi = 2
#' )
#' }
dplot3_calibration <- function(true.labels, est.prob,
                               n.windows = 10,
                               pos.class.idi = 1,
                               xlab = "Mean estimated probability",
                               ylab = "Empirical risk",
                               #    conf_level = .95,
                               mode = "markers+lines", ...) {
  if (!is.list(est.prob)) {
    est.prob <- list(estimated_prob = est.prob)
  }
  pos_class <- levels(true.labels)[pos.class.idi]

  # Create windows
  breaks <- seq(0, 1, length.out = n.windows + 1)

  # Calculate the mean probability in each window
  # mean_window_prob <- sapply(seq_len(n.windows), \(i) {
  #     mean(est.prob[est.prob >= breaks[i] & est.prob < breaks[i + 1]])
  # })
  mean_window_prob <- lapply(seq_along(est.prob), \(i) {
    sapply(seq_len(n.windows), \(j) {
      mean(est.prob[[i]][est.prob[[i]] >= breaks[j] & est.prob[[i]] < breaks[j + 1]])
    })
  })

  # Calculate the proportion of condition positive cases in each window
  # window_empirical_risk <- sapply(seq_len(n.windows), \(i) {
  #     idl <- est.prob >= breaks[i] & est.prob < breaks[i + 1]
  #     sum(true.labels[idl] == pos_class) / sum(idl)
  # })
  window_empirical_risk <- lapply(seq_along(est.prob), \(i) {
    sapply(seq_len(n.windows), \(j) {
      idl <- est.prob[[i]] >= breaks[j] & est.prob[[i]] < breaks[j + 1]
      sum(true.labels[idl] == pos_class) / sum(idl)
    })
  })

  # Calculate confidence intervals
  # confint <- sapply(seq_len(n.windows), \(i) {
  #     events <- length(true.labels[true.labels == pos_class & est.prob >= breaks[i] & est.prob < breaks[i + 1]])
  #     total <- length(est.prob >= breaks[i] & est.prob < breaks[i + 1])
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
