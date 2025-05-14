# calibrate.R
# ::rtemis::
# 2023 EDG rtemis.org

#' Calibrate predicted probabilities
#'
#' @param true.labels Factor with true class labels.
#' @param predicted.prob Numeric vector with predicted probabilities.
#' @param pos.class Integer: Index of the positive class.
#' @param alg Character: Algorithm to use to train calibration model. See [select_learn()].
#' @param learn.params List: List of parameters to pass to the learning algorithm
#' @param verbose Logical: If TRUE, print messages to the console
#'
#' @return Trained calibration model. Use `$fitted.values` to get calibrated
#' input probabilities; use `predict(mod, newdata = newdata, type = "response")`
#' to calibrate other estimated probabilities.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' data("segment_naive_bayes", package = "probably")
#'
#' # Plot the calibration curve of the original predictions
#' dplot3_calibration(
#'   true.labels = segment_naive_bayes$Class,
#'   predicted.prob = segment_naive_bayes$.pred_poor,
#'   pos.class = 2
#' )
#'
#' # Plot the calibration curve of the calibrated predictions
#' dplot3_calibration(
#'   true.labels = segment_naive_bayes$Class,
#'   predicted.prob = calibrate(
#'     segment_naive_bayes$Class,
#'     segment_naive_bayes$.pred_poor
#'   )$fitted.values,
#'   pos.class = 2
#' )
#' }
calibrate <- function(
  true.labels,
  predicted.prob,
  pos.class = NULL,
  alg = "isotonic",
  learn.params = list(),
  verbose = TRUE
) {
  # Check positive class
  if (is.null(pos.class)) {
    pos.class <- rtenv$binclasspos
  }
  if (pos.class == 2) {
    true.labels <- factor(true.labels, levels = rev(levels(true.labels)))
  }
  alg_name <- select_learn(alg, desc = TRUE)

  # mod ----
  if (verbose) msg20("Calibrating probabilities using ", alg_name, "...")
  learner <- select_learn(alg)
  do.call(
    learner,
    c(list(x = predicted.prob, y = true.labels), learn.params)
  )
} # rtemis::calibrate
