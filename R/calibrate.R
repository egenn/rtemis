# calibrate.R
# ::rtemis::
# 2023 EDG rtemis.org

#' Calibrate predicted probabilities using GAM
#'
#' Calibrate predicted probabilities using a generalized additive model (GAM).
#'
#' @param true.labels Factor with true class labels
#' @param predicted.prob Numeric vector with predicted probabilities
#' @param pos.class Integer: Index of the positive class
#' @param mod Character: Model to use for calibration. Either "gam" or "glm"
#' @param k Integer: GAM degrees of freedom
#' @param verbose Logical: If TRUE, print messages to the console
#'
#' @return mod: fitted GAM model. Use `mod$fitted.values` to get calibrated
#' input probabilities; use `predict(mod, newdata = newdata, type = "response")`
#' to calibrate other estimated probabilities.
#'
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
#'   n_windows = 10,
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
#'   n_windows = 10,
#'   pos.class = 2
#' )
#' }
calibrate <- function(true.labels,
                      predicted.prob,
                      pos.class = NULL,
                      mod = c("gam", "glm"),
                      k = 5,
                      verbose = TRUE) {
  stopifnot(pos.class %in% c(1, 2))
  mod <- match.arg(mod)
  if (is.null(pos.class)) {
    pos.class <- rtenv$binclasspos
  }
  if (pos.class == 1) {
    true.labels <- factor(true.labels, levels = rev(levels(true.labels)))
  }

  # GLAM ----
  if (mod == "glm") {
    mod <- glm(true.labels ~ predicted.prob, family = binomial())
  } else if (mod == "gam") {
    mod <- mgcv::gam(true.labels ~ s(predicted.prob, k = k), family = binomial())
  }
  if (verbose) {
    msg2done()
  }
  mod

} # rtemis::calibrate
