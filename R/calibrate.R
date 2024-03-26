# calibrate.R
# ::rtemis::
# 2023 EDG www.lambdamd.org

#' Calibrate predicted probabilities using GAM
#'
#' Calibrate predicted probabilities using a generalized additive model (GAM).
#'
#' @param true.labels Factor with true class labels
#' @param est.prob Numeric vector with predicted probabilities
#' @param pos.class.idi Integer: Index of the positive class
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
#'   est.prob = segment_logistic$.pred_poor,
#'   n_windows = 10,
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
#'   n_windows = 10,
#'   pos.class.idi = 2
#' )
#' }
calibrate <- function(true.labels,
                      est.prob,
                      pos.class.idi = 1,
                      mod = c("gam", "glm"),
                      k = 5,
                      verbose = TRUE) {
  stopifnot(pos.class.idi %in% c(1, 2))
  mod <- match.arg(mod)
  if (pos.class.idi == 1) {
    true.labels <- factor(true.labels, levels = rev(levels(true.labels)))
  }

  # GAM ----
  if (verbose) {
    if (mod == "gam") {
      msg2start(paste0("Fitting GAM (k=", k, ")..."))
    } else {
      msg2start("Fitting GLM...")
    }
  }
  if (mod == "glm") {
    mod <- glm(true.labels ~ est.prob, family = binomial())
  } else {
    mod <- mgcv::gam(true.labels ~ s(est.prob, k = k), family = binomial())
  }
  if (verbose) {
    msg2done()
  }
  mod
} # rtemis::calibrate
