# calibrate.R
# ::rtemis::
# 2023 EDG www.lambdamd.org


#' Calibrate predicted probabilities using GAM
#'
#' Calibrate predicted probabilities using a generalized additive model (GAM).
#'
#' @param true_labels Factor with true class labels
#' @param est_prob Numeric vector with predicted probabilities
#' @param pos_class_idi Integer: Index of the positive class
#' @param mod Character: Model to use for calibration. Either "gam" or "glm"
#' @param k Integer: GAM degrees of freedom
#' @param verbose Logical: If TRUE, printe messages to the console
#'
#' @return mod: fitted GAM model. Use `mod$fitted.values` to get calibrated
#' input probabilited; use `predict(mod, newdata = newdata, type = "response")`
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
#'     true_labels = segment_logistic$Class,
#'     est_prob = segment_logistic$.pred_poor,
#'     n_windows = 10,
#'     pos_class_idi = 2
#' )
#'
#' # Plot the calibration curve of the calibrated predictions
#' dplot3_calibration(
#'     true_labels = segment_logistic$Class,
#'     est_prob = calibrate(
#'         segment_logistic$Class,
#'         segment_logistic$.pred_poor
#'     )$fitted.values,
#'     n_windows = 10,
#'     pos_class_idi = 2
#' )
#' }
calibrate <- function(true_labels,
                      est_prob,
                      pos_class_idi = 1,
                      mod = c("gam", "glm"),
                      k = 5,
                      verbose = TRUE) {
    stopifnot(pos_class_idi %in% c(1, 2))
    mod <- match.arg(mod)
    if (pos_class_idi == 1) {
        true_labels <- factor(true_labels, levels = rev(levels(true_labels)))
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
        mod <- glm(true_labels ~ est_prob, family = binomial())
    } else {
        mod <- mgcv::gam(true_labels ~ s(est_prob, k = k), family = binomial())
    }
    if (verbose) {
        msg2done()
    }
    mod
} # rtemis::calibrate
