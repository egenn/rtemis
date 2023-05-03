# calibrate.R
# ::rtemis::
# 2023 EDG www.lambdamd.org


#' Calibrate predicted probabilities using GAM
#'
#' Calibrate predicted probabilities using a generalized additive model (GAM).
#'
#' This is meant for experimentation.
#' 
#' @param true_labels Factor with true class labels
#' @param est_prob Numeric vector with predicted probabilities
#' @param window_width Numeric: Width of the calibration windows
#' @param pos_class_idi Integer: Index of the positive class
#' @param verbose Logical: If TRUE, printe messages to the console
#'
#' @return List with mod: fitted GAM model to be used for calibration of new data
#' calibrated_prob: Numeric vector with calibrated probabilities
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' data(segment_logistic, package = "probably")
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
#'     est_prob = calibrate(segment_logistic$Class, segment_logistic$.pred_poor),
#'     n_windows = 10,
#'     pos_class_idi = 2
#' )
#' }
calibrate_slide <- function(true_labels, est_prob,
                            #   window_width = 0.1,
                            n_windows = NULL,
                            pos_class_idi = 1,
                            k = 9,
                            verbose = TRUE) {
    
    pos_class <- levels(true_labels)[pos_class_idi]

    if (is.null(n_windows)) {
        n_windows <- min(24, length(unique(est_prob)))
    }

    # Cut estimated probabilities into n_windows
    windows <- quantile(est_prob, probs = seq(0, 1, length.out = n_windows + 1))

    # Calculate the proportion of condition positive cases in each window
    if (verbose) {
        msg2start("Calculating empirical risk in ", n_windows, " windows...")
    }
    window_empirical_risk <- sapply(seq_len(n_windows), \(i) {
        idl <- est_prob > windows[i] & est_prob <= windows[i + 1]
        sum(true_labels[idl] == pos_class) / sum(idl)
    })
    if (verbose) msg2done()
    
    # Calculate the mean probability in each window
    if (verbose) {
        msg2start(
            "Calculating mean estimated probability in ", n_windows,
            " windows..."
        )
    }
    mean_window_prob <- sapply(seq_len(n_windows), \(i) {
        mean(est_prob[est_prob >= windows[i] & est_prob < windows[i + 1]])
    })
    if (verbose) msg2done()

     # Calculate the proportion of condition positive cases in each window
     # This may not be monotonic
     window_empirical_risk <- sapply(seq_len(n_windows), \(i) {
         idl <- est_prob >= windows[i] & est_prob < windows[i + 1]
         sum(true_labels[idl] == pos_class) / sum(idl)
     })

    # Fit GAM
    if (verbose) msg2start("Fitting GAM...")
    mod <- mgcv::gam(
        window_empirical_risk ~ s(mean_window_prob, k = k),
        family = gaussian()
    )
    if (verbose) msg2done()

    # Predict
    if (verbose) msg2start("Getting calibrated probabilities...")
    calibrated_prob <- predict(
        mod,
        newdata = data.frame(mean_window_prob = est_prob)
    )

    if (verbose) msg2done()

    # Output
    list(mod = mod, calibrated_prob = calibrated_prob)
} # rtemis::calibrate_slide
