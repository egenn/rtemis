# rtROC.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org
# TODO: consider renaming to estimated.probabilities

#' Build an ROC curve
#'
#' Calculate the points of an ROC curve and the AUC
#'
#' `true.labels` should be a factor (will be coerced to one) where the first level is the
#' "positive" case. `predicted.probabilities` should be a vector of floats {0, 1} where `[0, .5)`
#' corresponds to the first level and `[.5, 1]` corresponds to the second level.
#' predicted.probabilities
#'
#' @param true.labels Factor with true labels
#' @param predicted.probabilities Numeric vector of predicted probabilities /
#' estimated score
#' @param thresholds Numeric vector of thresholds to consider
#' @param plot Logical: If TRUE, print plot
#' @param theme rtemis theme to use
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export

rtROC <- function(
  true.labels,
  predicted.probabilities,
  thresholds = NULL,
  plot = FALSE,
  theme = rtTheme,
  verbose = TRUE
) {
  true.labels <- as.factor(true.labels)
  true.levels <- levels(true.labels)
  n.classes <- length(true.levels)

  if (is.null(thresholds)) {
    thresholds <- sort(c(-Inf, unique(predicted.probabilities), Inf))
  }

  if (n.classes == 2) {
    predicted.labels <- lapply(thresholds, function(i) {
      # pred <- factor(as.integer(predicted.probabilities >= i), levels = c(1, 0))
      pred <- factor(
        ifelse(predicted.probabilities >= i, 1, 0),
        levels = c(1, 0)
      )
      levels(pred) <- true.levels
      pred
    })
    predicted.labels <- as.data.frame(
      predicted.labels,
      col.names = paste0("t_", seq(thresholds))
    )
    sensitivity.t <- sapply(
      predicted.labels,
      function(i) sensitivity(true.labels, i)
    )
    specificity.t <- sapply(
      predicted.labels,
      function(i) specificity(true.labels, i)
    )
    .auc <- auc(predicted.probabilities, true.labels, verbose = FALSE)
  } else {
    stop("Multiclass ROC not yet supported")
  }

  # Plot ----
  if (plot) {
    mplot3_xy(
      1 - specificity.t,
      sensitivity.t,
      type = "l",
      zerolines = FALSE,
      diagonal = TRUE,
      xaxs = "i",
      yaxs = "i",
      order.on.x = FALSE,
      xlab = "False Positive Rate",
      ylab = "True Positive Rate",
      xlim = c(0, 1),
      ylim = c(0, 1),
      theme = theme
    )
  }

  out <- list(
    Sensitivity = sensitivity.t,
    Specificity = specificity.t,
    AUC = .auc,
    Thresholds = thresholds
  )
  class(out) <- "rtROC"
  if (verbose) {
    msg2("Positive class:", true.levels[1])
    msg2("AUC =", .auc)
  }
  invisible(out)
} # rtemis::rtROC
