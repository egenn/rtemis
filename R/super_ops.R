# super_ops.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Convert probabilities to categorical (factor)
#'
#' @param x Numeric vector: Probabilities
#' @param levels Character vector: Class labels
#' @param binclasspos Integer: Index of the positive class for binary classification
#'
#' @return Factor
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' # Binary classification where "A" is the positive class, so .1 => B, .5 & .9 => A
#' prob2categorical(c(.1, .5, .9), c("A", "B"), 1)
#' # Binary classification where "B" is the positive class, so .1 => A, .5 & .9 => B
#' prob2categorical(c(.1, .5, .9), c("A", "B"), 2)
#' # Multi-class classification
#' prob <- matrix(c(.1, .3, .6, .05, .6, .35, .4, .3, .3), nrow = 3, byrow = TRUE)
#' prob2categorical(prob, c("A", "B", "C"))
#' }
prob2categorical <- function(x, levels, binclasspos = NULL) {
  n_classes <- length(levels)
  if (n_classes == 2) {
    # Binary classification
    if (is.null(binclasspos)) {
      binclasspos <- rtenv$binclasspos
    }
    stopifnot(binclasspos %in% c(1, 2))
    if (binclasspos == 2) {
      levels <- rev(levels)
    }
    fitted <- factor(
      ifelse(x >= .5, 1, 0),
      levels = c(1, 0),
      labels = levels
    )
  } else {
    # Multi-class classification
    stopifnot(length(levels) == ncol(x))
    fitted <- factor(
      apply(x, 1, which.max),
      levels = seq_len(n_classes),
      labels = levels
    )
  }
  fitted
} # rtemis::prob2categorical


check_supervised_inputs <- function(x, y = NULL) {
  if (is.null(y) && NCOL(x) < 2) {
    stop("y is missing")
  }
}
