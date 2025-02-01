# super_ops.R
# ::rtemis::
# 2024 EDG rtemis.org

supervised_type <- function(dat) {
  if (is.factor(dat[, NCOL(dat)])) {
    "Classification"
  } else {
    "Regression"
  }
} # rtemis::supervised_type

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
prob2categorical <- function(x, levels, binclasspos = 2L) {
  n_classes <- length(levels)
  if (n_classes == 2) {
    # Binary classification
    stopifnot(binclasspos %in% c(1, 2))
    if (binclasspos == 1L) {
      levels <- rev(levels)
    }
    fitted <- factor(
      ifelse(x >= .5, 1, 0),
      levels = c(0, 1),
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

#' Move outcome to last column
#'
#' @param data data.frame or equivalent
#' @param outcome_column Character: Name of outcome column
#'
#' @return object of same class as `data`
#'
#' @author EDG
#' @export

set_outcome <- function(dat, outcome_column) {
  # Get index of outcome column
  id <- grep(outcome_column, names(dat))
  # Check
  if (length(id) == 0) {
    stop('Column "', outcome_column, '" not found in data.')
  }
  # Reorder columns
  dat[, c(setdiff(seq_len(NCOL(dat)), id), id)]
} # rtemis::set_outcome

#' Make formula
#'
#' Makes a formula from a data.frame assuming the last column is the outcome
#'
#' If the o
#' @param x data.frame
#'
#' @author EDG
#' @keywords internal
#'
#' @return character
make_formula <- function(x, output = "character") {
  outcome <- names(x)[NCOL(x)]
  out <- paste(outcome, "~ .")
  if (output == "formula") {
    as.formula(out, env = parent.env(parent.frame()))
  } else {
    out
  }
} # rtemis::make_formula
