# check_supervised_data.R
# ::rtemis::
# EDG rtemis.org

# Notes:
# Some algorithms do not work with variable names containing dots (SparkML)

#' Check data ahead of supervised learning
#'
#' @param x Data frame: Training set features and outcome in the last column.
#' @param dat_validation Data frame: Validation set features and outcome in the last column. Default is NULL.
#' @param dat_testing Data frame: Testing set features and outcome in the last column. Default is NULL.
#' @param allow_missing Logical: If TRUE, allow missing values in the data. Default is TRUE.
#' @param verbosity Integer: Verbosity level. Default is 1.
#'
#' @return None. Stops execution if checks fail.
#' 
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' check_supervised_data(training_data, validation_data, testing_data)
#' }
check_supervised_data <- function(x,
                                  dat_validation = NULL,
                                  dat_testing = NULL,
                                  allow_missing = TRUE,
                                  verbosity = 1L) {
  # if (upsample && downsample) stop("Only one of upsample and downsample can be TRUE")

  if (verbosity > 0L) {
    msg2start("Checking data is ready for training...")
  }

  # Check types ----
  check_inherits(x, "data.frame")
  if (!is.null(dat_validation)) {
    check_inherits(dat_validation, "data.frame")
  }
  if (!is.null(dat_testing)) {
    check_inherits(dat_testing, "data.frame")
  }
  
  # Check dimensions ----
  ncols <- NCOL(x)
  # Since one column must be outcome, need min of 2 columns
  if (ncols < 2) {
    stop("Data must contain at least 1 feature and 1 outcome column.")
  }
  if (!is.null(dat_validation)) {
    if (NCOL(dat_validation) != ncols) {
      stop("Validation set must contain same number of columns as training set.")
    }
  }
  if (!is.null(dat_testing)) {
    if (NCOL(dat_testing) != ncols) {
      stop("Testing set must contain same number of columns as training set.")
    }
  }

  # Missing values ----
  if (!allow_missing && anyNA(x)) {
    stop("Data should not contain missing values.")
  }

  # preprocess ----
  # if (!is.null(.preprocess)) {
  #   .preprocess$x <- x
  #   x <- do.call(preprocess, .preprocess)
  #   if (!is.null(x.test)) {
  #     .preprocess$x <- x.test
  #     x.test <- do.call(preprocess, .preprocess)
  #   }
  # }

  # Outcome class ----
  outcome_class <- class(x[[ncols]])
  if (!outcome_class %in% c("integer", "numeric", "factor")) {
    stop("Outcome must be integer, numeric, or factor.")
  }
  if (!is.null(dat_validation)) {
    if (class(dat_validation[[ncols]]) != outcome_class) {
      stop("Training and validation outcome must be of same class.")
    }
  }
  if (!is.null(dat_testing)) {
    if (class(dat_testing[[ncols]]) != outcome_class) {
      stop("Training and testing outcome must be of same class.")
    }
  }

  # Factor levels ----
  # Check that factors across training, validation, and testing contain the same levels.
  index_factor <- sapply(x, is.factor)
  training_levels <- lapply(x[index_factor], levels)
  if (!is.null(dat_validation)) {
    validation_levels <- lapply(dat_validation[index_factor], levels)
    if (!all(sapply(seq_along(training_levels), function(i) {
      identical(training_levels[[i]], validation_levels[[i]])
    }))) {
      stop("Training and validation set factor levels do not match.")
    }
  }
  if (!is.null(dat_testing)) {
    testing_levels <- lapply(dat_testing[index_factor], levels)
    if (!all(sapply(seq_along(training_levels), function(i) {
      identical(training_levels[[i]], testing_levels[[i]])
    }))) {
      stop("Training and testing set factor levels do not match.")
    }
  }

  if (verbosity > 0L) msg2done()
} # /rtemis::check_supervised_data


#' Check data ahead of unsupervised learning
#'
#' @param x Data frame: Features for unsupervised learning.
#' @param allow_missing Logical: If TRUE, allow missing values in the data. Default is FALSE.
#'
#' @return Nothing. Stops execution if checks fail.
#' 
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' check_unsupervised_data(features_data)
#' }
check_unsupervised_data <- function(x, allow_missing = FALSE, verbosity = 1L) {

  if (verbosity > 0L) {
    msg2start("Checking unsupervised data...")
  }
  
  if (NCOL(x) < 2) {
    stop("Data must contain at least 2 columns.")
  }
  if (any(sapply(x, function(x) !is.numeric(x)))) {
    stop("All columns must be numeric.")
  }
  if (!allow_missing && anyNA(x)) {
    stop("Data should not contain missing values.")
  }

  if (verbosity > 0L) msg2done()
} # /rtemis::check_unsupervised_data
