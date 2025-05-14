# typeset
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Set type of columns
#'
#' Given an index of columns, convert identified columns of data.frame to
#' factor, ordered factor, or integer. A number of datasets are distributed
#' with an accompanying index of this sort, especially to define which
#' variables should be treated as categorical (here, factors) for predicting
#' modeling. This functions aims to make data type conversions in those cases
#' easier.
#'
#' @param x data frame: input whose columns' types you want to edit
#' @param factor.index Integer, vector: Index of columns to be converted to
#' factors using `factor(x)`
#' @param orderedfactor.index Integer, vector: Index of columns to be
#' converted to ordered factors using `factor(x, ordered = TRUE)`
#' @param integer.index Integer, vector: Index of columns to be converted to
#' integers using `as.integer`
#'
#' @author E.D. Gennatas
#' @export

typeset <- function(
  x,
  factor.index = NULL,
  orderedfactor.index = NULL,
  integer.index = NULL
) {
  # Factors ----
  if (!is.null(factor.index)) {
    for (i in factor.index) x[, i] <- factor(x[, i])
  }

  # Ordered factors ----
  if (!is.null(orderedfactor.index)) {
    for (i in orderedfactor.index) x[, i] <- factor(x[, i], ordered = TRUE)
  }

  # Integers ----
  if (!is.null(integer.index)) {
    for (i in integer.index) x[, i] <- as.integer(x[, i])
  }

  x
} # rtemis::typeset
