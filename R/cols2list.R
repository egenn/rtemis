# cols2list.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Convert data frame columns to list elements
#'
#' Convenience function to create a list out of data frame columns
#'
#' @param x Input: Will be coerced to data.frame, then each column will become an element of a list
#' @author E.D. Gennatas
#' @export

cols2list <- function(x) {
  x <- as.data.frame(x)
  lst <- lapply(seq(x), function(i) x[, i])
  if (!is.null(colnames(x))) names(lst) <- colnames(x)
  lst
} # rtemis::cols2list
