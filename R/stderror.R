# stderror.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Standard Error of the Mean
#'
#' Calculate the standard error of the mean, which is equal to the standard deviation divided by
#' the square root of the sample size. NA values are automatically removed
#'
#' @param x Vector, numeric: Input data
#' @export
#' @author E.D. Gennatas

stderror <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)) - 1)
} # rtemis::stderror
