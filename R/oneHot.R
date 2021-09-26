# oneHot.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' One hot encoding
#'
#' One hot encode a vector or factors in a data.frame
#'
#' A vector input will be one-hot encoded regardless of type by looking at all unique values. With data.frame input,
#' only column of type factor will be one-hot encoded. This function is used by \link{preprocess}
#'
#' @param x Vector or data.frame
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @return For vector input, a one-hot-encoded matrix, for data.frame frame input, an expanded data.frame where all
#' factors are one-hot encoded
#' @author E.D. Gennatas
#' @export

oneHot <- function(x, verbose = FALSE) {

  UseMethod("oneHot", x)

} # rtemis::oneHot
