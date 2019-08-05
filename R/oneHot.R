# oneHot.R
# ::rtemis::
# 2019 Efstathios D Gennatas egenn.github.io

#' One hot encoding
#'
#' One hot encode a vector or all factors in a data.frame
#'
#' @param x Vector or data.frame
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @return For vector input, a one-hot-encoded matrix, for data.frame frame input, a data.frame where all factors are
#' one-hot encoded
#' @author Efstathios D Gennatas
#' @export

oneHot <- function(x, verbose = TRUE) {

  UseMethod("oneHot", x)

} # rtemis::oneHot
