# typeset
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Set type of columns
#' 
#' Given an index of columns, convert identified columns of data.frame to factor, ordered factor, 
#' or integer. A number of datasets are distributed with an accompanying index of this sort, 
#' especially to define which variables should be treated as categorical (here, factors) for
#' predicting modeling. This functions aims to make data type conversions in those cases easier.
#' 
#' @param x data frame: input whose columns' types you want to edit
#' @param factor.index Index of columns to be converted to factors using \code{factor(x)}
#' @param orderedfactor.index Index of columns to be converted to ordered factors 
#' using \code{factor(x, ordered = TRUE)}
#' @param integer.index Index of columns to be converted to integers using \code{as.integer}
#' @author Efstathios D. Gennatas
#' @export

typeset <- function(x,
                    factor.index = NULL,
                    orderedfactor.index = NULL, 
                    integer.index = NULL) {
  
  # [ Factors ] ====
  if (!is.null(factor.index)) {
    factor.index <- as.vector(factor.index)
    if (NCOL(x) != length(factor.index) & NCOL(x) != length(factor.index) + 1) 
      stop("Length of index does not match N cols of input")
    for (i in 1:length(factor.index)) {
      if (factor.index[i] == 1) x[, i] <- factor(x[, i])
    }
  } # factor.index
  
  # [ Ordered factors ] ====
  if (!is.null(orderedfactor.index)) {
    orderedfactor.index <- as.vector(orderedfactor.index)
    if (NCOL(x) != length(orderedfactor.index) & NCOL(x) != length(orderedfactor.index) + 1) 
      stop("Length of index does not match N cols of input")
    for (i in 1:length(orderedfactor.index)) {
      if (orderedfactor.index[i] == 1) x[, i] <- factor(x[, i], ordered = TRUE)
    }
  } # orderedfactor.index
  
  # [ Integers ] ====
  if (!is.null(integer.index)) {
    integer.index <- as.vector(integer.index)
    if (NCOL(x) != length(integer.index) & NCOL(x) != length(integer.index) + 1) 
      stop("Length of index does not match N cols of input")
    for (i in 1:length(integer.index)) {
      if (integer.index[i] == 1) x[, i] <- as.integer(x[, i])
    }
  } # integer.index
  
  x
} # rtemis::typeset
