# oneHot.R
# ::rtemis::
# 2019 Efstathios D Gennatas egenn.github.io

#' One hot encoding
#'
#' One hot encode all factors in a data.frame
#'
#' @param x data.frame
#' @return data.frame where all factors are one hot encoded
#' @author Efstathios D Gennatas
#' @export

oneHot <- function(x, verbose = TRUE) {
  ncases <- NROW(x)
  factor.index <- which(sapply(x, is.factor))
  one.hot <- as.list(x)
  if (verbose) .names <- colnames(x)
  for (i in factor.index) {
    if (verbose) msg("Converting", .names[i], "to one hot...")
    .levels <- levels(x[, i])
    index <- as.numeric(x[, i])
    oh <- matrix(0, ncases, length(.levels))
    colnames(oh) <- .levels
    for (j in seq(ncases)) oh[j, index[j]] <- 1
    one.hot[[i]] <- oh
  }
  if (verbose) msg("Done")
  as.data.frame(one.hot)
} # rtemis::oneHot
