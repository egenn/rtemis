# oneHot.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' @rdname oneHot
#' @export

oneHot.data.frame <- function(x, verbose = TRUE) {
  ncases <- NROW(x)
  factor.index <- which(sapply(x, is.factor))
  one.hot <- as.list(x)
  if (verbose) .names <- colnames(x)
  for (i in factor.index) {
    if (verbose) msg0("One hot encoding ", .names[i], "...")
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
