# oneHot.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' @rdname oneHot
#'
#' \code{oneHot.data.table} performs operations **in-place**
#' and returns a data.table invisibly
#'
#' @export

oneHot.data.table <- function(x, verbose = TRUE) {
  ncases <- NROW(x)
  factor.index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor.index) {
    if (verbose) info(paste0("One hot encoding ", .names[i], "..."))
    .levels <- levels(x[[i]])
    index <- as.numeric(x[[i]])
    oh <- as.data.table(matrix(0, ncases, length(.levels)))
    colnames(oh) <- .levels
    for (k in seq_along(.levels)) oh[index == k, (.levels[k]) := 1]
    x[, (paste(.names[i], .levels, sep = "_")) := oh]
  }
  x[, paste(.names[factor.index]) := NULL]
  if (verbose) msg("Done")
  invisible(x)

} # rtemis::oneHot.data.table
