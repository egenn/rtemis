# oneHot.default.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' @rdname oneHot
#' @export

oneHot.default <- function(x, verbose = TRUE) {

  x.name <- deparse(substitute(x))
  x <- factor(x)
  .levels <- levels(x)
  ncases <- NROW(x)
  index <- as.numeric(x)
  oh <- matrix(0, ncases, length(.levels))
  colnames(oh) <- paste0(x.name, ".", .levels)
  for (i in seq(ncases)) oh[i, index[i]] <- 1
  oh

} # rtemis::oneHot.default
