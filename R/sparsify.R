# sparsify.R
# ::rtemis::
# 2015 E.D. Gennatas rtemis.org

#' Sparsify a vector
#'
#' Keep top x% of values of a vector
#'
#' @param x Input vector
#' @param sparseness Percent of values of `x` to keep. The rest will be set to zero.
#' @author E.D. Gennatas

sparsify <- function(x, sparseness) {
  if (!is.vector(x)) stop("x must be a vector.")

  n.select <- sparseness * length(x)
  include <- order(x, decreasing = TRUE)[seq_len(n.select)]
  x.filtered <- x
  x.filtered[-include] <- 0

  return(x.filtered)
} # rtemis:: sparsify
