# sparseVectorSummary.R
# ::rtemis::
# 2015 E.D. Gennatas rtemis.org

#' Sparseness and pairwise correlation of vectors
#'
#' Get sparseness measure on a matrix of vectors
#'
#' @param vectors Matrix of column vectors
#' @param y Optional numeric vector.

sparseVectorSummary <- function(vectors, y = NULL) {
  if (missing(vectors)) {
    args(sparseVectorSummary)
    return(9)
  }

  # Force to matrix - needed for single vector to be indexable by [,1]
  vectors <- as.matrix(vectors)

  # Measure sparseness for each vector and overall
  vec.summary <- data.frame(
    nonzero = numeric(NCOL(vectors)),
    sparseness = numeric(NCOL(vectors))
  )
  for (i in seq_len(NCOL(vectors))) {
    vec.summary$nonzero[i] <- sum(as.numeric(vectors[, i] != 0))
    vec.summary$sparseness[i] <- vec.summary$nonzero[i] / length(vectors[, i])
    cat("Sparseness of vector", i, "=", vec.summary$sparseness[i], "\n")
  }
  vec.sum <- apply(vectors, 1, sum)
  overallSparseness <- sum(as.numeric(vec.sum != 0)) / length(vec.sum)
  cat("- - -\nOverall sparseness =", overallSparseness, "\n")

  # Calculate correlations to y
  if (!is.null(y)) {
    vec.summary$cor.y <- t(cor(y, vectors))
  }

  # Calculate correlations among vectors
  vec.cor <- cor(vectors)
  mean.vec.cor <- mean(vec.cor[lower.tri(vec.cor)])

  # Output
  listOut <- list(
    vec.summary = vec.summary,
    overallSparseness = overallSparseness,
    mean.vec.cor = mean.vec.cor
  )
  return(listOut)
} # rtemis::sparseVectorSummary
