# synthRegData.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

synthRegData <- function(nrow = 500, ncol = 50, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  x <- rnormmat(nrow, ncol)
  w <- rnorm(ncol)
  y <- c(x %*% w + rnorm(500))

  list(dat = data.frame(x, y),
       w = w,
       seed = seed)

} # rtemis::synthRegData
