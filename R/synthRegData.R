# synthRegData.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Synthesize Simple Regression Data
#'
#' @param nrow Integer: Number of rows. Default = 500
#' @param ncol Integer: Number of columns. Default = 50
#' @param seed Integer: Seed for random number generator. Default = NULL
#' @author Efstathios D. Gennatas
#' @export

synthRegData <- function(nrow = 500, ncol = 50, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  x <- rnormmat(nrow, ncol)
  w <- rnorm(ncol)
  y <- c(x %*% w + rnorm(nrow))
  dat <- data.frame(x, y)
  colnames(dat)[seq(ncol)] <- paste0("Feature_", seq(ncol))

  list(dat = dat,
       w = w,
       seed = seed)

} # rtemis::synthRegData
