# synthRegData.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Synthesize Simple Regression Data
#'
#' @param nrow Integer: Number of rows. Default = 500
#' @param ncol Integer: Number of columns. Default = 50
#' @param resample.rtset Output of \link{rtset.resample} defining training/testing split. The first resulting resample
#' will be used to create \code{dat.train} and \code{dat.test} output; all resample output under \code{resamples}
#' @param seed Integer: Seed for random number generator. Default = NULL
#' @author Efstathios D. Gennatas
#' @return List with elements \code{dat, dat.train, dat.test, resamples, w, seed}
#' @export

synthRegData <- function(nrow = 500,
                         ncol = 50,
                         resample.rtset = rtset.resample(),
                         seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  x <- rnormmat(nrow, ncol)
  w <- rnorm(ncol)
  y <- c(x %*% w + rnorm(nrow))
  dat <- data.frame(x, y)
  colnames(dat)[seq(ncol)] <- paste0("Feature_", seq(ncol))

  res <- resample(y, rtset = resample.rtset)
  dat.train <- dat[res[[1]], ]
  dat.test <- dat[-res[[1]], ]

  list(dat = dat,
       dat.train = dat.train,
       dat.test = dat.test,
       resamples = res,
       w = w,
       seed = seed)

} # rtemis::synthRegData
