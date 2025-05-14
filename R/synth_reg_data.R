# synth_reg_data.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Synthesize Simple Regression Data
#'
#' @param nrow Integer: Number of rows. Default = 500
#' @param ncol Integer: Number of columns. Default = 50
#' @param noise.sd.factor Numeric: Add rnorm(nrow, sd = noise.sd.factor * sd(y)). Default = 2
#' @param resample.params Output of [setup.resample] defining training/testing split. The first resulting resample
#' will be used to create `dat.train` and `dat.test` output; all resample output under `resamples`
#' @param seed Integer: Seed for random number generator. Default = NULL
#' @param verbose Logical: If TRUE, print messages to console. Default = FALSE
#' @author E.D. Gennatas
#' @return List with elements `dat, dat.train, dat.test, resamples, w, seed`
#' @export

synth_reg_data <- function(
  nrow = 500,
  ncol = 50,
  noise.sd.factor = 1,
  resample.params = setup.resample(),
  seed = NULL,
  verbose = FALSE
) {
  if (!is.null(seed)) set.seed(seed)
  x <- rnormmat(nrow, ncol)
  w <- rnorm(ncol)
  y <- c(x %*% w)
  y <- y + rnorm(nrow, sd = noise.sd.factor * sd(y))
  dat <- data.frame(x, y)
  colnames(dat)[seq(ncol)] <- paste0("Feature_", seq(ncol))

  res <- resample(y, rtset = resample.params)
  dat.train <- dat[res[[1]], ]
  dat.test <- dat[-res[[1]], ]

  list(
    dat = dat,
    dat.train = dat.train,
    dat.test = dat.test,
    resamples = res,
    w = w,
    seed = seed
  )
} # rtemis::synth_reg_data
