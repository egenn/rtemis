# bias_variance.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Bias-Variance Decomposition
#'
#' @param x Predictors
#' @param y Outcome
#' @param mod Character: rtemis learner
#' @param res1_train.p Numeric: Proportion of cases to use for training
#' @param params List of `mod` parameters
#' @param resample.params Output of [setup.resample]
#' @param seed Integer: Seed for initial train/test split
#' @param verbose Logical: If TRUE, print messages to console
#' @param res.verbose Logical: passed to the learning function
#' @param ... Additional arguments passed to `resLearn`
#'
#' @author E.D. Gennatas
#' @export
bias_variance <- function(
  x,
  y,
  mod,
  res1_train.p = .7,
  params = list(),
  resample.params = setup.resample(n.resamples = 100),
  seed = NULL,
  verbose = TRUE,
  res.verbose = FALSE,
  ...
) {
  if (missing(y)) {
    if (NCOL(x) == 1) stop("Need at least one predictor and one outcome")
    y <- x[, NCOL(x)]
    x <- x[, -NCOL(x)]
  }
  # Create single testing, multiple training sets
  # 1. Test - train
  res1 <- resample(
    y,
    n.resamples = 1,
    resampler = "strat.sub",
    train.p = res1_train.p,
    seed = seed
  )
  x_test <- x[-res1$Subsample_1, ]
  true <- y[-res1$Subsample_1]
  dat_train <- data.frame(x, y)[res1$Subsample_1, ]
  res2 <- resample(
    dat_train,
    n.resamples = resample.params$n.resamples,
    resampler = resample.params$resampler,
    index = resample.params$index,
    train.p = resample.params$train.p,
    strat.n.bins = resample.params$strat.n.bins,
    target.length = resample.params$target.length,
    seed = resample.params$seed
  )

  p <- progressr::progressor(steps = resample.params$n.resamples)
  mods <- lapply(seq_along(res2), \(i) {
    args <- c(
      list(
        x = dat_train[res2[[i]], ],
        verbose = res.verbose
      ),
      params
    )
    p(sprintf("Running resample: %i/%i...", i, resample.params$n.resamples))
    do.call(select_learn(mod), args)
  })

  # predicted: N cases x N resamples
  predicted <- sapply(mods, \(m) predict(m, x_test))

  bias_squared <- (rowMeans(predicted) - true)^2
  variance <- apply(predicted, 1, popvar)

  out <- list(
    bias_squared = bias_squared,
    mean_bias_squared = mean(bias_squared),
    sd_bias_squared = sd(bias_squared),
    variance = variance,
    mean_variance = mean(variance),
    sd_variance = sd(variance)
  )
  class(out) <- c("rtBiasVariance", "list")
  out
} # rtemis::bias_variance

#' Print method for [bias_variance]
#'
#' @param x Output of [bias_variance]
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export
#'

print.rtBiasVariance <- function(x, ...) {
  cat(".:rtBiasVariance\n")
  cat("Mean squared bias: ")
  cat(hilite(ddSci(x$mean_bias_squared)))
  cat(" (", ddSci(x$sd_bias_squared), ")\n", sep = "")
  cat("    Mean variance: ")
  cat(hilite(ddSci(x$mean_variance)))
  cat(" (", ddSci(x$sd_variance), ")\n", sep = "")
  cat("\n")
}
