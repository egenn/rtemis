# bias_variance.R
# ::rtemis::
# EDG rtemis.org

#' Bias-Variance Decomposition
#'
#' @param x data.frame or similar where last column is the outcome.
#' @param mod Character: rtemis learner.
#' @param res1_train_p Numeric: Proportion of cases to use for training.
#' @param params List of `mod` parameters.
#' @param resampler_parameters Output of [setup_Resampler].
#' @param seed Integer: Seed for initial train/test split.
#' @param verbosity Integer: Verbosity level.
#' @param res_verbosity Logical: passed to the learning function.
#'
#' @author EDG
#' @export
bias_variance <- function(x, y, mod,
                          res1_train_p = .7,
                          params = list(),
                          resampler_parameters = setup_Resampler(n_resamples = 100),
                          seed = NULL,
                          verbosity = 1L,
                          res_verbosity = 0L, ...) {
  # Outcome
  y <- x[, NCOL(x)]
  # Features
  x <- x[, -NCOL(x)]
  # Create single testing, multiple training sets
  # 1. Test - train
  res1 <- resample(
    y,
    parameters = setup_Resampler(
      n_resamples = 1, type = "StratSub", train_p = res1_train_p, seed = seed
    )
  )
  x_test <- x[-res1$Subsample_1, ]
  true <- y[-res1$Subsample_1]
  dat_train <- data.frame(x, y)[res1$Subsample_1, ]
  res2 <- resample(dat_train, parameters = resampler_parameters)

  p <- progressr::progressor(steps = resampler_parameters$n_resamples)
  mods <- lapply(seq_along(res2), \(i) {
    args <- c(
      list(
        x = dat_train[res2[[i]], ],
        verbosity = res_verbosity
      ),
      params
    )
    p(sprintf("Running resample: %i/%i...", i, resampler_parameters$n_resamples))
    do.call(select_learn(mod), args)
  })

  # predicted: N cases x N resamples
  predicted <- sapply(mods, \(m) predict(m, x_test))

  bias_squared <- (rowMeans(predicted) - true)^2
  variance <- apply(predicted, 1, popvar)

  BiasVariance(
    bias_squared = bias_squared,
    mean_bias_squared = mean(bias_squared),
    sd_bias_squared = sd(bias_squared),
    variance = variance,
    mean_variance = mean(variance),
    sd_variance = sd(variance)
  )
} # rtemis::bias_variance
