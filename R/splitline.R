# splitline.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io

#' Splitline
#'
#' Perform split in a feature of x to minimize sum of group loss in y
#'
#' This is an R port of splitline.jl from Rtemis.jl. It could be rewritten in a more
#' Rtistic style.
#' @param search String: "quantile" or "exhaustive". Default = "quantile"
#' @param n_quantiles Integer: Number of quantiles to use if \code{search = "quantile"}
#' @param trace Integer: If greater than 0, print diagnostic messages to console
#' @keywords internal
#' @author Efstathios D. Gennatas
#' @examples
#' \dontrun{
#' x = Array(rnormmat(1000, 20, seed = 2020))
#' y = x[:, 3] .+ x[:, 5] .^ 2 .+ randn(1000)
#' xysplit = splitline(x, y)
#' xysplit = splitline(x, y, lambda = [0.1])
#' }

splitline <- function(x, y,
                      caseweights = rep(1, length(y)),
                      gamma = .1,
                      search = "quantile",
                      n_quantiles = 20,
                      minbucket = 5,
                      # Lasso
                      alpha = 1.0,
                      lambda = .1,
                      trace = 0) {

  ncases <- NROW(x)
  nfeatures <- NCOL(x)
  minloss_perfeat <- matrix(Inf, nfeatures, 2)

  for (i in seq(nfeatures)) {
    # '- cutpoints ====
    if (search == "quantile") {
      cutpoints <- quantile(x[, i], probs = seq(0, 1, length.out = n_quantiles + 1))[-c(1, n_quantiles + 1)]
    } else {
      cutpoints <- sort(unique(x[, i]))
      cutpoints <- cutpoints[-length(cutpoints)]
    }
    loss1 <- rep(Inf, length(cutpoints))
    for (j in seq(cutpoints)) {
      if (trace > 1) msg0("Testing feature ", i, "; cutpoint ", j, "...")
      indexLeft <- x[, i] < cutpoints[j]
      if (sum(indexLeft) < minbucket | (ncases - sum(indexLeft) < minbucket)) break
      weightsLeft <- weightsRight <- caseweights
      weightsLeft[!indexLeft] <- weightsLeft[!indexLeft] * gamma
      weightsRight[indexLeft] <- weightsRight[indexLeft] * gamma
      # '- lm left ====
      elnetLeft <- c(predict(glmnet::glmnet(x, y,
                                            weights = weightsLeft,
                                            alpha = alpha, lambda = lambda), x))
      # '- lm right ====
      elnetRight <- c(predict(glmnet::glmnet(x, y,
                                             weights = weightsRight,
                                             alpha = alpha, lambda = lambda), x))
      loss1[j] <- sum((y[indexLeft] - elnetLeft[indexLeft])^2) +
        sum(y[!indexLeft] * (y[!indexLeft] - elnetRight[!indexLeft])^2)
    } # loop through cutpoints
    whichmin <- which.min(loss1)
    minloss_perfeat[i, ] <- c(cutpoints[whichmin], loss1[whichmin])
  } # loop through features

  featindex <- which.min(minloss_perfeat[, 2])

  list(featindex = featindex,
       cutoff = minloss_perfeat[featindex, 1],
       loss = minloss_perfeat[featindex, 2])
} # rtemis::splitline
