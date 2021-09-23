# splitline.R
# ::rtemis::
# 2020 E.D. Gennatas lambdamd.org

#' Splitline
#'
#' Perform split in a feature of x to minimize sum of group loss in y
#'
#' This is an R port of splitline.jl from Rtemis.jl. It could be rewritten in a more
#' Rtistic style.
#' @param search Character: "quantile" or "exhaustive". Default = "quantile"
#' @param n.quantiles Integer: Number of quantiles to use if \code{search = "quantile"}
#' @param minobsinnode Integer: Minimum number of caseweights that must be equal to 1 before
#' attempting split
#' @param trace Integer: If greater than 0, print diagnostic messages to console
#' @noRd
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' x <- rnormmat(1000, 20, seed = 2020)
#' y <- x[, 3] + x[, 5] ^ 2 + rnorm(1000)
#' xysplit <- splitline(x, y)
#' xysplit <- splitline(x, y, lambda = .1)
#' }

splitline <- function(x, y,
                      caseweights = rep(1, length(y)),
                      gamma = .1,
                      n.quantiles = 20,
                      minobsinnode = round(.1 * length(y)),
                      minbucket = round(.05 * length(y)),
                      # lincoef
                      lin.type = "glmnet",
                      alpha = 1,
                      lambda = .1,
                      lambda.seq = NULL,
                      cv.glmnet.nfolds = 5,
                      which.cv.glmnet.lambda = "lambda.min",
                      nbest = 1,
                      nvmax = 4,
                      # /lincoef
                      n.cores = 1,
                      trace = 0) {

  if (sum(caseweights == 1) > minobsinnode) {
    nfeatures <- NCOL(x)
    # for each feature: c(cutpoint, loss)
    minloss_perfeat <- pbapply::pbsapply(seq(nfeatures), function(i) {
      cutnsplit(x, y,
                caseweights = caseweights,
                index = i,
                n.quantiles = n.quantiles,
                gamma = gamma,
                # lincoef
                lin.type = lin.type,
                alpha = alpha,
                lambda = lambda,
                lambda.seq = lambda.seq,
                cv.glmnet.nfolds = cv.glmnet.nfolds,
                which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                nbest = nbest,
                nvmax = nvmax,
                # /lincoef
                minbucket = minbucket,
                trace = trace)

    }, cl = n.cores)

    if (all(minloss_perfeat[2, ] == Inf)) {
      return(list(featindex = NA,
                  cutoff = NA,
                  loss = NA))
    }
    featindex <- which.min(minloss_perfeat[2, ])
    list(featindex = featindex,
         cutoff = minloss_perfeat[1, featindex],
         loss = minloss_perfeat[2, featindex])
  } else {
    if (trace > 1) msg("Node has fewer caseweights equal to 1 than minobsinnode threshold of",
                       minobsinnode)
    list(featindex = NA,
         cutoff = NA,
         loss = NA)
  }

} # rtemis::splitline

# Input: Matrix with all features, but work on one. Output: cutpoints and loss
cutnsplit <- function(x, y,
                      caseweights,
                      index,
                      n.quantiles = 20,
                      gamma = .05,
                      # lincoef
                      lin.type,
                      alpha = 1,
                      lambda = .1,
                      lambda.seq = NULL,
                      cv.glmnet.nfolds = 5,
                      which.cv.glmnet.lambda = "lambda.min",
                      nbest = 1,
                      nvmax = 4,
                      # /lincoef
                      minbucket = 10,
                      trace = 0) {

  ncases <- NROW(x)

  cutpoints <- quantile(x[, index],
                        probs = seq(0, 1, length.out = n.quantiles + 1),
                        names = FALSE)[-c(1, n.quantiles + 1)]

  loss <- sapply(seq(cutpoints), function(i) {
    if (trace > 1) msg0("Testing cutpoint ", i, "...")
    indexLeft <- x[, index] < cutpoints[i]

    # Check minbucket
    if (sum(indexLeft) < minbucket | (ncases - sum(indexLeft) < minbucket)) return(Inf)
    weightsLeft <- weightsRight <- caseweights
    weightsLeft[!indexLeft] <- weightsLeft[!indexLeft] * gamma
    weightsRight[indexLeft] <- weightsRight[indexLeft] * gamma

    # Check either y is constant
    .constant <- is.constant(y) | is.constant(y * weightsLeft) | is.constant(y * weightsRight)
    if (.constant) {
      if (trace > 1) msg("y is constant, abort split", color = crayon::magenta)
      return(Inf)
    }

    # '- lm left ====
    elnetLeft <- try(cbind(1, x) %*% lincoef(x, y, weightsLeft,
                                             method = lin.type,
                                             alpha = alpha,
                                             lambda = lambda,
                                             lambda.seq = lambda.seq,
                                             cv.glmnet.nfolds = cv.glmnet.nfolds,
                                             which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                                             nbest = nbest,
                                             nvmax = nvmax),
                     outFile = stdout(),
                     silent = trace < 2)
    if (inherits(elnetLeft, "try-error")) return(Inf)

    # '- lm right ====
    elnetRight <- try(cbind(1, x) %*% lincoef(x, y, weightsRight,
                                              method = lin.type,
                                              alpha = alpha,
                                              lambda = lambda,
                                              lambda.seq = lambda.seq,
                                              cv.glmnet.nfolds = cv.glmnet.nfolds,
                                              which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                                              nbest = nbest,
                                              nvmax = nvmax),
                      outFile = stdout(),
                      silent = trace < 2)
    if (inherits(elnetRight, "try-error")) return(Inf)

    mean(c((y[indexLeft] - elnetLeft[indexLeft])^2,
           y[!indexLeft] * (y[!indexLeft] - elnetRight[!indexLeft])^2))

  })
  index <- which.min(loss)
  if (length(index) == 0) {
    c(NA, Inf)
  } else {
    c(cutpoints[index], loss[index])
  }

} # cutnsplit
