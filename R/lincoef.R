# lincoef.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

#' Linear Model Coefficients
#' 
#' Get linear model coefficients
#' 
#' @param x Features
#' @param y Outcome
#' @param weights Float, vector: Case weights
#' @param method String: Method to use: 
#' "glm": uses \code{stats::lm.wfit};
#' "glmnet": uses \code{glmnet::glmnet};
#' "cv.glmnet": uses \code{glmnet:cv.glmnet};
#' "lm.ridge": uses \code{MASS::lm.ridge};
#' "allsubsets": uses \code{leaps::regsubsets} with \code{method = "exhaustive"};
#' "forwardStepwise": uses \code{leaps::regsubsets} with \code{method = "forward};
#' "backwardStepwise": uses \code{leaps::regsubsets} with \code{method = "backward};
#' "solve": uses \code{base::solve}
#' @param alpha Float: \code{alpha} for method = \code{glmnet} or \code{cv.glmnet}. Default = 0
#' @param lambda Float: The lambda value for \code{glmnet}, \code{cv.glmnet}, \code{lm.ridge}
#' Note: For \code{glmnet} and \code{cv.glmnet}, this is the lambda used for prediction. Training uses
#' \code{lambda.seq}. Default = .01
#' @param lambda.seq Float, vector: lambda sequence for \code{glmnet} and \code{cv.glmnet}. Default = NULL
#' @param cv.glmnet.nfolds Integer: Number of folds for \code{cv.glmnet}
#' @param which.cv.glmnet.lambda String: Whitch lambda to pick from cv.glmnet:
#' "lambda.min": Lambda that gives minimum cross-validated error;
#' @param nbest Integer: For \code{method = "allSubsets"}, number of subsets of each size to record. Default = 1
#' @param nvmax Integer: For \code{method = "allSubsets"}, maximum number of subsets to examine.
#' @param ... Additional parameters to pass to \code{leaps::regsubsets}
#' "lambda.1se": Largest lambda such that error is within 1 s.e. of the minimum.
#' @return Coefficients
#' @export
#' @author Efstathios D. Gennatas

lincoef <- function(x, y,
                    weights = NULL,
                    method = c("glmnet",
                               "cv.glmnet",
                               "lm.ridge",
                               "allSubsets",
                               "forwardStepwise",
                               "backwardStepwise",
                               "glm",
                               "solve"),
                    alpha = 0,
                    lambda = .01,
                    lambda.seq = NULL,
                    cv.glmnet.nfolds = 5,
                    which.cv.glmnet.lambda = c("lambda.min", "lambda.1se"),
                    nbest = 1,
                    nvmax = 8, ...) {
  
  method <- match.arg(method)
  which.cv.glmnet.lambda <- match.arg(which.cv.glmnet.lambda)
  
  if (method == "glm") {
    # '-- "glm": base::lm.wfit ====
    if (is.null(weights)) weights <- rep(1, NROW(y))
    dat <- data.frame(x, y)
    coef <- lm.wfit(model.matrix(y ~ ., dat), y, weights)$coefficients
  } else if (method == "glmnet") {
    # '-- "glmnet": glmnet::glmnet ====
    if (is.null(weights)) weights <- rep(1, NROW(y))
    lin1 <- glmnet::glmnet(data.matrix(x), y, family = 'gaussian',
                           weights = weights,
                           alpha = alpha,
                           lambda = lambda.seq)
    coef <- as.matrix(coef(lin1, s = lambda))[, 1]
  } else if (method == "cv.glmnet") {
    # '-- "cv.glmnet": glmnet::cv.glmnet ====
    if (is.null(weights)) weights <- rep(1, NROW(y))
    lin1 <- glmnet::cv.glmnet(data.matrix(x), y, family = 'gaussian',
                              weights = weights,
                              alpha = alpha,
                              lambda = lambda.seq,
                              nfolds = cv.glmnet.nfolds)
    coef <- as.matrix(coef(lin1, s = lin1[[which.cv.glmnet.lambda]]))[, 1]
  } else if (method == "lm.ridge") {
    if (!is.null(weights)) stop("method 'lm.ridge' does not support weights")
    # '-- "lm.ridge": MASS::lm.ridge ====
    dat <- data.frame(x, y)
    lin1 <- MASS::lm.ridge(y ~ ., dat,
                           lambda = lambda)
    coef <- coef(lin1)
  } else if (method == "forwardStepwise") {
    # '-- "forwardStepwise": leaps::regsubsets ====
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(x, y,
                             weights = weights,
                             method = "forward",
                             nvmax = nvmax, ...)
    mod.summary <- summary(mod)
    .coef <- coef(mod, id = nvmax)
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    names(coef) <- c("(Intercept)", paste(seq(.nfeat)))
    coef[names(.coef)] <- .coef
  } else if (method == "backwardStepwise") {
    # '-- "backwardStepwise": leaps::regsubsets ====
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(x, y,
                             weights = weights,
                             method = "backward",
                             nvmax = nvmax, ...)
    mod.summary <- summary(mod)
    .coef <- coef(mod, id = nvmax)
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    names(coef) <- c("(Intercept)", paste(seq(.nfeat)))
    coef[names(.coef)] <- .coef
  } else if (method == "allSubset") {
    # '-- "allSubset": leaps::regsubsets ====
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(x, y,
                             weights = weights,
                             method = "exhaustive",
                             nbest = nbest,
                             nvmax = nvmax, ...)
    mod.summary <- summary(mod)
    id <- which.max(mod.summary$rsq)
    coef <- coef(mod, id = id)
    } else if (method == "solve") {
    if (!is.null(weights)) stop("method 'solve' does not currently support weights")
    # '-- solve ====
    x <- cbind(1, x)
    coef <- solve(t(x) %*% x, t(x) %*% y)[, 1]
  }
  
  coef
  
} # rtemis::lincoef
