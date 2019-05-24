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
#' "sgd": uses \code{sgd::sgd}
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
#' @param sgd.model String: Model to use for \code{method = "sgd"}. Default = "glm"
#' @param sgd.model.control List: \code{model.control} list to pass to \code{sgd::sgd}
#' @param sgd.control List: \code{sgd.control} list to pass to \code{sgd::sgd}
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
                               "sgd",
                               "solve"),
                    alpha = 0,
                    lambda = .01,
                    lambda.seq = NULL,
                    cv.glmnet.nfolds = 5,
                    which.cv.glmnet.lambda = c("lambda.min", "lambda.1se"),
                    nbest = 1,
                    nvmax = 8,
                    sgd.model = "glm",
                    sgd.model.control = list(lambda1 = 0,
                                             lambda2 = 0),
                    sgd.control = list(method = "ai-sgd"),
                    ...) {

  method <- match.arg(method)

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
    which.cv.glmnet.lambda <- match.arg(which.cv.glmnet.lambda)
    if (is.null(weights)) weights <- rep(1, NROW(y))
    lin1 <- glmnet::cv.glmnet(data.matrix(x), y, family = 'gaussian',
                              weights = weights,
                              alpha = alpha,
                              lambda = lambda.seq,
                              nfolds = cv.glmnet.nfolds)
    coef <- as.matrix(coef(lin1, s = lin1[[which.cv.glmnet.lambda]]))[, 1]
  } else if (method == "lm.ridge") {
    # '-- "lm.ridge": MASS::lm.ridge ====
    if (!is.null(weights)) stop("method 'lm.ridge' does not support weights")
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
    .coef <- coef(mod, id = nvmax)
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    .names <- colnames(x)
    if (is.null(.names)) .names <- seq(.nfeat)
    names(coef) <- c("(Intercept)", .names)
    coef[names(.coef)] <- .coef
  } else if (method == "backwardStepwise") {
    # '-- "backwardStepwise": leaps::regsubsets ====
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(x, y,
                             weights = weights,
                             method = "backward",
                             nvmax = nvmax, ...)
    .coef <- coef(mod, id = nvmax)
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    .names <- colnames(x)
    if (is.null(.names)) .names <- seq(.nfeat)
    names(coef) <- c("(Intercept)", .names)
    coef[names(.coef)] <- .coef
  } else if (method == "allSubsets") {
    # '-- "allSubsets": leaps::regsubsets ====
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(x, y,
                             weights = weights,
                             method = "exhaustive",
                             really.big = TRUE,
                             nbest = nbest,
                             nvmax = nvmax, ...)
    mod.summary <- summary(mod)
    id <- which.max(mod.summary$rsq)
    .coef <- coef(mod, id = id)
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    .names <- colnames(x)
    if (is.null(.names)) .names <- seq(.nfeat)
    names(coef) <- c("(Intercept)", .names)
    coef[names(.coef)] <- .coef
  } else if (method == "solve") {
    # '-- solve ====
    if (!is.null(weights)) stop("method 'solve' does not currently support weights")
    x <- cbind(1, x)
    coef <- solve(t(x) %*% x, t(x) %*% y)[, 1]
    .names <- colnames(x)
    .names <- if (!is.null(.names)) .names else paste0("Feature", seq(NCOL(x)))
    names(coef) <- c("(Intercept)", .names)
  } else if (method == "sgd") {
    # '-- sgd ====
    if (!is.null(weights)) stop("provide weights for method 'sgd' using model.control$wmatrix")
    mod <- sgd::sgd(x = data.matrix(cbind(Intercept = 1, x)),
                    y = y,
                    model = sgd.model,
                    model.control = sgd.model.control,
                    sgd.control = sgd.control)
    coef <- c(mod$coefficients)
    .names <- colnames(x)
    .names <- if (!is.null(.names)) .names else paste0("Feature", seq(NCOL(x)))
    names(coef) <- c("(Intercept)", .names)

  }

  coef

} # rtemis::lincoef
