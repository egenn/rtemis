# lincoef.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Linear Model Coefficients
#'
#' Get linear model coefficients
#'
#' This function minimizes checks for speed. It doesn't check dimensionality
#' of `x`.
#' Only use methods "glm", "sgd", or "solve" if there is only one feature in
#' `x`.
#'
#' @param x Feature matrix or data.frame. Will be coerced to data.frame for
#' method = "allSubsets", "forwardStepwise", or "backwardStepwise"
#' @param y Outcome
#' @param weights Float, vector: Case weights
#' @param method Character: Method to use:
#' \itemize{
#'  \item{"glm"}{: uses `stats::lm.wfit`}
#'  \item{"glmnet"}{: uses `glmnet::glmnet`}
#'  \item{"cv.glmnet"}{: uses `glmnet:cv.glmnet`}
#'  \item{"lm.ridge"}{: uses `MASS::lm.ridge`}
#'  \item{"allsubsets"}{: uses `leaps::regsubsets` with `method = "exhaustive"`}
#'  \item{"forwardStepwise"}{: uses `leaps::regsubsets` with `method = "forward"`}
#'  \item{"backwardStepwise"}{: uses `leaps::regsubsets` with `method = "backward"`}
#'  \item{"sgd"}{: uses `sgd::sgd`}
#'  \item{"solve"}{: uses `base::solve`}
#'  \item{"none"}{: fits no model and returns all zeroes, for programming convenience in special cases}
#' }
#' @param type Character: "Regression", "Classification", or "Survival"
#' @param learning.rate Numeric: Coefficients will be multiplied by this number
#' @param alpha Float: `alpha` for method = `glmnet` or
#' `cv.glmnet`.
#' @param lambda Float: The lambda value for `glmnet`, `cv.glmnet`,
#' `lm.ridge`
#' Note: For `glmnet` and `cv.glmnet`, this is the lambda used for
#' prediction. Training uses `lambda.seq`.
#' @param lambda.seq Float, vector: lambda sequence for `glmnet` and
#' `cv.glmnet`.
#' @param cv.glmnet.nfolds Integer: Number of folds for `cv.glmnet`
#' @param which.cv.glmnet.lambda Character: Whitch lambda to pick from
#' cv.glmnet: "lambda.min": Lambda that gives minimum cross-validated error;
#' @param nbest Integer: For `method = "allSubsets"`, number of subsets of
#' each size to record. Default = 1
#' @param nvmax Integer: For `method = "allSubsets"`, maximum number of
#' subsets to examine.
#' @param sgd.model Character: Model to use for `method = "sgd"`.
#' @param sgd.model.control List: `model.control` list to pass to
#' `sgd::sgd`
#' @param sgd.control List: `sgd.control` list to pass to `sgd::sgd`
# #' @param ... Additional parameters to pass to \code{leaps::regsubsets}
#' "lambda.1se": Largest lambda such that error is within 1 s.e. of the minimum.
#' @param trace Integer: If set to zero, all warnings are ignored
#'
#' @return Named numeric vector of linear coefficients
#' @export
#' @author E.D. Gennatas

lincoef <- function(
  x,
  y,
  weights = NULL,
  method = "glmnet",
  type = c("Regression", "Classification", "Survival"),
  learning.rate = 1,
  alpha = 1,
  lambda = .05,
  lambda.seq = NULL,
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = c("lambda.min", "lambda.1se"),
  nbest = 1,
  nvmax = 8,
  sgd.model = "glm",
  sgd.model.control = list(lambda1 = 0, lambda2 = 0),
  sgd.control = list(method = "ai-sgd"),
  trace = 0
) {
  if (trace == 0) {
    warn.orig <- getOption("warn")
    options(warn = -1)
    on.exit(options(warn = warn.orig))
  }
  if (is.null(colnames(x))) colnames(x) <- paste0("x_", seq_len(NCOL(x)))

  type <- match.arg(type)

  if (type != "Regression" && !method %in% c("glmnet", "cv.glmnet")) {
    stop("Use method 'glmnet' for", type)
  }

  if (method == "glm") {
    # '-- "glm": base::lm.wfit ----
    if (is.null(weights)) weights <- rep(1, NROW(y))
    dat <- data.frame(x, y)
    coef <- lm.wfit(model.matrix(y ~ ., dat), y, weights)$coefficients
  } else if (method == "glmnet") {
    # '-- "glmnet": glmnet::glmnet ----
    if (is.null(weights)) weights <- rep(1, NROW(y))
    family <- switch(
      type,
      Regression = "gaussian",
      Classification = ifelse(
        length(levels(y)) == 2,
        "binomial",
        "multinomial"
      ),
      Survival = "cox"
    )
    lin1 <- glmnet::glmnet(
      data.matrix(x),
      y,
      family = family,
      weights = weights,
      alpha = alpha,
      lambda = lambda.seq
    )
    coef <- as.matrix(coef(lin1, s = lambda))[, 1]
  } else if (method == "cv.glmnet") {
    # '-- "cv.glmnet": glmnet::cv.glmnet ----
    which.cv.glmnet.lambda <- match.arg(which.cv.glmnet.lambda)
    if (is.null(weights)) weights <- rep(1, NROW(y))
    family <- switch(
      type,
      Regression = "gaussian",
      Classification = ifelse(
        length(levels(y)) == 2,
        "binomial",
        "multinomial"
      ),
      Survival = "cox"
    )
    lin1 <- glmnet::cv.glmnet(
      data.matrix(x),
      y,
      family = family,
      weights = weights,
      alpha = alpha,
      lambda = lambda.seq,
      nfolds = cv.glmnet.nfolds
    )
    coef <- as.matrix(coef(lin1, s = lin1[[which.cv.glmnet.lambda]]))[, 1]
  } else if (method == "lm.ridge") {
    # '-- "lm.ridge": MASS::lm.ridge ----
    if (!is.null(weights)) stop("method 'lm.ridge' does not support weights")
    dat <- data.frame(x, y)
    lin1 <- MASS::lm.ridge(y ~ ., dat, lambda = lambda)
    coef <- coef(lin1)
  } else if (method == "forwardStepwise") {
    # '-- "forwardStepwise": leaps::regsubsets ----
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(
      x,
      y,
      weights = weights,
      method = "forward",
      nvmax = nvmax
    )
    .coef <- coef(mod, id = which.max(summary(mod)$rsq))
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    names(coef) <- c("(Intercept)", colnames(x))
    coef[names(.coef)] <- .coef
  } else if (method == "backwardStepwise") {
    # '-- "backwardStepwise": leaps::regsubsets ----
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(
      x,
      y,
      weights = weights,
      method = "backward",
      nvmax = nvmax
    )
    .coef <- coef(mod, id = which.max(summary(mod)$rsq))
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    names(coef) <- c("(Intercept)", colnames(x))
    coef[names(.coef)] <- .coef
  } else if (method == "allSubsets") {
    # '-- "allSubsets": leaps::regsubsets ----
    if (is.null(weights)) weights <- rep(1, length(y))
    mod <- leaps::regsubsets(
      x,
      y,
      weights = weights,
      method = "exhaustive",
      really.big = TRUE,
      nbest = nbest,
      nvmax = nvmax
    )
    .coef <- coef(mod, id = which.max(summary(mod)$rsq))
    .nfeat <- NCOL(x)
    coef <- rep(0, .nfeat + 1)
    names(coef) <- c("(Intercept)", colnames(x))
    coef[names(.coef)] <- .coef
  } else if (method == "solve") {
    # '-- solve ----
    if (!is.null(weights)) stop("method 'solve' does not support weights")
    x <- cbind(`(Intercept)` = 1, x)
    coef <- solve(t(x) %*% x, t(x) %*% y)[, 1]
    names(coef) <- colnames(x)
  } else if (method == "sgd") {
    # '-- sgd ----
    if (!is.null(weights))
      stop("provide weights for method 'sgd' using model.control$wmatrix")
    mod <- sgd::sgd(
      x = data.matrix(cbind(Intercept = 1, x)),
      y = y,
      model = sgd.model,
      model.control = sgd.model.control,
      sgd.control = sgd.control
    )
    coef <- c(mod$coefficients)
    names(coef) <- c("(Intercept)", colnames(x))
  } else if (method == "constant") {
    coef <- c(mean(y), rep(0, NCOL(x)))
    names(coef) <- c("(Intercept)", colnames(x))
  } else {
    # "none"
    coef <- rep(0, NCOL(x) + 1)
    names(coef) <- c("(Intercept)", colnames(x))
  }

  learning.rate * coef
} # rtemis::lincoef
