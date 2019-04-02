# glmLite.R
# ::rtemis::
# Efstathios D. Gennatas MBBS PhD egenn.github.io

#' Bare bones decision tree derived from \code{rpart}
#' 
#' A super-stripped down decision tree for when space and performance are critical
#' 
#' @inheritParams lincoef
#' @param save.fitted Logical: If TRUE, save fitted values in output. Default = FALSE
#' @author Efstathios D Gennatas
#' @export

glmLite <- function(x, y,
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
                    nvmax = 8,
                    save.fitted = FALSE,
                    trace = 0, ...) {
  
  # Arguments ====
  method <- match.arg(method)
  which.cv.glmnet.lambda <- match.arg(which.cv.glmnet.lambda)
  args <- c(list(x = x,
                 y = y,
                 weights = weights,
                 method = method,
                 alpha = alpha,
                 lambda = lambda,
                 lambda.seq = lambda.seq,
                 cv.glmnet.nfolds = cv.glmnet.nfolds,
                 which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                 nbest = nbest,
                 nvmax = nvmax),
            list(...))
  
  coefs <- do.call(lincoef, args)
  lin <- list(coefs = coefs)
  
  if (save.fitted) lin$fitted <- c(cbind(1, as.matrix(x)) %*% coefs)
  
  class(lin) <- "glmLite"
  lin
  
} # rtemis::glmLite


#' Predict method for \code{glmLite} object
#' 
#' @param object \link{glmLite} object
#' @param newdata Data frame of predictors
#' @param verbose Logical: If TRUE, print messages to console. Default = FALSE
#' @method predict glmLite
#' @author Efstathios D. Gennatas
#' @export

predict.glmLite <- function(object, newdata, verbose = FALSE, ...) {
  
  c(cbind(1, as.matrix(newdata)) %*% object$coefs)
  
} # rtemis::predict.glmLite
