# ridge.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

#' Ridge regression coefficients
#' 
#' @param x Features
#' @param y Outcome
#' @param lambda Float: The lambda parameter
#' @param method String: "glmnet", "MASS", or "R": which method to use to estimate ridge coefficients. 
#' @export
#' @author Efstathios D. Gennatas

ridge <- function(x, y,
                  lambda = 0,
                  method = c("glmnet", "MASS", "R")) {
  
  method <- match.arg(method)
  switch (method,
    glmnet = as.matrix(glmnet::glmnet(x, y, family = "gaussian", alpha = 0, lambda = lambda)$beta)[, 1],
    MASS = MASS::lm.ridge(y ~ x, lambda = lambda)$coef,
    R = solve(t(x) %*% x + lambda * diag(NCOL(x))) %*% (t(x) %*% y)
  )
  
} # ridge.R
