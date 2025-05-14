# nlareg.R
# ::rtemis::
# 2018 E.D. Gennatas

#' \pkg{rtemis} internal: NonLinear Activation regression (NLAreg)
#'
#' @inheritParams s_GLM
#' @param activation String or Function: Activation function to use: provide its name or the function itself.
#' Default = link{softplus}
#' @param b_o Float, vector (length y): Output bias. Defaults to `mean(y)`
#' @param W_o Float: Output weight. Defaults to 1
#' @param b_h Float: Hidden layer bias. Defaults to 0
#' @param W_h Float, vector (length `NCOL(x)`): Hidden layer weights. Defaults to 0
#' @param optim.method Character: Optimization method to use: "Nelder-Mead", "BFGS", "CG", "L-BFGS-B",
#' "SANN", "Brent". See `stats::optim` for more details. Default = `"BFGS"`
#' @param control List: Control parameters passed to `stats::optim`
#'
#' @export
#' @author E.D. Gennatas
#' @return `nlareg` object
#' @keywords internal

nlareg <- function(
  x,
  y,
  b_o = mean(y),
  W_o = 1,
  b_h = 0,
  W_h = 0,
  activation = softplus,
  optim.method = "BFGS",
  control = list(),
  ...
) {
  # Arguments ----

  if (is.character(activation)) {
    fn.name <- activation
    activation <- match.fun(activation)
  } else if (is.function(activation)) {
    fn.name <- deparse(substitute(activation))
  } else {
    stop("Unrecognized activation function supplied")
  }

  x <- as.data.frame(x)
  # feature.names <- colnames(x)
  # weight.names <- paste0("w", seq(feature.names))
  # weight.names <- paste0("w", seq(feature.names))
  # wxf <- paste0(weight.names, "*", feature.names, collapse = " + ")
  # params <- c("b_o", "W_o", "b_h", weight.names)

  if (length(W_h) < NCOL(x)) W_h <- rep(rev(W_h)[1], NCOL(x))

  dat <- data.matrix(data.frame(x, y = y))

  # 1 Sigmoid nla regression:
  # .formula <- as.formula(paste0("y ~ b_o + W_o * sigmoid(b_h + ", wxf,")"))

  # data is x, y
  # par is b_o, W_o, b_h, W_h; W_h has length = N of features in x
  minSS <- function(data, par) {
    nc <- ncol(data)
    x <- data[, -nc, drop = FALSE]
    y <- data[, nc]
    b_o = par[1]
    W_o = par[2]
    b_h = par[3]
    W_h = par[-seq(3)]
    sum((y - (b_o + W_o * activation(b_h + x %*% W_h)))^2)
  }

  # optim ----
  est <- optim(
    c(b_o, W_o, b_h, W_h),
    minSS,
    method = optim.method,
    control = control,
    data = dat
  )

  if (est$convergence > 0)
    warning("Optimizer failed to converge. Error code: ", est$convergence)

  # nlareg object ----
  b_o <- est$par[1]
  W_o <- est$par[2]
  b_h <- est$par[3]
  W_h <- est$par[-seq(3)]
  # change: replace with above
  .nla <- list(
    activation = activation,
    params = list(
      b_o = est$par[1],
      W_o = est$par[2],
      b_h = est$par[3],
      W_h = est$par[-seq(3)]
    ),
    formula = paste0(
      ddSci(b_o),
      " + ",
      ddSci(W_o),
      " * ",
      fn.name,
      "(",
      ddSci(b_h),
      paste0(
        ifelse(W_h >= 0, " + ", " - "),
        ddSci(abs(W_h)),
        "*",
        colnames(x),
        collapse = ""
      ),
      ")"
    ),
    optim.method = optim.method
  )
  class(.nla) <- c("nlareg", "list")
  .nla
} # rtemis::nlareg

#' Predict method for `nlareg` object
#'
#' @param object [nlareg] object
#' @param newdata Data frame of predictors
#' @param ... Unused
#' @method predict nlareg
#' @author E.D. Gennatas
#' @export

predict.nlareg <- function(object, newdata, ...) {
  xm <- data.matrix(newdata)
  b_o <- object$params$b_o
  W_o <- object$params$W_o
  b_h <- object$params$b_h
  W_h <- object$params$W_h
  yhat <- c(b_o + W_o * (object$activation(b_h + xm %*% W_h)))
  yhat
} # rtemis::predicr.nlareg
