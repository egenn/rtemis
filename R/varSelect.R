# varSelect.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Variable Selection by Variable Importace
#'
#' Select important variables from a set of features based on RANGER- or XGBLIN-estimated variable importance
#'
#' @param x Matrix / Data Frame of Predictors
#' @param y Outcome vector
#' @param method Character: "RANGER", "XGBLIN": Learner to use for estimating variable importace. Default = "RANGER"
#' @param xgb.params List of parameters for \code{method = "XGBLIN"}
#' @param p Float (0, 1): Fraction of variables in x to select. \code{p * ncol(x)}. May help to set to a fraction twice
#'   what you expect to be the true fraction of useful variables, to reduce false negatives at the expense of false
#'   positives which can be dealt by an appropriate learning algorithm. (Default = .2)
#' @param print.plot Logical: If TRUE, print index plot of variable importance using \link{mplot3.x}
#' @param verbose Logical: If TRUE, print messages to screen
#' @author E.D. Gennatas
#' @export
varSelect <- function(x, y,
                      method = c("RANGER", "XGBLIN"),
                      xgb.params = list(alpha = .1, lambda = .1),
                      p = .2,
                      print.plot = TRUE,
                      verbose = TRUE) {

  # Intro ====
  method <- match.arg(method)
  n <- NCOL(x)
  if (n < 2) stop("You need 2 or more variables to select from")
  start.time <- intro(verbose = verbose)

  # Model ====
  if (method == "RANGER") {
    if (verbose) msg("Running Variable Selection using Random Forest...")
    mod <- s.RANGER(x, y)
    importance <- mod$varimp
  } else {
    if (verbose) msg("Running Variable Selection using XGboost with linear booster...")
    mod <- do.call(s.XGBLIN, args = c(list(x = x, y = y), xgb.params))
    importance <- xgboost::xgb.importance(feature_names = mod$xnames, model = mod$mod)$Weight
  }

  # Plot ====
  if (print.plot) mplot3.x(importance)
  importance.rank <- order(abs(importance), decreasing = TRUE)
  top.rank <- importance.rank[1:(p * n)]
  if (verbose) msg(length(top.rank), " variables selected based on ", method, "-estimated importance", sep = "")
  outro(start.time, verbose = verbose)
  return(sort(top.rank))

} # rtemis::varSelect
