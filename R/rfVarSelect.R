# rfVarSelect.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Variable Selection by Random Forest
#'
#' Select important variables from a set of features based on RF-estimated variable
#' importance
#'
#' Please note that this function is included for academic and exploratory purposes.
#' It may be best to rely on each supervised learning algorithm's own variable selection
#' approach.
#'
#' @param x Predictors
#' @param y outcome
#' @param p Float (0, 1): Fraction of variables in x to select. `p * ncol(x)`. May help to set to a fraction twice
#'   what you expect to be the true fraction of useful variables, to reduce false negatives at the expense of false
#'   positives which can be dealt by an appropriate learning algorithm.
#' @param print.plot Logical: If TRUE, print plot of variable importance
#' @param verbose Logical: If TRUE, print messages to console.
#'
#' @author E.D. Gennatas
#' @export
rfVarSelect <- function(x, y, p = .2, print.plot = TRUE, verbose = TRUE) {
  n <- NCOL(x)
  if (n < 2) stop("You need 2 or more variables to select from")
  start.time <- intro(verbose = verbose)
  if (verbose) msg2("Running Variable Selection using Random Forest...")
  mod <- s_RF(x, y, importance = TRUE)
  importance <- mod$mod$importance[, 1]
  if (print.plot) mplot3_x(mod$mod$importance, group.title = "")
  importance.rank <- order(importance, decreasing = TRUE)
  top.index <- importance.rank[1:(p * n)]
  if (verbose)
    msg2(
      length(top.index),
      "variables selected based on RF-estimated importance"
    )
  outro(start.time, verbose = verbose)
  return(top.index)
} # rtemis:: rfVarSelect
