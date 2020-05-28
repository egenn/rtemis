# JL_splitlin.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io

#' Split where lines would minimize error
#'
#'
#' @author Efstathios D. Gennatas
#' @keywords internal

JL_splitlin <- function(x, y, wts = rep(1, length(y)),
                        lambda = .1,
                        verbose = TRUE) {

  julia <- JuliaCall::julia_setup()
  julia$command("import Rtemis.splitline")
  # julia$library("Rtemis")
  julia$assign("x", data.matrix(x))
  julia$assign("y", y)
  julia$assign("wts", wts)
  julia$assign("lambda", lambda)
  julia$command("lambda = [lambda];")
  sl <- julia$eval("splitline(x, y, wts, lambda = lambda);")
  sl

} # rtemis::JL_splitlin

#' predict JLsplitlin
#'
#' @author Efstathios D. Gennatas
#' @keywords internal
predict.JLsplitlin <- function(object, newdata, ...) {

  estimated <- rep(0, NROW(newdata))
  index <- newdata[, object$featindex] < object$cutoff
  estimated[index] <- predict(object$glmLeft, newdata[index, ])

}
