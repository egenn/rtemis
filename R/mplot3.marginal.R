# mplot3.marginal.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Marginal Effects
#'
#' Plots and returns invisibly the estimated outcome by varying one predictor and keeping others fixed.
#'   Works on any \pkg{rtemis} model trained on multiple features
#'
#' @param mod \pkg{rtemis} model to use for prediction
#' @param x.name String: Predictor name as used in \code{mod} whose marginal effects you wish to estimate
#'   Predictor names are stored in \code{mod$xnames}
#' @param x Numeric vector: Values at which to evaluate marginal effects. Define them based on the range of
#'   the values used to train the model, or estimates will be unreliable, potentially entirely off.
#' @param fix.at Numeric: Assign this constant to all features other than \code{x.name}. Defaults to 1
#' @param print.plot Logical: if TRUE, draw plot using \link{mplot3}
#' @param plot.type "p" for points, "l" for line
#' @param ... Additional arguments to be passed to \link{mplot3.xy}
#' @return Returns predicted values invisibly
#' @author Efstathios D. Gennatas
#' @export

mplot3.marginal <- function(mod,
                            x.name = "x",
                            x = -10:10,
                            y.name = "y",
                            fix.at = 1,
                            print.plot = TRUE,
                            plot.type = "l", ...) {

  # [ ARGUMENTS ] ====
  if (NCOL(x) > 1) stop("Argument Error: \"x\" must be a vector")
  if (length(mod$xnames) < 2) stop("Argument Error: Input model was trained using single predictor")

  # Construct predictor data frame
  n.col <- length(mod$xnames)
  n.row <- length(x)
  dt <- data.frame(matrix(rep(fix.at, n.col * n.row), n.row))
  names(dt) <- mod$xnames
  dt[[x.name]] <- x

  # [ PREDICTED ] ====
  predicted <- predict(mod, dt)

  # [ PLOT ] ====
  if (print.plot) {
    main <- paste0("Marginal effect of ", x.name, " on ", y.name,
                   "\n(other features fixed at ", fix.at, ")")
    mplot3.xy(x, predicted, main = main, type = plot.type, ...)
  }

  # [ OUTRO ] ====
  invisible(predicted)

} # rtemis::mplot3.marginal
