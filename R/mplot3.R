# mplot3.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

#' \pkg{rtemis} static plotting
#'
#' Draw quality plots using fast, base graphics
#'
#' @param x Numeric vector or list of vectors
#' @param y Numeric vector or list of vectors
#' @param type String. "scatter", "density", "index", "heatmap"
#' @param fit String. Any `rtemis` model, best to use "lm" for linear fits and "gam" for non-linear fits
#' @param ... Additional arguments to be passed to
#' @author Efstathios D. Gennatas
#' @export

mplot3 <- function(x, y = NULL,
                   type = NULL,
                   fit = NULL,
                   ...) {

  # [ ARGUMENTS ]
  if (is.null(type)) {
    if (is.null(y)) {
      if (length(y) < 50) type <- "index" else type <- "density"
    }
  }

  if (!is.null(fit)) if (fit == "none") fit <- NULL # easier to work with shiny

  # [ MPLOT3 ]
  if (is.null(y)) {
    if (NCOL(x) == 1) {
      mplot3.x(x, type = type, ...)
    } else {
      mplot3.img(x, ...)
    }
  } else {
    mplot3.xy(x, y, type = type, fit = fit, ...)
  }

} # rtemis::mplot

# pennPalette <- structure(list(penn.red.a = "#1C4088", penn.blue.a = "#AF001E",
#                         penn.green.a = "#1C8E1C", penn.yellow.a = "#F2CB30",
#                         penn.purple.a = "#971E89", penn.orange.a = "#C36F27",
#                         penn.lighterBlue = "#045EA7", lighterRed = "#C2004D",
#                         lightestGreen = "#80DF80"),
#                    .Names = c("penn.red.a", "penn.blue.a", "penn.green.a",
#                               "penn.yellow.a", "penn.purple.a", "penn.orange.a",
#                               "penn.lighterBlue", "lighterRed", "lightestGreen"
#                    ))
