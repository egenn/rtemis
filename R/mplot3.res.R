# mplot3.resample
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' \code{mplot3} Plot \code{resample}
#'
#' Visualizes resampling output using \link{mplot3.img}
#'
#' For resampling with no replacement where each case may be selected 0 or 1 time,
#' 0 is white and 1 is teal For resampling with replacement, 0 is white, 1 is blue, 2 is teal
#'
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' x <- rnorm(500)
#' res <- resample(x)
#' mplot3.res(res)
#' }
#' @export

mplot3.res <- function(res,
                       col = NULL,
                       mar = NULL,
                       theme = getOption("rt.theme"), ...) {

  # [ Theme ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  ind <- seq(length(unique(unlist(res))))
  resn <- t(sapply(res, function(i) sapply(ind, function(k) sum(k == i))))
  nlevels <- max(resn) * 2 + 1
  if (is.null(col)) col <- colorGrad(nlevels, mid = theme$bg, midhi = ucsfCol$blue, hi = ucsfCol$teal)
  ynames <- names(res)
  # if (is.null(mar)) mar <- c(2, 7, 2, .5)

  # [ Plot ] ====
  mplot3.img(resn,
             theme = theme,
             col = col,
             y.axis.las = 2,
             xlab = "Cases",
             xlab.line = 0.5,
             mar = mar)

} # rtemis::mplot3.res
