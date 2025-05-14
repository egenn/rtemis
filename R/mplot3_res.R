# mplot3_resample
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' `mplot3` Plot `resample`
#'
#' Visualizes resampling output using [mplot3_img]
#'
#' For resampling with no replacement where each case may be selected 0 or 1 time,
#' 0 is white and 1 is teal For resampling with replacement, 0 is white, 1 is blue, 2 is teal
#'
#' @param res rtemis [resample] object
#' @param col Color vector
#' @param mar Numeric vector: image margins
#' @param theme rtemis theme
#' @param ... Additional theme arguments
#'
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' x <- rnorm(500)
#' res <- resample(x)
#' mplot3_res(res)
#' }
#' @export

mplot3_res <- function(res, col = NULL, mar = NULL, theme = rtTheme, ...) {
  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  ind <- seq_len(length(unique(unlist(res))))
  resn <- t(sapply(res, function(i) sapply(ind, function(k) sum(k == i))))
  nlevels <- max(resn) * 2 + 1
  if (is.null(col)) {
    col <- colorGrad(
      nlevels,
      mid = theme$bg,
      midhi = "#006BE9",
      hi = "#16A0AC"
    )
  }
  ynames <- names(res)
  # if (is.null(mar)) mar <- c(2, 7, 2, .5)

  # Plot ----
  mplot3_img(
    resn,
    theme = theme,
    col = col,
    y.axis.las = 2,
    xlab = "Cases",
    xlab.line = 0.5,
    mar = mar
  )
} # rtemis::mplot3_res
