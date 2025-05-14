# plotops.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Get estimated text width
#'
#' @keywords internal
#' @noRd
# nchar 46 => 19.5
# nchar 20 => 10
# lm(c(10, 19.5) ~ c(20, 46))
textwidth <- function(x) {
  .nchar <- if (is.null(x)) 1 else max(nchar(x), na.rm = TRUE)
  2.6923 + 0.3654 * .nchar
}

#' Get y below current plot area
#'
#' @param pct_lower Numeric: Get y this percent below bottom of plot
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

ylo <- function(pct_lower = .08) {
  ylo <- par("usr")[3]
  yhi <- par("usr")[4]
  ylo - pct_lower * (yhi - ylo)
}

#' Get y above current plot area
#'
#' @param pct_higher Numeric: Get y this percent above top of plot
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

yhi <- function(pct_higher = .08) {
  ylo <- par("usr")[3]
  yhi <- par("usr")[4]
  yhi + pct_higher * (yhi - ylo)
}

xleft <- function(pct_left = .08) {
  xleft <- par("usr")[1]
  xright <- par("usr")[2]
  xleft - pct_left * (xright - xleft)
}

xright <- function(pct_right = .08) {
  xleft <- par("usr")[1]
  xright <- par("usr")[2]
  xright + pct_right * (xright - xleft)
}

ymid <- function() .5 * sum(par("usr")[3:4])

xmid <- function() .5 * (par("usr")[1:2])

getlim <- function(x, axs = c("r", "i"), axs.r.pct = .04) {
  axs <- match.arg(axs)

  .x <- na.exclude(x)
  .min <- min(.x)
  .max <- max(.x)

  if (axs == "r") {
    .diff <- .max - .min
    c(.min - axs.r.pct * .diff, .max + axs.r.pct * .diff)
  } else {
    c(.min, .max)
  }
} # rtemis::getlim

# basegrid.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

basegrid <- function(
  x = NULL,
  y = NULL,
  col = NULL,
  lty = NULL,
  lwd = NULL,
  theme = rtTheme
) {
  # Theme ----
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), list())
  } else {
    stopifnot(is.list(theme))
  }

  if (is.null(col)) {
    col <- colorAdjust(theme$grid.col, theme$grid.alpha)
  }

  if (is.null(lty)) {
    lty <- theme$grid.lty
  }

  if (is.null(lwd)) {
    lwd <- theme$grid.lwd
  }

  if (!is.null(x)) abline(v = x, col = col, lty = lty, lwd = lwd)
  if (!is.null(y)) abline(h = y, col = col, lty = lty, lwd = lwd)
} # rtemis::basegrid
