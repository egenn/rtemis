# basegrid.R
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

basegrid <- function(x = NULL,
                     y = NULL,
                     col = NULL,
                     lty = NULL,
                     lwd = NULL,
                     theme = getOption("rt.theme")) {
  
  # Theme ====
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


