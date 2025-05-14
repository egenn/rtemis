# mplot3_harmonograph
# ::rtemis::
# E.D. Gennatas rtemis.org

# Harmonograph source: https://fronkonstin.com/2014/10/13/beautiful-curves-the-harmonograph/

#' Plot a harmonograph
#'
#' Plot a [harmonograph](https://en.wikipedia.org/wiki/Harmonograph)
#'
#' Unless you define a seed, each graph will be random. Try different seeds if you want to reproduce your graphs.
#' Some seeds to try: 9, 17, 26, 202, 208, ...
#'
#' @param steps Float, vector
#' @param seed Integer
#' @param col Line color. Default = "white"
#' @param alpha Alpha for line color `col`. Default = .2
#' @param bg Color for background. Default = "black"
#' @param lwd Float: Line width
#' @param text Character: Text you want printed along with the harmonograph. Default = NULL
#' @param text.side Integer {1, 2, 3, 4}: `side` argument for `mtext`
#' @param text.line Float: `line` argument for `mtext`
#' @param text.adj Float: `adj` argument for `mtext`
#' @param text.padj Float: `padj` argument for `mtext`
#' @param text.col Color: Text color. Default is same as `col`
#' @param mar Float vector, length 4: Plot margins. (`par`'s `mar` argument)
#' @param oma Float vector, length 4: Outer margins. (`par`'s `oma` argument)
#' @param xlim Float vector, length 2: x-axis limits
#' @param ylim Float vector, length 2: y-axis limits
#' @param new Logical. If TRUE, do not clear plot before drawing
#' @param par.reset Logical. If TRUE, reset par before exit
#' @author E.D. Gennatas
#' @export

mplot3_harmonograph <- function(
  steps = seq(1, 500, by = .01),
  seed = NULL,
  col = "white",
  alpha = .2,
  bg = "black",
  lwd = 1,
  text = NULL,
  text.side = 1,
  text.line = -1,
  text.adj = 0,
  text.padj = 0,
  text.col = NULL,
  mar = c(0, 0, 0, 0),
  oma = c(0, 0, 0, 0),
  xlim = NULL,
  ylim = NULL,
  new = FALSE,
  par.reset = TRUE
) {
  if (!is.null(seed)) set.seed(seed)
  f1 <- jitter(sample(c(2, 3), 1))
  f2 <- jitter(sample(c(2, 3), 1))
  f3 <- jitter(sample(c(2, 3), 1))
  f4 <- jitter(sample(c(2, 3), 1))
  d1 <- runif(1, 0, 1e-02)
  d2 <- runif(1, 0, 1e-02)
  d3 <- runif(1, 0, 1e-02)
  d4 <- runif(1, 0, 1e-02)
  p1 <- runif(1, 0, pi)
  p2 <- runif(1, 0, pi)
  p3 <- runif(1, 0, pi)
  p4 <- runif(1, 0, pi)
  xt <- function(t)
    exp(-d1 * t) * sin(t * f1 + p1) + exp(-d2 * t) * sin(t * f2 + p2)
  yt <- function(t)
    exp(-d3 * t) * sin(t * f3 + p3) + exp(-d4 * t) * sin(t * f4 + p4)
  t <- steps
  x <- xt(t)
  y <- yt(t)

  # Plot ----
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(par(par.orig))
  par(bg = bg, mar = mar, oma = oma)
  cola <- colorAdjust(color = col, alpha = alpha)
  plot(
    x,
    y,
    type = "l",
    lwd = lwd,
    col = cola,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xlim = xlim,
    ylim = ylim,
    new = new
  )

  if (!is.null(text)) {
    if (is.null(text.col)) text.col <- col
    mtext(
      text,
      side = text.side,
      line = text.line,
      adj = text.adj,
      padj = text.padj,
      col = text.col
    )
  }
} # rtemis::mplot3_harmonograph


#' Chill
#'
#' Relax. Use Ctrl-C to exit (but try to stay relaxed)
#'
#' @param sleep Float: Time in seconds between drawings. Default = .5
#' @param text Character: Text to display
#' @param max Integer: Max times to repeat. Default = 1000
#'
#' @export

chill <- function(sleep = .5, text = NULL, max = 1000) {
  msg2("Ctrl-C to exit")
  for (i in seq(max)) {
    Sys.sleep(sleep)
    mplot3_harmonograph(col = sample(2:8, 1), text = text)
  }
} # rtemis::chill
