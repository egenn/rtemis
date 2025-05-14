# mplot_hsv.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Plot HSV color range
#'
#' @param h.steps Float, vector: Hue values to plot.
#' Default = `seq(0, 1, .0125)`
#' @param s.steps Float, vector: Saturation values to plot. Default = same as
#' `h.steps`
#' @param v Float: Value.
#' @param alpha Float: Alpha.
#' @param pch Integer: pch plot parameter. Default = 15 (square)
#' @param bg Color: Background color. Default = "black"
#' @param axes Logical: for `type = "square"`: If TRUE, draw axes.
#' @param pty Character: for `type = "square"`: "s", "r", par's pty
#' argument. Default = "s" (square plot)
#' @param cex Float: `par/plot`'s cex argument.
#' @param mar Float, vector: for `type = "square"`: `par`'s mar
#' argument.
#' @param lab.col Color: Color for axes and labels. Defaults to inverse of
#' `bg`, i.e. white if bg is black
#' @param type Character: "square" for square plot, "radial" for radial plot.
#' @param show.grid Logical: if TRUE, show grid then type is "radial"
#' @param par.reset Logical: If TRUE, reset `par` before exit
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' mplot_hsv()
#' }
mplot_hsv <- function(
  h.steps = seq(0, 1, .025),
  s.steps = seq(0, 1, .05),
  v = 1,
  alpha = 1,
  pch = 16,
  bg = "black",
  axes = TRUE,
  pty = "s",
  cex = 2,
  mar = c(3, 3, 2, .5),
  lab.col = NULL,
  type = c("radial", "square"),
  # radial
  line.col = "gray50",
  show.grid = TRUE,
  show.radial.grid = FALSE,
  show.grid.labels = 1,
  cex.axis = 1,
  cex.lab = 1,
  par.reset = TRUE
) {
  # Arguments ----
  type <- match.arg(type)

  # Plot ----
  par.orig <- par(no.readonly = TRUE)
  on.exit(par(par.orig))

  if (is.null(lab.col)) {
    lab.col <- colorOp(bg, "invert")[[1]]
  }

  grd <- expand.grid(h.steps, s.steps)
  col <- hsv(h = grd[, 1], s = grd[, 2], v = v, alpha = alpha)

  if (type == "square") {
    # '- Square ----
    par(bg = bg, mar = mar, pty = pty)
    plot(
      grd,
      xlim = c(0, 1),
      ylim = c(0, 1),
      pch = pch,
      col = col,
      axes = FALSE,
      ann = FALSE,
      cex = cex
    )

    if (axes) {
      axis(1, col = lab.col, col.ticks = lab.col, col.axis = lab.col)
      axis(2, col = lab.col, col.ticks = lab.col, col.axis = lab.col)
    }
    mtext("H", 1, col = lab.col, line = 2, font = 2)
    mtext("S", 2, col = lab.col, line = 2, font = 2)
    mtext(
      paste0("HSV color (V = ", v, ")"),
      3,
      adj = 0,
      font = 2,
      line = .5,
      col = lab.col
    )
  } else {
    # '- Radial ----
    dependency_check("plotrix")
    par(bg = bg, cex.axis = cex.axis, cex.lab = cex.lab)
    plotrix::radial.plot(
      drange(grd[, 2], 0, 360 * pi / 180),
      drange(grd[, 1], 0, 360 * pi / 180),
      labels = ddSci(h.steps),
      start = pi / 2,
      clockwise = TRUE,
      rp.type = "s",
      point.symbols = pch,
      point.col = col,
      line.col = line.col,
      label.prop = 1,
      show.grid = show.grid,
      show.grid.labels = show.grid.labels,
      show.radial.grid = show.radial.grid,
      radial.labels = "",
      boxed.radial = FALSE,
      cex = cex,
    )
    mtext(
      paste0("HSV color (V = ", v, ")"),
      font = 1,
      col = lab.col
    )
  }
} # rtemis::mplot_hsv
