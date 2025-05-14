# mplot_raster.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Plot Array as Raster Image
#'
#' Plots 2D (grayscale) or 3D (color) array as Raster Image
#'
#' @param x Array, 2D or 3D: Input describing grayscale or color image in RGB space
#' @param mono Logical: If TRUE, plot as grayscale using `mono.fn` to convert RGB to grayscale. Default = FALSE
#' @param mono.fn Function: Apply this function to the array to convert to 2D for grayscale plotting. Default = mean
#' @param bg Color: Background color (around the plotted image when window proportions do not match image). Default = "gray10"
#' @param par.reset Logical: If TRUE, reset par settings before exiting. Default = TRUE
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' img <- imager::load.image("https://www.r-project.org/logo/Rlogo.png")
#' mplot_raster(img)
#' }

mplot_raster <- function(
  x,
  max.value = max(x),
  mar = NULL,
  main = NULL,
  main.line = 0,
  main.side = 3,
  main.col = "#ffffff",
  main.adj = 0,
  main.font = 2,
  mono = FALSE,
  mono.fn = mean,
  bg = "gray10",
  par.set = TRUE,
  par.reset = TRUE,
  verbose = TRUE
) {
  if (dim(x)[3] > 3) {
    .dim <- dim(x)
    if (verbose)
      msg20(
        "Input has dimensions ",
        .dim[1],
        "x",
        .dim[2],
        "x",
        .dim[3],
        "; Using first 3"
      )
    x <- x[,, seq(3)]
  }

  if (mono) x <- apply(x, c(1, 2), mono.fn)

  if (!par.set) par.reset <- FALSE
  if (par.reset) {
    par.orig <- par(no.readonly = TRUE)
    on.exit(par(par.orig))
  }
  if (is.null(mar)) {
    mar <- if (is.null(main)) rep(0, 4) else c(0, 0, 1, 0)
  }
  if (par.set) {
    par(
      pty = "s",
      bg = bg,
      mar = mar,
      oma = c(0, 0, 0, 0),
      xaxs = "i",
      yaxs = "i"
    )
  }
  plot(
    NULL,
    NULL,
    xlim = c(0, 100),
    ylim = c(0, 100),
    axes = FALSE,
    ann = FALSE
  )
  if (!is.null(main)) {
    mtext(
      main,
      main.side,
      line = main.line,
      col = main.col,
      adj = main.adj,
      font = main.font
    )
  }
  rasterImage(x / max.value, 0, 0, 100, 100)
} # rtemis::mplot_raster
