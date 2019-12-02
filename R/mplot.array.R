# mplot.array.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Plot Array as Image
#'
#' Plots 2D (grayscale) or 3D (color) array as Raster Image
#'
#' @param x Array, 2D or 3D: Input describing grayscale or color image in RGB space
#' @param mono Logical: If TRUE, plot as grayscale using \code{mono.fn} to convert RGB to grayscale. Default = FALSE
#' @param mono.fn Function: Apply this function to the array to convert to 2D for grayscale plotting. Default = mean
#' @param bg Color: Background color (around the plotted image when window proportions do not match image). Default = "gray10"
#' @param par.reset Logical: If TRUE, reset par settings before exiting. Default = TRUE
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' img <- imager::load.image("https://www.r-project.org/logo/Rlogo.png")
#' mplot.array(img)
#' }

mplot.array <- function(x,
                        mono = FALSE,
                        mono.fn = mean,
                        bg = "gray10",
                        par.reset = TRUE,
                        verbose = TRUE) {

  if (dim(x)[3] > 3) {
    .dim <- dim(x)
    if (verbose) msg0("Input has dimensions ", .dim[1], "x", .dim[2], "x", .dim[3],
                     "; Using first 3")
    x <- x[, , seq(3)]
  }

  if (mono) x <- apply(x, c(1, 2), mono.fn)

  if (par.reset) {
    par.orig <- par(no.readonly = TRUE)
    on.exit(par(par.orig))
  }
  par(pty = "s", bg = bg, mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot(NULL, NULL, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ann = FALSE)
  rasterImage(x, 0, 0, 100, 100)

} # rtemis::mplot.array
