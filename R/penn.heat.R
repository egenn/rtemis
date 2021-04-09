# penn.heat.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org
#
# Gradient of Penn colors
# -----------------------
#
# Creates a gradient of penn colors from light blue (Penn blue with increaseed Value in HSV space),
# to Penn blue (the middle number in the output), to Penn red, to Penn yellow with increased Value.
# Best to provide an odd number, so that Penn blue is the middle color and at least n of 5
#
# Arguments
#         n       Integer. Number of discreet colors to create.
#         space   "RGB" or "Lab". Defaults to "Lab": better perceptual separation
#                 (though may fail to include the precise requested color at the limits)

#' Create a color gradient
#'
#' Creates a gradient of colors ranging from light Penn blue (Penn blue with increased Value in HSV)
#' to Penn blue, to Penn red, to light Penn yellow.
#' It is best to provide an odd number, so that there is always an equal number of colors on either side
#' of the midpoint, which will be Penn blue.
#' For example, if you want a gradient from -1 to 1 or equivalent, an n = 11, will give 5 colors on either
#' side of 0, each representing a 20% change from the next.
#'
#' @param n Integer. How many distinct colors you want. Defaults to 11
#' @param space String. Which colorspace to use. Option: "RGB", "Lab". Defaults to "Lab
#' @param demo Logical. Plot the colors horizontally
#' @param colorbar Logical. Create a vertical colorbar
#' @param bar.min Numeric. Lowest value in colorbar
#' @param bar.mid Numeric. Middle value in colorbar
#' @param bar.max Numeric. Max value in colorbar
#' @param filename Optional. Path to file to save colorbar
#' @param col.test Color for colorbar text
#' @export

penn.heat <- function(n = 11, space = "Lab",
                      demo = FALSE,
                      colorbar = FALSE,
                      bar.min = -1, bar.mid = 0, bar.max = 1,
                      cex = 1.2,
                      filename = NULL,
                      col.text = "black") {

  # [ Arguments ]
  if (!is.null(filename)) colorbar <- TRUE

  # [ GRAD ]
  n <- as.integer(n)
  midpoint <- ceiling(n/2)
  if (space == "RGB") {
    penn.vlight2blue <- grDevices::colorRampPalette(c(colorAdjust(pennCol$blue, hue = -.08, val = .568), pennCol$blue))
    penn.blue2red2yellow <- grDevices::colorRampPalette(c(pennCol$blue, pennCol$red, pennCol$yellow))
    grad <- c(penn.vlight2blue(midpoint), penn.blue2red2yellow(n - midpoint))
  } else {
    penn.vlight2blue.lab <- grDevices::colorRampPalette(c(colorAdjust(pennCol$blue, hue = -.08, val = .568), pennCol$blue),
                                             space = "Lab")
    penn.blue2red2yellow.lab <- grDevices::colorRampPalette(c(pennCol$blue, colorAdjust(pennCol$red, val = .1), pennCol$yellow),
                                                 space = "Lab")
    grad <- c(penn.vlight2blue.lab(midpoint), penn.blue2red2yellow.lab(n - midpoint + 1)[-1])
  }

  # [ DEMO ]
  if (demo) {
    plot(rep(1, n), col = grad, pch = 19, cex = 6,
         xlim = c(0.5, n + .5), ylim = c(.8, 1.2),
         ann = F, axes = FALSE)
    text(x = 0.25, y = 1.05, labels = paste0("Penn heat colors (n = ", n, ")"), adj = 0, cex = 1.5)
    segments(midpoint, .95, midpoint, 1.05, lwd = 2, lty = 2, col = NA)
  }

  if (colorbar) {
    # bar.grad <- c(penn.vlight2blue.lab(31), penn.blue2red2yellow.lab(30))
    par.orig <- par(no.readonly = TRUE)
    par(mar = c(1, 1, 1, 1))
    if (!is.null(filename)) grDevices::pdf(filename, width = 3, height = 9)
    plot(rep(1, n), 1:n, col = grad, pch = 19, cex = 6,
         xlim = c(0.5, 1.5), ylim = c(.5, n + .5),
         ann = F, axes = FALSE)
    # text(1.5, c(1, midpoint, n), labels = c(bar.min, bar.mid, bar.max), col = col.text)
    axis(side = 4, at = c(1, midpoint, n), labels = c(bar.min, bar.mid, bar.max),
         col = colorAdjust("black", 0), col.axis = col.text, col.ticks = colorAdjust("black", 0),
         pos = 1.1, las = 1, cex.axis = cex, hadj = 0)
    if (!is.null(filename)) grDevices::dev.off()
    par(par.orig)
  }

  return(grad)

} # rtemis::penn.heat
