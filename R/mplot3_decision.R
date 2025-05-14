# mplot3_decision
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' `mplot3`: Decision boundaries
#'
#' Plot classification decision boundaries of rtemis models
#'
#' If data has more than 2 variables, any variable not selected using `vars` will be fixed to their mean
#' Underlying model (e.g. `randomForest`, `rpart`, etc) must support standard R predict format for classification:
#' `predict(model, newdata, type = "class")`
#' @param rtmod rtemics trained model
#' @param data Matrix / data frame of features; last column is class
#' @param vars Integer vector, length 2: Index of features (columns of `x`) to use to draw decision
#' boundaries. Default = c(1, 2)
#' @param dots.per.axis Integer: Draw a grid with this many dots on each axis. Default = 100
#' @param bg.cex Float: Point cex for background / decision surface. Default = .5
#' @param bg.alpha Float: Point alpha for background / decision surface. Default = .2
#' @param bg.pch Integer vector: pch for background / decision surface. Default = c(3, 4)
#' @param par.reset Logical: If TRUE, reset `par` before exiting. Default = TRUE
#' @param theme Character: Theme for [mplot3_xy], "light" or "dark". Default = "light'
#' @param col Color vector for classes. Default = `ucsfPalette`
#' @param contour.col Color for decision boundary. Default = "black"
#' @param contour.lwd Float: Line width for decision boundary. Default = .3
#' @param point.pch Integer: pch for data points. Default = c(3, 4)
#' @param point.alpha Float: Alpha for data points. Default = 1
#' @return Predicted labels for background grid (invisibly)
#' @examples
#' \dontrun{
#' dat <- as.data.frame(mlbench::mlbench.2dnormals(200))
#' mod.cart <- s_CART(dat)
#' mod.rf <- s_RF(dat)
#' mplot3_decision(mod.cart, dat)
#' mplot3_decision(mod.rf, dat)
#' }
#' @author E.D. Gennatas
#' @export

mplot3_decision <- function(
  rtmod,
  data,
  vars = c(1, 2),
  dots.per.axis = 100,
  bg.cex = .5,
  bg.alpha = .4,
  bg.pch = 15,
  par.reset = TRUE,
  theme = "white",
  col = c("#18A3AC", "#F48024"),
  contour.col = "black",
  contour.lwd = .1,
  point.pch = c(3, 4),
  point.alpha = 1
) {
  # [ Data ] ----
  data <- as.data.frame(data)
  class.dat <- data[, ncol(data)]
  data[, ncol(data)] <- NULL
  xdat <- data[vars[1]]
  ydat <- data[vars[2]]
  xlim <- range(xdat)
  ylim <- range(ydat)
  x <- seq(xlim[1], xlim[2], length.out = dots.per.axis)
  y <- seq(ylim[1], ylim[2], length.out = dots.per.axis)
  dat <- data.frame(expand.grid(x, y))
  names(dat) <- names(data)[vars]
  if (ncol(data) > 2) {
    col.means <- colMeans(data[, -vars, drop = FALSE])
    dat2 <- data.frame(matrix(
      rep(col.means, dots.per.axis^2),
      dots.per.axis^2,
      byrow = TRUE
    ))
    names(dat2) <- names(data[, -vars, drop = FALSE])
    dat <- cbind(dat, dat2)
  }

  predicted <- predict(rtmod, dat)

  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Plot ----
  # '- Background: decision surface ----
  mplot3_xy(
    dat[, 1],
    dat[, 2],
    group = predicted,
    xlab = names(data)[vars[1]],
    ylab = names(data)[vars[2]],
    point.cex = bg.cex,
    marker.alpha = bg.alpha,
    group.legend = FALSE,
    par.reset = FALSE,
    zerolines = FALSE,
    theme = theme,
    marker.col = col,
    xaxs = "i",
    yaxs = "i"
  )

  # '- Contour lines ----
  contour(
    x,
    y,
    matrix(as.integer(predicted) - 1, dots.per.axis),
    lwd = contour.lwd,
    col = contour.col,
    drawlabels = FALSE,
    add = TRUE
  )

  # '- Data points ----
  mplot3_xy(
    xdat[, 1],
    ydat[, 1],
    xlab = "",
    ylab = "",
    pch = point.pch,
    marker.alpha = point.alpha,
    group = class.dat,
    group.legend = FALSE,
    marker.col = col,
    theme = theme,
    xaxs = "i",
    yaxs = "i",
    zerolines = FALSE,
    axes.visible = FALSE,
    new = TRUE
  )

  invisible(predicted)
} # rtemis::mplot3_decision
