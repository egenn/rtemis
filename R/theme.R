# theme.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Themes for \code{mplot3} and \code{dplot3} functions
#'
#' @param bg Color: Figure background
#' @param plot.bg Color: Plot region background
#' @param fg Color: Foreground color used as default for multiple elements like axes and labels, which can
#' be defined separately
#' @param pch Integer: Point character. Default = 16
#' @param cex Float: Character expansion factor. Default = 1.2
#' @param lwd Float: Line width. Default = 2
#' @param bty Character: Box type:  "o", "l", "7", "c", "u", or "]", or "n". Default = "n" (no box)
#' @param box.col Box color if \code{bty != "n"}
#' @param box.alpha Float: Box alpha
#' @param box.lty Integer: Box line type
#' @param box.lwd Float: Box line width
#' @param grid Logical: If TRUE, draw grid in plot regions
#' @param grid.nx Integer: N of vertical grid lines
#' @param grid.ny Integer: N of horizontal grid lines
#' @param grid.col Grid color
#' @param grid.alpha Float: Grid alpha
#' @param grid.lty Integer: Grid line type
#' @param grid.lwd Float: Grid line width
#' @param axes Logical: If TRUE, draw axes
#' @param axes.col Axes colors
#' @param tick.col Tick color
#' @param tick.alpha Float: Tick alpha
#' @param tick.labels.col Tick labels' color
#' @param x.axis.side Integer: Side to place x-axis. Default = 1 (bottom)
#' @param y.axis.side Integer: Side to place y-axis. Default = 2 (left)
#' @param labs.col Labels' color
#' @param zerolines Logical: If TRUE, draw lines on x = 0, y = 0, if within plot limits
#' @param zerolines.col Zerolines color
#' @param zerolines.alpha Float: Zerolines alpha
#' @param zerolines.lty Integer: Zerolines line type
#' @param zerolines.lwd Float: Zerolines line width
#' @param main.line Float: How many lines away from the plot region to draw title. Default = .5
#' @param main.adj Float: How to align title. Default = 0 (left-align)
#' @param main.font Integer: 1: Regular, 2: Bold
#' @param main.col Title color
#' @param font.family Character: Font to be used throughout plot. Must be available to the OS
#'
#' @rdname theme
#' @export
theme_blackgrid <- function(bg = "#000000",
                            plot.bg = NA,
                            fg = "#ffffff",
                            pch = 16,
                            cex = 1.1,
                            lwd = 2,
                            # box --
                            bty = "n",
                            box.col = fg,
                            box.alpha = 1,
                            box.lty = 1,
                            box.lwd = .5,
                            # grid --
                            grid = TRUE,
                            grid.nx = NULL,
                            grid.ny = NULL,
                            grid.col = fg,
                            grid.alpha = .2,
                            grid.lty = 1,
                            grid.lwd = 1,
                            # axes --
                            axes.visible = TRUE,
                            axes.col = NA,
                            tick.col = fg,
                            tick.alpha = 1,
                            tick.labels.col = fg,
                            tck = -0.01,
                            tcl = NA,
                            x.axis.side = 1,
                            y.axis.side = 2,
                            labs.col = fg,
                            x.axis.line = 0,
                            x.axis.las = 0,
                            x.axis.padj = -1.1,
                            x.axis.hadj = .5,
                            y.axis.line = 0,
                            y.axis.las = 0,
                            y.axis.padj = 1,
                            y.axis.hadj = .5,
                            xlab.line = 1.4,
                            ylab.line = 2,
                            # zerolines --
                            zerolines = TRUE,
                            zerolines.col = fg,
                            zerolines.alpha = .5,
                            zerolines.lty = 1,
                            zerolines.lwd = 1,
                            # title --
                            main.line = .25,
                            main.adj = 0,
                            main.font = 2,
                            main.col = fg,
                            font.family = "Helvetica") {

  list(name = "blackgrid",
       bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_blackgrid


#' @rdname theme
#' @export
theme_darkgrid <- function(name = "darkgrid",
                           bg = "#000000",
                           plot.bg = "#1A1A1A",
                           fg = "#ffffff",
                           pch = 16,
                           cex = 1.1,
                           lwd = 2,
                           # box --
                           bty = "n",
                           box.col = fg,
                           box.alpha = 1,
                           box.lty = 1,
                           box.lwd = .5,
                           # grid --
                           grid = TRUE,
                           grid.nx = NULL,
                           grid.ny = NULL,
                           grid.col = bg,
                           grid.alpha = 1,
                           grid.lty = 1,
                           grid.lwd = 1,
                           # axes --
                           axes.visible = TRUE,
                           axes.col = NA,
                           tick.col = fg,
                           tick.alpha = 1,
                           tick.labels.col = fg,
                           tck = -0.01,
                           tcl = NA,
                           x.axis.side = 1,
                           y.axis.side = 2,
                           labs.col = fg,
                           x.axis.line = 0,
                           x.axis.las = 0,
                           x.axis.padj = -1.1,
                           x.axis.hadj = .5,
                           y.axis.line = 0,
                           y.axis.las = 0,
                           y.axis.padj = 1,
                           y.axis.hadj = .5,
                           xlab.line = 1.4,
                           ylab.line = 2,
                           # zerolines --
                           zerolines = TRUE,
                           zerolines.col = fg,
                           zerolines.alpha = .5,
                           zerolines.lty = 1,
                           zerolines.lwd = 1,
                           # title --
                           main.line = .25,
                           main.adj = 0,
                           main.font = 2,
                           main.col = fg,
                           font.family = "Helvetica") {

  list(bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_darkgrid

#' @rdname theme
#' @export
theme_darkgraygrid <- function(name = "darkgraygrid",
                               bg = "#121212", # #202020
                               plot.bg = "#202020", # #121212
                               fg = "#ffffff",
                               pch = 16,
                               cex = 1.1,
                               lwd = 2,
                               # box --
                               bty = "n",
                               box.col = fg,
                               box.alpha = 1,
                               box.lty = 1,
                               box.lwd = .5,
                               # grid --
                               grid = TRUE,
                               grid.nx = NULL,
                               grid.ny = NULL,
                               grid.col = bg,
                               grid.alpha = 1,
                               grid.lty = 1,
                               grid.lwd = 1,
                               # axes --
                               axes.visible = TRUE,
                               axes.col = NA,
                               tick.col = fg,
                               tick.alpha = 1,
                               tick.labels.col = fg,
                               tck = -0.01,
                               tcl = NA,
                               x.axis.side = 1,
                               y.axis.side = 2,
                               labs.col = fg,
                               x.axis.line = 0,
                               x.axis.las = 0,
                               x.axis.padj = -1.1,
                               x.axis.hadj = .5,
                               y.axis.line = 0,
                               y.axis.las = 0,
                               y.axis.padj = 1,
                               y.axis.hadj = .5,
                               xlab.line = 1.4,
                               ylab.line = 2,
                               # zerolines --
                               zerolines = TRUE,
                               zerolines.col = fg,
                               zerolines.alpha = .5,
                               zerolines.lty = 1,
                               zerolines.lwd = 1,
                               # title --
                               main.line = .25,
                               main.adj = 0,
                               main.font = 2,
                               main.col = fg,
                               font.family = "Helvetica") {

  list(bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_darkgraygrid

#' @rdname theme
#' @export
theme_black <- function(name = "black",
                        bg = "#000000",
                        plot.bg = NA,
                        fg = "#ffffff",
                        pch = 16,
                        cex = 1.1,
                        lwd = 2,
                        # box --
                        bty = "n",
                        box.col = fg,
                        box.alpha = 1,
                        box.lty = 1,
                        box.lwd = .5,
                        # grid --
                        grid = FALSE,
                        grid.nx = NULL,
                        grid.ny = NULL,
                        grid.col = fg,
                        grid.alpha = .2,
                        grid.lty = 1,
                        grid.lwd = 1,
                        # axes --
                        axes.visible = TRUE,
                        axes.col = NA,
                        tick.col = fg,
                        tick.alpha = .5,
                        tick.labels.col = fg,
                        tck = -0.01,
                        tcl = NA,
                        x.axis.side = 1,
                        y.axis.side = 2,
                        labs.col = fg,
                        x.axis.line = 0,
                        x.axis.las = 0,
                        x.axis.padj = -1.1,
                        x.axis.hadj = .5,
                        y.axis.line = 0,
                        y.axis.las = 0,
                        y.axis.padj = 1,
                        y.axis.hadj = .5,
                        xlab.line = 1.4,
                        ylab.line = 2,
                        # zerolines --
                        zerolines = TRUE,
                        zerolines.col = fg,
                        zerolines.alpha = .5,
                        zerolines.lty = 1,
                        zerolines.lwd = 1,
                        # title --
                        main.line = .25,
                        main.adj = 0,
                        main.font = 2,
                        main.col = fg,
                        font.family = "Helvetica") {

  list(bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_black

#' @rdname theme
#' @export
theme_whitegrid <- function(name = "whitegrid",
                            bg = "#ffffff",
                            plot.bg = NA,
                            fg = "#000000",
                            pch = 16,
                            cex = 1.1,
                            lwd = 2,
                            # box --
                            bty = "n",
                            box.col = fg,
                            box.alpha = 1,
                            box.lty = 1,
                            box.lwd = .5,
                            # grid --
                            grid = TRUE,
                            grid.nx = NULL,
                            grid.ny = NULL,
                            grid.col = fg,
                            grid.alpha = .2,
                            grid.lty = 1,
                            grid.lwd = 1,
                            # axes --
                            axes.visible = TRUE,
                            axes.col = NA,
                            tick.col = fg,
                            tick.alpha = 1,
                            tick.labels.col = fg,
                            tck = -0.01,
                            tcl = NA,
                            x.axis.side = 1,
                            y.axis.side = 2,
                            labs.col = fg,
                            x.axis.line = 0,
                            x.axis.las = 0,
                            x.axis.padj = -1.1,
                            x.axis.hadj = .5,
                            y.axis.line = 0,
                            y.axis.las = 0,
                            y.axis.padj = 1,
                            y.axis.hadj = .5,
                            xlab.line = 1.4,
                            ylab.line = 2,
                            # zerolines --
                            zerolines = TRUE,
                            zerolines.col = fg,
                            zerolines.alpha = .5,
                            zerolines.lty = 1,
                            zerolines.lwd = 1,
                            # title --
                            main.line = .25,
                            main.adj = 0,
                            main.font = 2,
                            main.col = fg,
                            font.family = "Helvetica") {

  list(bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_whitegrid

#' @rdname theme
#' @export
theme_lightgrid <- function(name = "lightgrid",
                            bg = "#ffffff",
                            plot.bg = "#E6E6E6",
                            fg = "#000000",
                            pch = 16,
                            cex = 1.1,
                            lwd = 2,
                            # box --
                            bty = "n",
                            box.col = fg,
                            box.alpha = 1,
                            box.lty = 1,
                            box.lwd = .5,
                            # grid --
                            grid = TRUE,
                            grid.nx = NULL,
                            grid.ny = NULL,
                            grid.col = bg,
                            grid.alpha = 1,
                            grid.lty = 1,
                            grid.lwd = 1,
                            # axes --
                            axes.visible = TRUE,
                            axes.col = NA,
                            tick.col = fg,
                            tick.alpha = 1,
                            tick.labels.col = fg,
                            tck = -0.01,
                            tcl = NA,
                            x.axis.side = 1,
                            y.axis.side = 2,
                            labs.col = fg,
                            x.axis.line = 0,
                            x.axis.las = 0,
                            x.axis.padj = -1.1,
                            x.axis.hadj = .5,
                            y.axis.line = 0,
                            y.axis.las = 0,
                            y.axis.padj = 1,
                            y.axis.hadj = .5,
                            xlab.line = 1.4,
                            ylab.line = 2,
                            # zerolines --
                            zerolines = TRUE,
                            zerolines.col = fg,
                            zerolines.alpha = .5,
                            zerolines.lty = 1,
                            zerolines.lwd = 1,
                            # title --
                            main.line = .25,
                            main.adj = 0,
                            main.font = 2,
                            main.col = fg,
                            font.family = "Helvetica") {

  list(bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_darkgrid

#' @rdname theme
#' @export
theme_white <- function(name = "white",
                        bg = "#ffffff",
                        plot.bg = NA,
                        fg = "#000000",
                        pch = 16,
                        cex = 1.1,
                        lwd = 2,
                        # box --
                        bty = "n",
                        box.col = fg,
                        box.alpha = 1,
                        box.lty = 1,
                        box.lwd = .5,
                        # grid --
                        grid = FALSE,
                        grid.nx = NULL,
                        grid.ny = NULL,
                        grid.col = fg,
                        grid.alpha = .2,
                        grid.lty = 1,
                        grid.lwd = 1,
                        # axes --
                        axes.visible = TRUE,
                        axes.col = NA,
                        tick.col = fg,
                        tick.alpha = .5,
                        tick.labels.col = fg,
                        tck = -0.01,
                        tcl = NA,
                        x.axis.side = 1,
                        y.axis.side = 2,
                        labs.col = fg,
                        x.axis.line = 0,
                        x.axis.las = 0,
                        x.axis.padj = -1.1,
                        x.axis.hadj = .5,
                        y.axis.line = 0,
                        y.axis.las = 0,
                        y.axis.padj = 1,
                        y.axis.hadj = .5,
                        xlab.line = 1.4,
                        ylab.line = 2,
                        # zerolines --
                        zerolines = TRUE,
                        zerolines.col = fg,
                        zerolines.alpha = .5,
                        zerolines.lty = 1,
                        zerolines.lwd = 1,
                        # title --
                        main.line = .25,
                        main.adj = 0,
                        main.font = 2,
                        main.col = fg,
                        font.family = "Helvetica") {

  list(bg = bg,
       plot.bg = plot.bg,
       fg = fg,
       pch = pch,
       cex = cex,
       lwd = lwd,
       # box --
       bty = bty,
       box.col = box.col,
       box.alpha = box.alpha,
       box.lty = box.lty,
       box.lwd = box.lwd,
       # grid --
       grid = grid,
       grid.nx = grid.nx,
       grid.ny = grid.ny,
       grid.col = grid.col,
       grid.alpha = grid.alpha,
       grid.lty = grid.lty,
       grid.lwd = grid.lwd,
       # axes --
       axes.visible = axes.visible,
       axes.col = axes.col,
       tick.col = tick.col,
       tick.alpha = tick.alpha,
       tick.labels.col = tick.labels.col,
       tck = tck,
       tcl = tcl,
       x.axis.side = x.axis.side,
       y.axis.side = y.axis.side,
       labs.col = labs.col,
       x.axis.line = x.axis.line,
       x.axis.las = x.axis.las,
       x.axis.padj = x.axis.padj,
       x.axis.hadj = x.axis.hadj,
       y.axis.line = y.axis.line,
       y.axis.las = y.axis.las,
       y.axis.padj = y.axis.padj,
       y.axis.hadj = y.axis.hadj,
       xlab.line = xlab.line,
       ylab.line = ylab.line,
       # zerolines --
       zerolines = zerolines,
       zerolines.col = zerolines.col,
       zerolines.alpha = zerolines.alpha,
       zerolines.lty = zerolines.lty,
       zerolines.lwd = zerolines.lwd,
       # title --
       main.line = main.line,
       main.adj = main.adj,
       main.font = main.font,
       main.col = main.col,
       font.family = font.family)

} # rtemis::theme_white
