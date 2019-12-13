# mplot3.x.R
# ::rtemis::
# 2016-8 Efstathios D. Gennatas egenn.github.io

#' \code{mplot3}: Univariate plots: index, histogram, density, QQ-line
#'
#' Draw plots of 1-dimensional data: index, histogram, density, and Q-Q plots.
#'
#' You can group data either by supplying x as a list where each element contains one vector per group, or
#' as a data frame where each column represents group,
#' or by providing a \code{group} variable, which will be converted to factor.
#' For bivariate plots, see \link{mplot3.xy} and \link{mplot3.xym}. For heatmaps, see \link{mplot3.heatmap}
#' To plot histograms of multiple groups, it's best to use \code{hist.type = "lines"}, which will use \link{mhist}
#' and space apart the breaks for each group
#'
#' @inheritParams mplot3.xy
#' @param x Numeric vector or list of vectors, one for each group.
#'   If \code{data} is provided, x is name of variable in \code{data}
#' @param type Character: "density", "histogram",  "index", "ts", "qqline"
#'   Case-insensitive and supports partial matching: e.g. \code{mplot3.x(x, "H")} gives histogram
#' @param group Vector denoting group membership. Will be converted to factor.
#'   If \code{data} is provided, \code{group} is name of variable if \code{data}
#' @param data Optional data frame containing x data
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param index.ypad Float: Expand ylim by this much for plot type "index" Default = .1
#' (Stops points being cut off)
#' @param index.type Character: "p" for points (Default), "l" for lines (timeseries)
#' @param labs.col Color for labels
#' @param filename Path to file: If supplied, plot will be printed to file
#' @param lwd Integer: Line width. Used for \code{type = "ts" or "density"}
#' @param lab.adj Adjust the axes labels. 0 = left adjust; 1 = right adjust; .5 = center (Default)
#' @param hist.breaks See \code{histogram("breaks")}
#' @param hist.type Character: "bars" or "lines". Default = "bars"
#' @param hist.lwd Float: Line width for \code{type = "histogram"}; \code{hist.type = "lines"}
#' @param density.line Logical: If TRUE, draw line for \code{type = "density"}. Default = FALSE
#' @param density.shade Logical: If TRUE, draw shaded polygon for \code{type = "density"}. Default = TRUE
#' @param qqline.col Color for Q-Q line
#' @param qqline.alpha Float: Alpha for Q-Q line
#' @param density.mean Logical: If TRUE, print mean of \code{x} along plot. Default = TRUE, for \code{type = "density"}
#' @param na.rm Logical: Will be passed to all functions that support it. If set to FALSE,
#'   input containing NA values will result in error, depending on the \code{type}
#' @param group.legend Logical: If TRUE, include legend with group names
#' @param group.title Character: Title above group names
#' @seealso \link{mplot3}, \link{mplot3.xy}, \link{mplot3.xym}, \link{mplot3.heatmap}
#' @author Efstathios D. Gennatas
#' @export

mplot3.x <- function(x,
                     type = c("density", "histogram", "index", "ts", "qqline"),
                     group = NULL,
                     data = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     main = NULL,
                     main.col = NULL,
                     main.adj = 0,
                     main.line = .5,
                     main.font = 2,
                     main.family = "",
                     xlim = NULL,
                     ylim = NULL,
                     index.ypad = .1,
                     axes = TRUE,
                     axes.swap = FALSE,
                     axes.col = NULL,
                     tick.col = NULL,
                     # axes.equal = FALSE,
                     cex = 1.2,
                     col = NULL,
                     alpha = .75,
                     index.type = c("p", "l"),
                     hist.breaks = "Sturges",
                     hist.type = c("bars", "lines"),
                     hist.lwd = 3,
                     density.line = FALSE,
                     density.shade = TRUE,
                     density.legend.side = 3,
                     density.legend.adj = .98,
                     density.params = list(),
                     qqline.col = NULL,
                     qqline.alpha = .66,
                     # point.rad = 1,
                     # point.size = 1,
                     # point.inches = NULL,
                     pch = 16,
                     point.col = NULL,
                     point.cex = 1,
                     # point.fg.col = NULL,
                     point.bg.col = NULL,
                     point.alpha = .66,
                     hline = NULL,
                     vline = NULL,
                     diagonal = FALSE,
                     diagonal.col = NULL,
                     grid = FALSE,
                     grid.col = NULL,
                     grid.alpha = .5,
                     grid.lty = 3,
                     grid.lwd = 1,
                     bg = NULL,
                     plot.bg = NULL,
                     annotation = NULL,
                     annot.col = NULL,
                     group.legend = NULL,
                     group.title = "",
                     group.names = NULL,
                     group.side = 3,
                     group.adj = 0.02,
                     group.at = NA,
                     # legend.tl = NULL,
                     # legend.tl.col = "black",
                     # legend.tr = NULL,
                     # legend.tr.col = "black",
                     # legend.tc = NULL,
                     # legend.tc.col = "black",
                     text.xy = NULL,
                     text.x = NULL,
                     text.y = NULL,
                     text.xy.cex = 1,
                     text.xy.col = "white",
                     line.col = "#008E00", # for QQ-line
                     tck = .02, # R default is -.01
                     x.axis.padj = -1.1,
                     xlab.line = 1.3,
                     y.axis.padj = .9,
                     ylab.line = 1.6,
                     labs.col = NULL,
                     # pch = NULL,
                     bty = "o",
                     box = NULL,
                     box.col = NULL,
                     box.alpha = 1,
                     box.lty = 1,
                     box.lwd = 2,
                     lab.adj = .5,
                     density.mean = ifelse(type == "density", TRUE, FALSE),
                     # line.alpha = .7,
                     hline.col = "black",
                     hline.lwd = 1,
                     hline.lty = 1,
                     vline.col = "black",
                     vline.lwd = 1,
                     vline.lty = 1,
                     lty = 1,
                     lwd = 2,
                     qqline.lwd = lwd,
                     density.lwd = lwd,
                     theme = getOption("rt.theme", "lightgrid"),
                     palette = getOption("rt.palette", "rtCol1"),
                     zero.lines = NULL,
                     zero.col = NULL,
                     zero.alpha = .66,
                     zero.lty = 1,
                     zero.lwd = 1.5,
                     pty = "s",
                     mar = c(2.5, 2.5, 1.5, 1),
                     xaxs = "r",
                     yaxs = "r",
                     autolabel = letters,
                     new = FALSE,
                     alpha.off = FALSE,  # Turn off all alpha if it's not supported by the device
                     na.rm = TRUE,
                     par.reset = TRUE,
                     export.par.orig = FALSE,
                     filename = NULL,
                     pdf.width = 6,
                     pdf.height = 6,
                     ...) {

  # [ ARGUMENTS ] ====
  type <- match.arg(type)
  if (type == "ts") {
    type <- "index"
    index.type <- "l"
  }
  index.type <- match.arg(index.type)
  hist.type <- match.arg(hist.type)
  # if (is.null(yaxs)) yaxs <- if (type == "density") "i" else "r"

  # [ xlab & ylab ] ====
  if (is.list(x) & type == "density" & is.null(xlab)) xlab <- ""
  # if (is.null(xlab)) xlab <- deparse(substitute(x))
  xname <- deparse(substitute(x))

  # data
  if (!is.null(data)) {
    x <- data[[xname]]
    if (!is.null(group)) group <- data[[deparse(substitute(group))]]
  }

  if (alpha.off) alpha <- 1

  if (!is.null(filename)) if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE)
  if (is.character(palette)) palette <- rtPalette(palette)

  # [ DATA ] ====
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group)
    if (is.null(group.names)) group.names <- labelify(levels(group))
    names(x) <- group.names
  }

  # Convert data to lists
  # if (!is.list(x)) xl <- as.list(as.data.frame(x)) else xl <- x
  xl <- if (!is.list(x)) list(x) else x

  # Remove non-numeric vectors
  which.nonnum <- which(sapply(xl, function(i) !is.numeric(i)))
  if (length(which.nonnum) > 0) xl[[which.nonnum]] <- NULL

  if (type == "index") yl <- lapply(seq_along(xl), function(i) seq(1, length(xl[[i]])))
  if (type == "qqline" & length(xl) > 1) stop("Draw Q-Q plots one variable at a time")

  # Group names
  if (!is.null(group.names)) group.names <- c(group.title, group.names)
  if (is.null(group.names)) {
    if (!is.null(names(xl))) {
      group.names <- c(group.title, labelify(names(xl)))
    } else {
      group.names <- c(group.title, paste(" ", toupper(letters[seq_along(xl)])) )
    }
  }
  if (length(lty) != length(xl)) lty <- as.list(rep(lty, length(xl)/length(lty)))

  # If not defined, group legend defaults to T, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(length(xl) > 1, TRUE, FALSE)

  # [ COLORS ] ====
  if (is.null(col)) {
    if (type == "qqline" & length(xl) < 2) {
      col <- if (theme == "light" | theme == "lightgrid") "black" else "white"
    } else {
      # if (theme == "light" | theme == "lightgrid") {
      #   col <- paletteLight
      # } else {
      #   col <- paletteDark
      # }
      col <- palette
    }
  }
  marker.col <- lapply(col, function(cl) adjustcolor(cl, alpha.f = alpha))

  # [ THEMES ] ====
  if (theme == "lightbox") {
    theme <- "light"
    box <- TRUE
    if (is.null(zero.lines)) zero.lines <- FALSE
  }
  if (theme == "darkbox") {
    theme <- "dark"
    box <- TRUE
    if (is.null(zero.lines)) zero.lines <- FALSE
  }

  # if (length(theme) > 1) theme <- "light"
  if (theme == "lightgrid") {
    theme <- "light"
    if (is.null(plot.bg)) plot.bg <- "gray90"
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "white"
    grid.lty <- zero.lty <- 1
    grid.lwd <- zero.lwd <- 1.5
    if (is.null(tick.col)) tick.col <- grid.col
  }
  if (theme == "darkgrid") {
    theme <- "dark"
    if (is.null(plot.bg)) plot.bg <- "gray15"
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "black"
    grid.lty <- zero.lty <- 1
    grid.lwd <- zero.lwd <- 1.5
    if (is.null(tick.col)) tick.col <- grid.col
  }
  themes <- c("light", "dark", "box", "darkbox")
  if (!theme %in% themes) {
    warning(paste(theme, "is not an accepted option; defaulting to \"light\""))
    theme <- "light"
  }

  if (theme == "light") {
    if (is.null(bg)) bg <- "white"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("black", alpha.f = point.alpha))
    #   col <- list("gray30")
    # }
    # box.col <- "white"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray10"
    if (is.null(labs.col)) labs.col <- "gray10"
    if (is.null(main.col)) main.col <- "black"
    if (is.null(grid.col)) grid.col <- "black"
    if (is.null(diagonal.col)) diagonal.col <- "black"
    if (is.null(hline.col)) hline.col <- "black"
    # gen.col <- "black"
  } else if (theme == "dark") {
    if (is.null(bg)) bg <- "black"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("white", alpha.f = point.alpha))
    #   col <- list("gray70")
    # }
    # box.col <- "black"
    if (is.null(axes.col)) axes.col <- adjustcolor("black", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray90"
    if (is.null(labs.col)) labs.col <- "gray90"
    if (is.null(main.col)) main.col <- "white"
    if (is.null(grid.col)) grid.col <- "white"
    if (is.null(diagonal.col)) diagonal.col <- "white"
    if (is.null(hline.col)) hline.col <- "white"
    gen.col <- "white"
  } else if (theme == "box") {
    if (is.null(bg)) bg <- "white"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("black", alpha.f = point.alpha))
    #   col <- list("gray30")
    # }
    # if (is.null(box.col)) box.col <- "gray10"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray10"
    if (is.null(labs.col)) labs.col <- "gray10"
    if (is.null(main.col)) main.col <- "black"
    if (is.null(grid.col)) grid.col <- "black"
    if (is.null(diagonal.col)) diagonal.col <- "black"
    if (is.null(hline.col)) hline.col <- "black"
    gen.col <- "black"
  } else if (theme == "darkbox") {
    if (is.null(bg)) bg <- "black"
    # if (is.null(col) & length(xl) == 1) {
    #   # col <- as.list(adjustcolor("white", alpha.f = point.alpha))
    #   col <- list("gray70")
    # }
    # if (is.null(box.col)) box.col <- "gray90"
    if (is.null(axes.col)) axes.col <- adjustcolor("black", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray90"
    if (is.null(labs.col)) labs.col <- "gray90"
    if (is.null(main.col)) main.col <- "white"
    if (is.null(grid.col)) grid.col <- "white"
    if (is.null(diagonal.col)) diagonal.col <- "white"
    if (is.null(hline.col)) hline.col <- "white"
    gen.col <- "white"
  }

  # [ DATA: QQLINE ] ====
  if (type == "qqline") {
    yl <- list()
    for (i in seq_along(xl)) {
      x.qqnorm <- qqnorm(xl[[i]], plot.it = FALSE)
      # x <- xl
      xl[[i]] <- x.qqnorm$x
      yl[[i]] <- x.qqnorm$y
      xlab <- "Theoretical Quantiles"
      ylab <- "Sample Quantiles"
      if (is.null(main)) main <- "Q-Q Plot"
    }
  }

  # [ DATA: DENSITY ] ====
  if (type == "density") {
    if (is.null(ylab)) ylab <- "Density"
    densityl <- lapply(xl, function(j) do.call(density, c(list(x = j), density.params)))
    densityl <- lapply(densityl, function(d) data.frame(x = d$x, y = d$y))
    meanl <- lapply(xl, mean)
    sdl <- lapply(xl, sd)
    if (is.null(xlab)) xlab <- labelify(xname)
  }

  # [ DATA: HISTOGRAM ] ====
  if (type == "histogram") {
    histl <- lapply(xl, function(x) hist(x, breaks = hist.breaks, plot = FALSE))
  }

  # [ AXES LIMITS ] ====
  if (type == "histogram") {
    if (is.null(xlim)) xlim <- range(sapply(histl, function(x) c(x$breaks)))
    if (is.null(ylim)) ylim <- c(0, max(unlist(sapply(histl, function(x) c(x$counts)))))
  } else if (type == "density") {
    if (is.null(xlim)) xlim <- range(sapply(densityl, function(d) range(d$x)))
    if (is.null(ylim)) ylim <- c(0, max(sapply(densityl, function(d) max(d$y))))
  } else if (type == "qqline") {
    if (is.null(xlim)) xlim <- ylim <- range(c(xl, yl), na.rm = na.rm)
  } else if (type == "index") {
    if (is.null(xlim)) xlim <- c(1, max(sapply(xl, length), na.rm = na.rm))
    if (is.null(ylim)) ylim <- range(unlist(xl), na.rm = na.rm)
    ypadding <-  index.ypad * diff(ylim)
    # xpadding <- xpad * diff(xlim)
    ylim <- c(ylim[1] - ypadding, ylim[2] + ypadding)
    # xlim <- c(xlim[1] - xpadding, xlim[2] + xpadding)
  }

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) grDevices::pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  par(bg = bg, cex = cex, pty = pty, new = new, mar = mar) # tck = -.02
  if (!axes.swap) {
    plot(NULL, NULL, xlim = xlim, ylim = ylim, ann = FALSE, axes = FALSE, xaxs = xaxs, yaxs = yaxs)
  } else {
    plot(NULL, NULL, xlim = ylim, ylim = xlim, ann = FALSE, axes = FALSE, xaxs = xaxs, yaxs = yaxs)
  }

  # [ PLOT BG ] ====
  if (!is.null(plot.bg)) {
    x1 <- if (xaxs == "r") min(xlim) - .04 * diff(range(xlim)) else min(xlim)
    y1 <- if (yaxs == "r") min(ylim) - .04 * diff(range(ylim)) else min(ylim)
    x2 <- if (xaxs == "r") max(xlim) + .04 * diff(range(xlim)) else max(xlim)
    y2 <- if (yaxs == "r") max(ylim) + .04 * diff(range(ylim)) else max(ylim)
    if (!axes.swap) {
      rect(x1, y1, x2, y2, border = NA, col = plot.bg)
    } else {
      rect(y1, x1, y2, x2, border = NA, col = plot.bg)
    }
  }

  # [ GRID ] ====
  if (grid) grid(col = grid.col, lty = grid.lty, lwd = grid.lwd)

  # [ AXES ] ====
  if (axes) {
    if (type == "index") {
      if (!axes.swap) {
        if (is.null(ylab)) ylab <- labelify(xname)
        if (is.null(xlab)) xlab <- "Index"
      } else {
        if (is.null(xlab)) xlab <- labelify(xname)
        if (is.null(ylab)) ylab <- "Index"
      }
    }
    # if (type == "histogram") {
    #   if (!axes.swap) {
    #     ylab <- "Count"
    #   } else {
    #     ylab <- xlab
    #     xlab <- "Count"
    #   }
    # }
    axis(1, col = axes.col, col.axis = labs.col, col.ticks = tick.col, padj = x.axis.padj, tck = tck)
    axis(2, col = axes.col, col.axis = labs.col, col.ticks = tick.col, padj = y.axis.padj, tck = tck)
    mtext(xlab, side = 1, line = xlab.line, cex = cex, adj = lab.adj, col = labs.col)
    mtext(ylab, side = 2, line = ylab.line, cex = cex, adj = lab.adj, col = labs.col)
  }

  # [ ZERO LINES ] ====
  if (is.null(zero.lines)) {
    zero.lines <- if (type == "index" && (theme == "light" | theme == "dark")) TRUE else FALSE
  }
  if (zero.lines) {
    if (is.null(zero.col)) {
      if (theme == "light" | theme == "box") zero.col <- "black" else zero.col <- "white"
    }
    zero.col <- adjustcolor(zero.col, zero.alpha)
    if (ylim[1] <= 0 & 0 <= ylim[2]) abline(h = 0, lwd = zero.lwd, col = zero.col, lty = zero.lty)
    if (xlim[1] <= 0 & 0 <= xlim[2]) abline(v = 0, lwd = zero.lwd, col = zero.col, lty = zero.lty)
  }

  # [ BOX ] ====
  if (is.null(box)) {
    if (theme == "box" | theme == "darkbox") box <- TRUE else box <- FALSE
  }
  if (box) {
    if (is.null(box.col)) {
      if (theme == "light" | theme == "box") box.col <- "gray10" else box.col <- "gray90"
    }
    box.col <- adjustcolor(box.col, box.alpha)
    box(col = box.col, lty = box.lty, lwd = box.lwd, bty = bty)
  }

  # [ PLOT: INDEX ] ====
  if (type == "index") {
    if (length(index.type) < length(xl)) index.type <- rep(index.type, length(xl)/length(index.type))

    # point.rad
    #     if (!is.list(point.rad)) {
    #       point.rad <- lapply(1:length(xl), function(i) c(point.rad))
    #     }
    #     point.rad <- lapply(1:length(xl),
    #                         function(i) rep(point.rad[[i]], length(xl[[i]])/length(point.rad[[i]])))

    #     if (theme == "zero" | theme == "dark") {
    #       if (xlim[1] <= 0 & 0 <= xlim[2]) abline(v = 0, lwd = 2, col = "gray50", lty = 3)
    #       if (ylim[1] <= 0 & 0 <= ylim[2]) abline(h = 0, lwd = 2, col = "gray50", lty = 3)
    #   }
    for (i in seq_along(xl)) {
      # circles = rep(point.rad, length(xl[[i]])) # old
      #       symbols(yl[[i]], xl[[i]], circles = point.rad[[i]],
      #               inches = point.inches, fg = point.fg.col, bg = marker.col[[i]], add = T)
      if (!axes.swap) {
        points(yl[[i]], xl[[i]],
               type = index.type[[i]],
               col = marker.col[[i]],
               bg = point.bg.col,
               lwd = lwd,
               pch = pch,
               cex = point.cex)
        # ylab <- xlab
        # xlab <- "Index"
      } else {
        points(xl[[i]], yl[[i]],
               type = index.type[[i]],
               col = marker.col[[i]],
               bg = point.bg.col,
               lwd = lwd,
               pch = pch,
               cex = point.cex)
        # ylab <- "Index"
      }
    }

  }

  # [ PLOT: DENSITY ] ====
  if (type == "density") {
    # if (is.null(main)) main <- "Density"
    if (!axes.swap) {
      for (i in seq_along(xl)) {
        if (density.shade) {
          polygon(c(densityl[[i]]$x, rev(densityl[[i]]$x)), c(densityl[[i]]$y, rep(0, length(densityl[[i]]$y))),
                  col = marker.col[[i]], border = NA)
        }
        if (density.line) {
          lines(densityl[[i]]$x, densityl[[i]]$y, xlim = xlim, ylim = ylim, ann = FALSE,
                col = marker.col[[i]],
                lwd = density.lwd, type = "l", lty = lty[[i]])
        }
      }
      if (is.null(xlab)) xlab <- labelify(xname)
      ylab <- "Density"
    } else {
      for (i in seq_along(xl)) {
        if (density.shade) {
          polygon(c(densityl[[i]]$y, rev(densityl[[i]]$y)), c(densityl[[i]]$x, rep(0, length(densityl[[i]]$x))),
                  col = marker.col[[i]], border = NA)
        }
        if (density.line) {
          lines(densityl[[i]]$y, densityl[[i]]$x, xlim = ylim, ylim = xlim, ann = FALSE,
                col = marker.col[[i]],
                lwd = density.lwd, type = "l", lty = lty[[i]])
        }
      }
      xlab <- "Density"
    }

  } # type == "density"

  # [ PLOT: HISTOGRAM ] ====

  # # Stagger hist breaks for more than one group
  # if (is.null(hist.breaks)) {
  #   if (length(xl) == 1) {
  #     breaks <- "Sturges"
  #   } else {
  #     breaks <- 10
  #   }
  # }

  if (type == "histogram") {
    if (length(xl) > 1) {
      breaks <- hist(unlist(xl), breaks = hist.breaks, plot = FALSE)$breaks
      dist <- diff(breaks)[1] # mean(diff(breaks))
      breaksl <- lapply(seq_along(xl), function(i) {
        c(breaks - ((i - 1)/length(xl) * dist), max(breaks) - ((i - 1)/length(xl) * dist) + dist)
      })
    } else {
      # breaksl <- list(hist(xl[[1]], breaks = hist.breaks, plot = FALSE)$breaks)
      breaksl <- list(histl[[1]]$breaks)
    }
  }

  if (type == "histogram") {
    # if (is.null(main)) main <- "Histogram"
    if (hist.type == "bars") {
      for (i in seq_along(xl)) {
        hist(xl[[i]], breaks = hist.breaks, col = marker.col[[i]], add = TRUE, border = bg, xlim = xlim)
      }
    } else {
      for (i in seq_along(xl)) {
        mhist(xl[[i]], measure = "count", breaks = breaksl[[i]], col = marker.col[[i]], add = TRUE,
              lwd = hist.lwd,
              xlim = xlim, ylim = ylim, plot.axes = FALSE, xaxis = FALSE, yaxis = FALSE, xlab = "", ylab = "")
      }
    }
  }

  # [ MAIN ] ====
  if (exists("autolabel", envir = rtenv)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (!is.null(main)) {
    mtext(main, line = main.line, font = main.font, family = main.family,
          adj = main.adj, cex = cex, col = main.col)
  }

  # [ GROUP LEGEND ] ====
  if (group.legend) {

    # Regular
    mtext(group.names,
          col = c(main.col, unlist(col[seq_along(xl)])),
          side = group.side,
          adj = group.adj,
          at = group.at,
          cex = cex,
          padj = seq(2, 2 + 1.5 * length(xl), 1.5))
  }

  # [ ANNOTATION ] ====
  if (!is.null(annotation)) {
    if (is.null(annot.col)) annot.col = col[[1]]
    mtext(annotation, 1, -1.5, adj = .97, cex = cex, col = annot.col)
  }

  # [ QQ LINE ] ====
  if (is.null(qqline.col)) {
    if (length(xl) == 1) {
      qqline.col <- "#045ea7"
    } else {
      qqline.col <- pennPalette
    }
  }
  qqline.col <- lapply(qqline.col, function(x) adjustcolor(qqline.col, qqline.alpha))
  if (length(qqline.col) < length(xl)) qqline.col <- rep(qqline.col, length(xl) / length(qqline.col))

  if (type == "qqline") {
    for (i in seq_along(xl)) {
      #       symbols(xl[[i]], yl[[i]], circles = rep(point.rad, length(xl[[i]])),
      #               inches = point.inches, fg = point.fg.col, bg = marker.col[[i]], add = T)
      points(xl[[i]], yl[[i]],
             type = "p",
             col = marker.col[[i]],
             bg = point.bg.col,
             lwd = lwd,
             pch = pch,
             cex = point.cex)
    }

    for (i in seq_along(xl)) {
      qqline(xl[[i]], col = qqline.col[[i]], lwd = qqline.lwd)
    }
  }

  # [ DENSITY PLOT MEAN X ] ====
  # if (type == "density" & density.mean & is.null(legend.tr)) {
  #   mtext(c("Mean", lapply(meanl, ddSci)), col = c(main.col, unlist(col[1:length(xl)])),
  #         side = 3, adj = .98, cex = cex,
  #         padj = seq(2, 2 + 1.5 * length(xl), 1.5))
  # }
  if (type == "density" & density.mean) {
    mtext(c("Mean (SD)", lapply(seq(xl),
                                function(j) paste0(ddSci(meanl[[j]]), " (", ddSci(sdl[[j]]), ")"))),
          col = c(main.col, unlist(col[seq_along(xl)])),
          side = density.legend.side, adj = density.legend.adj, cex = cex,
          padj = seq(2, 2 + 1.5 * length(xl), 1.5))
  }

  # [ MISC LEGENDS ] ====
  # if (!is.null(legend.tl)) {
  #   mtext(legend.tl,
  #         col = legend.tl.col,
  #         side = 3, adj = 0.02, cex = cex,
  #         padj = seq(2, 2 + 1.5 * (length(legend.tl) - 1), 1.5) )
  # }
  #
  # if (!is.null(legend.tr)) {
  #   mtext(legend.tr,
  #         col = legend.tr.col,
  #         side = 3, adj = 0.98, cex = cex,
  #         padj = 2)
  # }
  #
  # if (!is.null(legend.tc)) {
  #   mtext(legend.tc,
  #         col = legend.tc.col,
  #         side = 3, adj = 0.5, cex = cex,
  #         padj = 2)
  # }

  if (!is.null(text.xy)) {
    if (is.null(text.x)) text.x <- mean(xlim)
    if (is.null(text.y)) text.y <- mean(ylim)
    text(x = text.x, y = text.y, labels = text.xy, cex = text.xy.cex, col = text.xy.col)
  }

  # [ HLINE & VLINE ] ====
  if (!is.null(hline)) abline(h = hline, lwd = hline.lwd, col = hline.col, lty = hline.lty)
  if (!is.null(vline)) abline(v = vline, lwd = vline.lwd, col = vline.col, lty = vline.lty)

  # # [ Autolabel ] ====
  # if (exists("autolabel", envir = rtenv)) {
  #   mtext(letters[rtenv$autolabel], line = autolabel.line, adj = 1, col = autolabel.col, cex = cex)
  #   rtenv$autolabel <- rtenv$autolabel + 1
  # }

  # [ OUTRO ] ====
  if (!is.null(filename)) grDevices::dev.off()
  if (export.par.orig) invisible(par.orig)

} # rtemis::mplot3.x
