# mplot3.x.R
# ::rtemis::
# 2016- E.D. Gennatas lambdamd.org

#' \code{mplot3}: Univariate plots: index, histogram, density, QQ-line
#'
#' Draw plots of 1-dimensional data: index, histogram, density, and Q-Q plots.
#'
#' You can group data either by supplying x as a list where each element contains one vector per
#' group, or as a data frame where each column represents group,
#' or by providing a \code{group} variable, which will be converted to factor.
#' For bivariate plots, see \link{mplot3.xy} and \link{mplot3.xym}. For heatmaps, see
#' \link{mplot3.heatmap}
#' To plot histograms of multiple groups, it's best to use \code{hist.type = "lines"}, which will
#' use \link{mhist}
#' and space apart the breaks for each group
#'
#' @inheritParams mplot3.xy
#' @param x Numeric vector or list of vectors, one for each group.
#'   If \code{data} is provided, x is name of variable in \code{data}
#' @param type Character: "density", "histogram", "hd" (histogram bars & density lines),
#' "lhist" (line histogram like \link{mhist}; same as \code{type = "hist", hist.type = "lines"}),
#' "index", "ts", "qqline"
#' Case-insensitive and supports partial matching: e.g. \code{mplot3.x(x, "H")} gives histogram
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
#' @param density.shade Logical: If TRUE, draw shaded polygon for \code{type = "density"}.
#' Default = TRUE
#' @param qqline.col Color for Q-Q line
#' @param qqline.alpha Float: Alpha for Q-Q line
#' @param density.avg Logical: If TRUE, print mean of \code{x} along plot. Default = TRUE,
#' for \code{type = "density"}
#' @param density.avg.fn Character: "median" or "mean". Function to use if
#' \code{density.avg = TRUE}. Default = "median"
#' @param density.avg.line Logical: If TRUE, draw vertical lines at the density average x-value
#' @param density.avg.lwd Float: Line width for \code{density.avg.line}. Default = 1.5
#' @param density.avg.lty Integer: Line type for \code{density.avg.line}. Default = 3
#' @param na.rm Logical: Will be passed to all functions that support it. If set to FALSE,
#'   input containing NA values will result in error, depending on the \code{type}
#' @param group.legend Logical: If TRUE, include legend with group names
#' @param group.title Character: Title above group names
#' @return Invisibly returns the output of \code{density}, \code{hist}, \code{qqnorm}, or NULL
#' @seealso \link{mplot3.xy}, \link{mplot3.xym}, \link{mplot3.heatmap}
#' @author E.D. Gennatas
#' @export

mplot3.x <- function(x,
                     type = c("density", "histogram", "hd", "lhist", "index", "ts", "qqline"),
                     group = NULL,
                     data = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     main = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     index.ypad = .1,
                     axes.swap = FALSE,
                     axes.col = NULL,
                     tick.col = NULL,
                     cex = 1.2,
                     col = NULL,
                     alpha = .75,
                     index.type = c("p", "l"),
                     hist.breaks = "Sturges",
                     hist.type = c("bars", "lines"),
                     hist.probability = FALSE,
                     hist.lwd = 3,
                     density.line = FALSE,
                     density.shade = TRUE,
                     density.legend.side = 3,
                     density.legend.adj = .98,
                     density.bw = "nrd0",
                     density.kernel = "gaussian",
                     density.params = list(na.rm = na.rm),
                     qqline.col = "#18A3AC",
                     qqline.alpha = 1,
                     pch = 16,
                     point.col = NULL,
                     point.cex = 1,
                     point.bg.col = NULL,
                     point.alpha = .66,
                     hline = NULL,
                     vline = NULL,
                     diagonal = FALSE,
                     grid = FALSE,
                     grid.col = NULL,
                     grid.alpha = .5,
                     grid.lty = 3,
                     grid.lwd = 1,
                     annotation = NULL,
                     annot.col = NULL,
                     group.legend = NULL,
                     group.title = "",
                     group.names = NULL,
                     group.side = 3,
                     group.adj = 0.02,
                     group.at = NA,
                     text.xy = NULL,
                     text.x = NULL,
                     text.y = NULL,
                     text.xy.cex = 1,
                     text.xy.col = "white",
                     line.col = "#008E00", # for QQ-line or dh density line
                     x.axis.padj = -1.1,
                     # xlab.line = 1.3,
                     y.axis.padj = .9,
                     # ylab.line = 1.6,
                     labs.col = NULL,
                     lab.adj = .5,
                     density.avg = ifelse(type == "density", TRUE, FALSE),
                     density.avg.fn = c("median", "mean"),
                     density.avg.line = FALSE,
                     density.avg.lwd = 1.5,
                     density.avg.lty = 3,
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
                     pty = "m",
                     mar = NULL,
                     oma = rep(0, 4),
                     xaxs = "r",
                     yaxs = "r",
                     autolabel = letters,
                     new = FALSE,
                     alpha.off = FALSE,  # Turn off all alpha if it's not supported by the device
                     na.rm = TRUE,
                     par.reset = TRUE,
                     filename = NULL,
                     pdf.width = 6,
                     pdf.height = 6,
                     ...) {

  # [ Arguments ] ====
  type <- match.arg(type)
  if (type == "ts") {
    type <- "index"
    index.type <- "l"
  }
  if (type == "lhist") {
    type <- "histogram"
    hist.type <- "lines"
  }
  index.type <- match.arg(index.type)
  hist.type <- match.arg(hist.type)
  density.avg.fn <- match.arg(density.avg.fn)
  if (is.null(mar)) mar <- c(2.5, 3, 2, 1)

  # [ Theme ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # [ xlab & ylab ] ====
  if (is.list(x) && type == "density" && is.null(xlab)) xlab <- ""
  xname <- deparse(substitute(x))

  # data
  if (!is.null(data)) {
    x <- data[[xname]]
    if (!is.null(group)) group <- data[[deparse(substitute(group))]]
  }

  if (alpha.off) alpha <- 1

  if (!is.null(filename)) if (!dir.exists(dirname(filename))) dir.create(dirname(filename),
                                                                         recursive = TRUE)
  if (is.character(palette)) palette <- rtPalette(palette)

  # [ Data ] ====
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group)
    if (is.null(group.names)) group.names <- labelify(levels(group))
    names(x) <- group.names
  }

  # Convert data to lists
  xl <- if (!is.list(x)) list(x) else x

  # Remove non-numeric vectors
  which.nonnum <- which(sapply(xl, function(i) !is.numeric(i)))
  if (length(which.nonnum) > 0) xl[[which.nonnum]] <- NULL

  if (type == "index") yl <- lapply(seq_along(xl), function(i) seq(1, length(xl[[i]])))
  # if (type == "qqline" & length(xl) > 1) stop("Draw Q-Q plots one variable at a time")

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
      col <- theme$fg
    } else {
      col <- palette
    }
  }
  col.alpha <- lapply(col, function(cl) adjustcolor(cl, alpha.f = alpha))

  .out <- NULL

  # [ Data: QQLINE ] ====
  if (type == "qqline") {
    qyl <- qxl <- xl
    for (i in seq_along(xl)) {
      .out <- x.qqnorm <- qqnorm(xl[[i]], plot.it = FALSE)
      qxl[[i]] <- x.qqnorm$x
      qyl[[i]] <- x.qqnorm$y
      xlab <- "Theoretical Quantiles"
      ylab <- "Sample Quantiles"
      if (is.null(main)) main <- "Q-Q Plot"
    }
  }

  # [ Data: DENSITY ] ====
  if (type == "density") {
    if (is.null(ylab)) ylab <- "Density"
    density.params$bw <- density.bw
    density.params$kernel <- density.kernel
    .out <- lapply(xl, function(j) do.call(density, c(list(x = j), density.params)))
    densityl <- lapply(.out, function(d) data.frame(x = d$x, y = d$y))
    if (is.null(xlab)) xlab <- labelify(xname)
  }

  # [ Data: HISTOGRAM ] ====
  if (type == "histogram") {
    .out <- histl <- lapply(xl, function(x) hist(x, breaks = hist.breaks,
                                                 # probability = hist.probability,
                                                 plot = FALSE))
    if (is.null(xlab)) xlab <- labelify(xname)
  }

  # [ Data: DH ] ====
  if (type %in% c("hd", "density")) {
    histl = lapply(xl, function(x) hist(x, breaks = hist.breaks,
                                        # probability = hist.probability,
                                        plot = FALSE))
    .out <- list(densityl = lapply(xl, function(j) do.call(density, c(list(x = j), density.params))),
                 histl = histl)
    densityl = lapply(.out$densityl, function(d) data.frame(x = d$x, y = d$y))
  }

  # [ AXES LIMITS ] ====
  if (type == "histogram") {
    if (is.null(xlim)) xlim <- range(sapply(histl, function(x) c(x$breaks)))
    if (is.null(ylim)) ylim <- c(0, max(unlist(sapply(histl, function(x) c(x$counts)))))
  } else if (type == "density") {
    if (is.null(xlim)) xlim <- range(sapply(densityl, function(d) range(d$x)))
    if (is.null(ylim)) ylim <- c(0, max(sapply(densityl, function(d) max(d$y))))
  } else if (type == "hd") {
    if (is.null(xlim)) xlim <- range(sapply(histl, function(x) c(x$breaks)))
    if (is.null(ylim)) ylim <- c(0, max(max(sapply(densityl, function(d) max(d$y))),
                                        unlist(sapply(histl, function(x) c(x$density)))))
  } else if (type == "qqline") {
    if (is.null(xlim)) xlim <- range(qxl, na.rm = na.rm)
    if (is.null(ylim)) ylim <- range(qyl, na.rm = na.rm)
  } else if (type == "index") {
    if (is.null(xlim)) xlim <- c(1, max(sapply(xl, length), na.rm = na.rm))
    if (is.null(ylim)) ylim <- range(unlist(xl), na.rm = na.rm)
    ypadding <-  index.ypad * diff(ylim)
    # xpadding <- xpad * diff(xlim)
    ylim <- c(ylim[1] - ypadding, ylim[2] + ypadding)
  }

  # [ Plot ] ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height,
                              title = "rtemis Graphics")
  par.orig <- par(no.readonly = TRUE)
  if (!is.null(rtenv$rtpar)) {
    par.reset <- FALSE
    par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex, new = new)
  } else {
    par(mar = mar, oma = oma, bg = theme$bg, pty = pty, cex = theme$cex, new = new)
  }
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  if (!axes.swap) {
    plot(NULL, NULL, xlim = xlim, ylim = ylim, ann = FALSE, axes = FALSE, xaxs = xaxs, yaxs = yaxs)
  } else {
    plot(NULL, NULL, xlim = ylim, ylim = xlim, ann = FALSE, axes = FALSE, xaxs = xaxs, yaxs = yaxs)
  }

  # [ Plot bg ] ====
  if (!is.na(theme$plot.bg)) {
    x1 <- if (xaxs == "r") min(xlim) - .04 * diff(range(xlim)) else min(xlim)
    y1 <- if (yaxs == "r") min(ylim) - .04 * diff(range(ylim)) else min(ylim)
    x2 <- if (xaxs == "r") max(xlim) + .04 * diff(range(xlim)) else max(xlim)
    y2 <- if (yaxs == "r") max(ylim) + .04 * diff(range(ylim)) else max(ylim)
    if (!axes.swap) {
      rect(x1, y1, x2, y2, border = NA, col = theme$plot.bg)
    } else {
      rect(y1, x1, y2, x2, border = NA, col = theme$plot.bg)
    }
  }

  # [ GRID ] ====
  if (theme$grid) {
    grid(nx = theme$grid.nx,
         ny = theme$grid.ny,
         col = colorAdjust(theme$grid.col, theme$grid.alpha),
         lty = theme$grid.lty,
         lwd = theme$grid.lwd)
  }

  # [ AXES ] ====
  if (theme$axes.visible) {
    if (type == "index") {
      if (!axes.swap) {
        if (is.null(ylab)) ylab <- labelify(xname)
        if (is.null(xlab)) xlab <- "Index"
      } else {
        if (is.null(xlab)) xlab <- labelify(xname)
        if (is.null(ylab)) ylab <- "Index"
      }
    }

    axis(side = theme$x.axis.side,
         line = theme$x.axis.line,
         las = theme$x.axis.las,
         padj = theme$x.axis.padj,
         hadj = theme$x.axis.hadj,
         col = theme$axes.col,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col.axis = theme$tick.labels.col,
         tck = theme$tck,
         tcl = theme$tcl,
         cex = theme$cex,
         family = theme$font.family)
    axis(side = theme$y.axis.side,
         line = theme$y.axis.line,
         las = theme$y.axis.las,
         padj = theme$y.axis.padj,
         hadj = theme$y.axis.hadj,
         col = theme$axes.col,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col.axis = theme$tick.labels.col,
         tck = theme$tck,
         tcl = theme$tcl,
         cex = theme$cex,
         family = theme$font.family)
    mtext(xlab, side = 1,
          line = theme$xlab.line,
          cex = theme$cex,
          col = theme$labs.col,
          family = theme$font.family)
    mtext(ylab, side = 2,
          line = theme$ylab.line,
          cex = theme$cex,
          col = theme$labs.col,
          family = theme$font.family)
  }

  # [ ZERO LINES ] ====
  if (type == "index" && theme$zerolines) {
    zerocol <- adjustcolor(theme$zerolines.col, theme$zerolines.alpha)
    if (ylim[1] <= 0 & 0 <= ylim[2]) abline(h = 0, lwd = theme$zerolines.lwd,
                                            col = zerocol, lty = theme$zerolines.lty)
    if (xlim[1] <= 0 & 0 <= xlim[2]) abline(v = 0, lwd = theme$zerolines.lwd,
                                            col = zerocol, lty = theme$zerolines.lty)
  }

  if (theme$bty != "n") {
    box(col = adjustcolor(theme$box.col, theme$box.alpha),
        lty = theme$box.lty, lwd = theme$box.lwd, bty = theme$bty)
  }

  # [ PLOT: INDEX ] ====
  if (type == "index") {
    if (length(index.type) < length(xl)) index.type <- rep(index.type,
                                                           length(xl)/length(index.type))
    for (i in seq_along(xl)) {
      if (!axes.swap) {
        points(yl[[i]], xl[[i]],
               type = index.type[[i]],
               col = col[[i]],
               bg = point.bg.col,
               lwd = lwd,
               pch = pch,
               cex = point.cex)
      } else {
        points(xl[[i]], yl[[i]],
               type = index.type[[i]],
               col = col[[i]],
               bg = point.bg.col,
               lwd = lwd,
               pch = pch,
               cex = point.cex)
      }
    }

  }

  # [ PLOT: DENSITY ] ====
  if (type == "density") {
    if (!axes.swap) {
      for (i in seq_along(xl)) {
        if (density.shade) {
          polygon(c(densityl[[i]]$x, rev(densityl[[i]]$x)), c(densityl[[i]]$y,
                                                              rep(0, length(densityl[[i]]$y))),
                  col = col.alpha[[i]], border = NA)
        }
        if (density.line) {
          lines(densityl[[i]]$x, densityl[[i]]$y, xlim = xlim, ylim = ylim, ann = FALSE,
                col = col.alpha[[i]],
                lwd = density.lwd, type = "l", lty = lty[[i]])
        }
      }
      if (is.null(xlab)) xlab <- labelify(xname)
      ylab <- "Density"
    } else {
      for (i in seq_along(xl)) {
        if (density.shade) {
          polygon(c(densityl[[i]]$y, rev(densityl[[i]]$y)), c(densityl[[i]]$x,
                                                              rep(0, length(densityl[[i]]$x))),
                  col = col.alpha[[i]], border = NA)
        }
        if (density.line) {
          lines(densityl[[i]]$y, densityl[[i]]$x, xlim = ylim, ylim = xlim, ann = FALSE,
                col = col.alpha[[i]],
                lwd = density.lwd, type = "l", lty = lty[[i]])
        }
      }
      xlab <- "Density"
    }

  } # type == "density"

  # [ PLOT: HISTOGRAM ] ====
  if (type %in% c("histogram", "hd")) {
    if (length(xl) > 1) {
      breaks <- hist(unlist(xl), breaks = hist.breaks,
                     # probability = hist.probability,
                     plot = FALSE)$breaks
      dist <- diff(breaks)[1] # mean(diff(breaks))
      breaksl <- lapply(seq_along(xl), function(i) {
        c(breaks - ((i - 1)/length(xl) * dist), max(breaks) - ((i - 1)/length(xl) * dist) + dist)
      })
    } else {
      breaksl <- list(histl[[1]]$breaks)
    }
  }

  if (type == "hd") {
    hist.type <- "bars"
    hist.probability <- TRUE
  }

  if (type %in% c("histogram", "hd")) {
    if (hist.type == "bars") {
      for (i in seq_along(xl)) {
        hist(xl[[i]], breaks = hist.breaks,
             col = col.alpha[[i]], add = TRUE,
             probability = hist.probability,
             border = theme$bg, xlim = xlim)
      }
    } else {
      for (i in seq_along(xl)) {
        mhist(xl[[i]], measure = "count", breaks = breaksl[[i]],
              col = col.alpha[[i]], add = TRUE,
              lwd = hist.lwd,
              xlim = xlim, ylim = ylim, plot.axes = FALSE,
              xaxis = FALSE, yaxis = FALSE, xlab = "", ylab = "")
      }
    }
  }

  # [PLOT: DH LINES ] ====
  if (type == "hd") {
    for (i in seq_along(xl)) {
      lines(densityl[[i]]$x, densityl[[i]]$y,
            col = col[[i]], lwd = lwd)
    }
  }

  # [ MAIN ] ====
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    mtext(main, line = theme$main.line,
          font = theme$main.font, adj = theme$main.adj,
          cex = theme$cex, col = theme$main.col,
          family = theme$font.family)
  }

  # [ GROUP LEGEND ] ====
  if (group.legend) {
    mtext(group.names,
          col = c(theme$fg, unlist(col[seq_along(xl)])),
          side = group.side,
          adj = group.adj,
          at = group.at,
          cex = theme$cex,
          padj = seq(2, 2 + 1.5 * length(xl), 1.5),
          family = theme$font.family)
  }

  # [ ANNOTATION ] ====
  if (!is.null(annotation)) {
    if (is.null(annot.col)) annot.col = col[[1]]
    mtext(annotation, 1, -1.5, adj = .97, cex = theme$cex, col = annot.col,
          family = theme$family)
  }

  # [ QQ LINE ] ====
  if (is.null(qqline.col)) {
    if (length(xl) == 1) {
      qqline.col <- palette[1]
    } else {
      qqline.col <- palette
    }
  }
  qqline.col <- lapply(qqline.col, function(x) adjustcolor(qqline.col, qqline.alpha))
  qqline.col <- recycle(qqline.col, xl)
  # if (length(qqline.col) < length(xl)) qqline.col <- rep(qqline.col,
  #                                                        length(xl) / length(qqline.col))

  if (type == "qqline") {
    for (i in seq_along(xl)) {
      points(qxl[[i]], qyl[[i]],
             type = "p",
             col = col.alpha[[i]],
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
  if (type == "density" & density.avg) {
    avgl <- lapply(xl, density.avg.fn, na.rm = TRUE)
    sdl <- lapply(xl, sd, na.rm = TRUE)
    density.legend.text <- if (density.avg.fn == "mean") "Mean (SD)" else "Median (SD)"
    mtext(c(density.legend.text, lapply(seq(xl),
                                function(j) paste0(ddSci(avgl[[j]]), " (", ddSci(sdl[[j]]), ")"))),
          col = c(theme$fg, unlist(col[seq_along(xl)])),
          side = density.legend.side, adj = density.legend.adj, cex = theme$cex,
          padj = seq(2, 2 + 1.5 * length(xl), 1.5))
    if (density.avg.line) {
      abline(v = unlist(avgl), col = unlist(col[seq_along(xl)]),
             lwd = density.avg.lwd, lty = density.avg.lty)
    }
  }

  # [ MISC LEGENDS ] ====
  if (!is.null(text.xy)) {
    if (is.null(text.x)) text.x <- mean(xlim)
    if (is.null(text.y)) text.y <- mean(ylim)
    text(x = text.x, y = text.y, labels = text.xy, cex = text.xy.cex, col = text.xy.col)
  }

  # [ HLINE & VLINE ] ====
  if (!is.null(hline)) abline(h = hline, lwd = hline.lwd, col = hline.col, lty = hline.lty)
  if (!is.null(vline)) abline(v = vline, lwd = vline.lwd, col = vline.col, lty = vline.lty)

  # [ Outro ] ====
  if (!is.null(filename)) grDevices::dev.off()
  invisible(.out)

} # rtemis::mplot3.x
