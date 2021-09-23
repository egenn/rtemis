# mplot3.box.R
# ::rtemis::
# 2017-2021 E.D. Gennatas lambdamd.org

#' \code{mplot3}: Boxplot
#'
#' Draw boxplots of a vector (single box), data.frame (one box per column) or list
#' (one box per element - good for variable of different length)
#'
#' Note that argument `xnames` refers to the x-axis labels below each box. If not specified, these
#' are inferred from the input when possible. Argument `xlab` is a single label for the x-axis as
#' per usual and often omitted if `xnames` suffice.
#' @inheritParams mplot3.xy
#' @param x Vector, data.frame or list: Each data.frame column or list element will be drawn as a box
#' @param col Vector of colors to use
#' @param alpha Float: Alpha to be applied to \code{col}
#' @param border Color for lines around boxes
#' @param boxwex Float: Scale factor for box width. Default = .5
#' @param staplewex Float: max and min line ("staple") width proportional to box. Default = .5
#' @param horizontal Logical: If TRUE, draw horizontal boxplot(s). Default = FALSR
#' @param na.rm Logical: If TRUE, remove NA values, otherwise function will give error.
#' Default = TRUE
#' @param order.by.fn Character: "mean", "median" or any function that outputs a single number: E
#' stimate function on each vector and order boxes (when input is data.frame or list) by ascending
#' order. Default = NULL, i.e. no reordering
#' @param ... Additional arguments to \code{graphics::boxplot}
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' ## vector
#' x <- rnorm(500)
#' mplot3.box(x)
#'
#' ## data.frame - each column one boxplot
#' x <- data.frame(alpha = rnorm(50), beta = rnorm(50), gamma = rnorm(50))
#' mplot3.box(x)
#'
#' ## list of vectors - allows different length vectors
#' x <- list(alpha = rnorm(50),
#'           beta = rnorm(80, 4, 1.5),
#'           gamma = rnorm(30, -3, .5))
#' mplot3.box(x)
#'
#' ## grouped boxplots: input a list of lists. outer list: groups; inner lists: matched data vectors
#' x <- list(Cases = list(Weight = rnorm(50), Temperature = rnorm(45, 1)),
#'           Controls = list(Weight = rnorm(80), Temperature = rnorm(72)))
#' mplot3.box(x)
#' }
#' @export

mplot3.box <- function(x,
                       col = NULL,
                       alpha = .66,
                       border = NULL,
                       border.alpha = 1,
                       group.spacing = .25,
                       xlim = NULL,
                       ylim = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       boxwex = NULL,
                       staplewex = .5,
                       horizontal = FALSE,
                       main = NULL,
                       names.arg = NULL,
                       axisnames = FALSE,
                       groupnames = NULL,
                       xnames = NULL,
                       xnames.at = NULL,
                       xnames.y = NULL,
                       xnames.font = 1,
                       xnames.adj = NULL,
                       xnames.pos = NULL,
                       xnames.srt = NULL,
                       order.by.fn = NULL,
                       legend = FALSE,
                       legend.names = NULL,
                       legend.position = "topright",
                       legend.inset = c(0, 0),
                       mar = NULL, # auto, 3, 1or2, 1
                       oma = rep(0, 4),
                       pty = "m",
                       yaxis = TRUE,
                       ylim.pad = 0,
                       theme = getOption("rt.theme", "lightgrid"),
                       labelify = TRUE,
                       autolabel = letters,
                       na.rm = TRUE,
                       palette = getOption("rt.palette", "rtCol1"),
                       par.reset = TRUE,
                       pdf.width = 6,
                       pdf.height = 6,
                       filename = NULL, ...) {

  # Arguments ====
  .grouped <- is.list(x) & is.list(x[[1]])
  if (.grouped) {
    ngroups <- length(x)
    nvars <- length(x[[1]])
    if (is.null(groupnames)) groupnames <- names(x)
    if (is.null(xnames)) xnames <- names(x[[1]])
    # if (is.null(xnames.at)) {
    #   xnames.at <- seq(mean(seq(nvars)), ngroups * nvars/2 + (ngroups - 1) * group.spacing,
    #                    length.out = ngroups)
    # }
  } else {
    ngroups <- 0 # used for xlim
  }
  if (is.null(boxwex)) boxwex <- if (.grouped) .75 else .5

  # Group names
  if (is.null(xnames)) {
    if (!is.null(names(x))) {
      xnames <- names(x)
    }
    # no xnames for vector
    # if (is.null(dim(x))) xnames <- deparse(substitute(x))
  }
  if (labelify) xnames <- labelify(xnames)
  if (is.null(ylab) & !horizontal & !.grouped) ylab <- deparse(substitute(x))
  if (is.null(xlab) & horizontal) xlab <- deparse(substitute(x))
  if (!is.list(x)) x <- list(x)
  # xnames on x-axis only for not grouped, otherwise as legend
  if (!is.null(xnames) & !.grouped) {
    if (is.null(xnames.at)) {
      xnames.at <- seq_along(xnames)
    }
  }

  if (!is.null(xnames) & !.grouped) {
    if (is.null(xnames.srt)) {
      if (horizontal) {
        xnames.srt <- 0
      } else {
        xnames.srt <- ifelse(length(x) * max(nchar(xnames)) > 8, 90, 0)
      }
    }

    if (is.null(xnames.adj)) {
      if (horizontal) {
        xnames.adj <- 1
      } else {
        xnames.adj <- if (xnames.srt == 0) c(.5, 1) else 1
      }
    }
  } else {
    xnames.srt <- 0
  }

  if (is.character(palette)) palette <- rtPalette(palette)

  if (is.null(col)) {
    if (length(x) == 1) {
      col <- palette[1]
    } else {
      col <- if (.grouped) {
        palette[seq_len(nvars)]
      } else {
        palette[seq_along(x)]
      }
    }
  }

  # mar ====
  if (is.null(mar)) {
    # mar.bottom <- if (xnames.srt != 0) 1.8571 + max(nchar(xnames)) * .4107 else 2.5
    mar.bottom <- if (xnames.srt != 0) textwidth(xnames) else 2.5
    mar.left <- if (horizontal) textwidth(xnames) else 3
    mar.top <- if (is.null(main)) 1 else 2
    mar.right <- if (.grouped) textwidth(xnames) else 1
    mar <- c(mar.bottom, mar.left, mar.top, mar.right)
  }

  col.alpha <- colorAdjust(col, alpha = alpha)
  if (is.null(border)) border <- colorAdjust(col, alpha = border.alpha)

  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # Theme ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # xlim & ylim ====
  xv <- unlist(x)

  if (is.null(xlim)) {
    xlim <- if (.grouped) {
      c(.5, ngroups*nvars + (ngroups - 1) * group.spacing + .5)
    } else {
      c(.5, length(x) + .5)
    }
  }

  if (is.null(ylim)) ylim <- getlim(xv)
  if (horizontal) {
    xxlim <- ylim
    ylim <- xlim
    xlim <- xxlim
  }

  # Plot ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height,
                              title = "rtemis Graphics")
  par.orig <- par(no.readonly = TRUE)
  if (!is.null(rtenv$rtpar)) {
    par.reset <- FALSE
    par(mar = mar, bg = theme$bg, pty = pty, cex = theme$cex)
  } else {
    par(mar = mar, oma = oma, bg = theme$bg, pty = pty, cex = theme$cex)
  }
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  plot(NULL, NULL, xlim = xlim, ylim = ylim, bty = "n",
       axes = FALSE, ann = FALSE,
       xaxs = "i", yaxs = "i")

  # Plot bg ====
  if (!is.na(theme$plot.bg)) {
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = theme$plot.bg)
  }

  # Grid ====
  if (theme$grid) {
    if (horizontal) {
      grid(nx = theme$grid.nx,
           ny = 0,
           col = colorAdjust(theme$grid.col, theme$grid.alpha),
           lty = theme$grid.lty,
           lwd = theme$grid.lwd)
    } else {
      grid(nx = 0,
           ny = theme$grid.ny,
           col = colorAdjust(theme$grid.col, theme$grid.alpha),
           lty = theme$grid.lty,
           lwd = theme$grid.lwd)
    }
  }

  # Order by fn ====
  if (!.grouped & !is.null(order.by.fn) && order.by.fn != "none") {
    if (is.list(x)) {
      .order <- order(sapply(x, order.by.fn, na.rm = TRUE))
      if (is.data.frame(x)) {
        x <- x[, .order]
      } else {
        x <- x[names(x)[.order]]
      }
    }
    if (!is.null(xnames)) xnames <- xnames[.order]
  }

  if (.grouped & !is.null(order.by.fn) && order.by.fn != "none") {
    groupmeans <- sapply(x, function(y) do.call(order.by.fn, list(x = unlist(y), na.rm = TRUE)))
    .order <- order(groupmeans)
    x <- x[.order]
    if (!is.null(groupnames)) groupnames <- groupnames[.order]
  }

  # Boxplot ====
  if (.grouped) {
    bp <- vector("list", length(x))
    for (i in seq_along(x)) {
      bp[[i]] <- boxplot(x[[i]], col = col.alpha,
                         pch = theme$pch,
                         border = border,
                         boxwex = boxwex,
                         horizontal = horizontal,
                         ylim = ylim,
                         axes = FALSE,
                         add = TRUE,
                         at = (i - 1) * group.spacing + ((i - 1)*nvars + seq_len(nvars)),
                         xlab = NULL)
    }
  } else {
    bp <- boxplot(x, col = col.alpha,
                  pch = theme$pch,
                  border = border,
                  boxwex = boxwex,
                  horizontal = horizontal,
                  ylim = ylim,
                  axes = FALSE,
                  add = TRUE,
                  xlab = NULL)
  }

  # y axis ====
  if (yaxis) {
    axis(side = if (horizontal) 1 else 2,
         las = if (horizontal) theme$x.axis.las else theme$y.axis.las,
         padj = if (horizontal) theme$x.axis.padj else theme$y.axis.padj,
         hadj = if (horizontal) theme$x.axis.hadj else theme$y.axis.hadj,
         col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
         col = NA, # The axis line, which we want to omit
         col.axis = theme$tick.labels.col, # the axis numbers i.e. tick labels
         tck = theme$tck,
         tcl = theme$tcl,
         cex = theme$cex,
         family = theme$font.family)
  }

  # Main Title ====
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (!is.null(main)) {
    mtext(text = main, side = 3, line = theme$main.line,
          font = theme$main.font, adj = theme$main.adj,
          cex = theme$cex, col = theme$main.col,
          family = theme$font.family)
  }

  # xnames ====
  if (length(xnames) > 0 & !.grouped) {
    if (horizontal) {
      # .x <- xlim[1] - .04 * diff(xlim)
      text(x = xleft(.04),
           y = xnames.at,
           labels = xnames,
           adj = xnames.adj,
           pos = xnames.pos,
           srt = xnames.srt, xpd = TRUE,
           font = xnames.font,
           col = theme$labs.col,
           family = theme$font.family)
    } else {
      if (is.null(xnames.y)) {
        xnames.y <- ylo(.04)
      }
      text(x = xnames.at, y = xnames.y,
           labels = xnames,
           adj = xnames.adj,
           pos = xnames.pos,
           srt = xnames.srt, xpd = TRUE,
           font = xnames.font,
           col = theme$labs.col,
           family = theme$font.family)
    }
  }

  # Axes Labels ====
  # if (!is.null(xlab))  mtext(xlab, 1, cex = theme$cex, line = theme$xlab.line)
  # if (!is.null(ylab))  mtext(ylab, 2, cex = theme$cex, line = theme$ylab.line)

  if (!is.null(xlab)) mtext(xlab, side = theme$x.axis.side,
                            line = theme$xlab.line, cex = theme$cex,
                            # adj = xlab.adj,
                            col = theme$labs.col,
                            family = theme$font.family)
  if (!is.null(ylab)) mtext(ylab, side = theme$y.axis.side,
                            line = theme$ylab.line, cex = theme$cex,
                            # adj = ylab.adj,
                            col = theme$labs.col,
                            family = theme$font.family)

  # Group names and legend ====
  if (.grouped) {

    # Group names below x-axis
    groupnames.at <- mean(seq(nvars)) + (seq(ngroups) - 1) * (nvars + group.spacing)
    mtext(text = groupnames,
          side = if (horizontal) 2 else 1,
          line = .5, at = groupnames.at,
          col = theme$labs.col)

    # Variable names in top-right legend
    mtextlegend(labels = xnames,
                font.family = theme$font.family,
                col = col)
  }

  # Outro ====
  if (!is.null(filename)) dev.off()
  invisible(bp)

} # rtemis::mplot3.box
