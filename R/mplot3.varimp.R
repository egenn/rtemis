# mplot3.varimp
# ::rtemis::
# 2017-9 Efstathios D. Gennatas egenn.github.io

#' \code{mplot3}: Variable Importance
#'
#' Draw horizontal barplots for variable importance
#'
#' @param x Vector, numeric: Input
#' @param error Vector, numeric; length = length(x): Plot error bars with given error.
#' @param names Vector, string; optional: Names of variables in \code{x}
#' @param plot.top Float or Integer: If <= 1, plot this percent highest absolute values, otherwise plot this many top values.
#' i.e.: \code{plot.top = .2} will print the top 20% highest values, and \code{plot.top = 20} will plot the top 20
#' highest values
#' @param labelify Logical: If TRUE convert \code{names(x)} using \link{labelify}. Default = TRUE
#' @param col Colors: Gradient to use for barplot fill.
#' @param alpha Float (0, 1): Alpha for \code{col}
#' @param error.col Color: For error bars
#' @param trace Integer: If \code{trace > 0} prints out the automatically set \code{mar} (so you can adjust if needed)
#' \code{names} provided
#' @return Position of bar centers (invisibly)
#' @author Efstathios D. Gennatas
#' @export

mplot3.varimp <- function(x,
                          error = NULL,
                          names = NULL,
                          names.pad = .02,
                          plot.top = 1, # 1 or less means plot this percent
                          labelify = TRUE,
                          col = NULL,
                          palette = NULL,
                          alpha = .8,
                          error.col = "white",
                          error.lwd = 2,
                          beside = TRUE,
                          border = NA,
                          width = 1,
                          space = .2,
                          xlim = NULL,
                          ylim = NULL,
                          xlab = "Variable Importance",
                          xlab.line = 1.3,
                          ylab = NULL,
                          ylab.line = 1.5,
                          main = NULL,
                          main.line = .5,
                          main.adj = 0,
                          main.col = NULL,
                          main.font = 2,
                          main.family = "",
                          names.arg = NULL,
                          axisnames = FALSE,
                          sidelabels = NULL,
                          mar = NULL,
                          pty = "m",
                          cex = 1.2,
                          cex.axis = cex,
                          cex.names = cex,
                          bg = NULL,
                          plot.bg = NULL,
                          barplot.axes = FALSE,
                          xaxis = TRUE,
                          x.axis.padj = -1.2,
                          tck = -.015,
                          tick.col = NULL,
                          theme = getOption("rt.theme", "light"),
                          axes.col = NULL,
                          labs.col = NULL,
                          grid = FALSE,
                          grid.lty = NULL,
                          grid.lwd = NULL,
                          grid.col = NULL,
                          grid.alpha = 1,
                          par.reset = TRUE,
                          pdf.width = NULL,
                          pdf.height = NULL,
                          trace = 0,
                          filename = NULL, ...) {

  # [ DATA ] ====
  if (NCOL(x) > 1 && NROW(x) > 1) stop("x must be a vector or single row or column")

  # '- Names ====
  if (is.null(names)) {
    if (is.null(names(x))) {
      .names <- if (NCOL(x) == 1) labelify(rownames(x)) else labelify(colnames(x))
    } else {
      .names <- labelify(names(x))
    }
  } else {
    .names <- labelify(names)
  }

  x <- as.numeric(x)
  if (length(.names) == 0) {
    .names <- paste("Feature", 1:length(x))
  }

  # '- Index ====
  index <- if (plot.top <= 1) {
    order(abs(x))[(length(x) - plot.top * length(x)):length(x)]
  } else {
    if (plot.top > length(x)) plot.top <- length(x)
    order(abs(x))[(length(x) - plot.top + 1):length(x)]
  }
  x <- x[index]
  .names <- .names[index]
  # reorder to arrange negative to positive
  index <- order(x)
  x <- x[index]
  .names <- .names[index]
  if (!is.null(error)) error <- error[index]

  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # # [ XLIM & YLIM ] ====
  if (is.null(ylim)) {
    ylim <- c(0, length(x) + (length(x) + 1) * space)
  }

  if (is.null(xlim)) {
    .error <- if (is.null(error)) 0 else error
    x.range <- max(x + .error) - min(x + .error)
    x.min <- min(x + .error) - .04 * x.range
    x.max <- max(x + .error) + .04 * x.range
    xlim <- range(c(0, x.min, x.max))
  }

  # Add x% either side (unless zero)
  # ylim[1] <- ylim[1] - ylim.pad * diff(ylim)
  # ylim[2] <- ylim[2] + ylim.pad * diff(ylim)

  # [ THEMES ] ====
  # Defaults for all themes
  if (is.null(grid.lty)) grid.lty <- 1
  if (is.null(grid.lwd)) grid.lwd <- 1

  if (theme == "lightgrid" | theme == "darkgrid") {
    if (is.null(grid.lty)) grid.lty <- 1
    # if (is.null(zero.lty)) zero.lty <- 1
    if (is.null(grid.lwd)) grid.lwd <- 1.5
  }
  if (theme == "lightgrid") {
    theme <- "light"
    if (is.null(plot.bg)) plot.bg <- "gray90"
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "white"
    if (is.null(tick.col)) tick.col <- "white"
  }
  if (theme == "darkgrid") {
    theme <- "dark"
    if (is.null(plot.bg)) plot.bg <- "gray15"
    grid <- TRUE
    if (is.null(grid.col)) grid.col <- "black"
    if (is.null(tick.col)) tick.col <- "black"
  }
  themes <- c("light", "dark", "lightbox", "darkbox")
  if (!theme %in% themes) {
    warning(paste(theme, "is not an accepted option; defaulting to \"light\""))
    theme <- "light"
  }

  if (length(grep("light", theme) > 0)) {
    # light themes
    if (is.null(bg)) bg <- "white"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray10"
    if (is.null(labs.col)) labs.col <- "gray10"
    if (is.null(main.col)) main.col <- "black"
    if (is.null(grid.col)) grid.col <- "black"
    if (is.null(col)) {
      if (is.null(palette)) palette <- c("gray20", "#18A3AC")
      col <- colorGrad.x(x, palette)
    }
  } else {
    # dark themes
    if (is.null(bg)) bg <- "black"
    if (is.null(axes.col)) axes.col <- adjustcolor("black", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray90"
    if (is.null(labs.col)) labs.col <- "gray90"
    if (is.null(main.col)) main.col <- "white"
    if (is.null(grid.col)) grid.col <- "white"
    if (is.null(col)) {
      if (is.null(palette)) palette <- c("gray80", "#18A3AC")
      col <- colorGrad.x(x, palette)
    }
  }

  cols <- colorAdjust(col, alpha = alpha)

  # [ AUTOMARGINS ] ====
  if (is.null(mar)) {
    maxchar <- max(nchar(.names))
    # This gets too large as n increases; rerun with average-width character
    # measure using 'w' - a wide character (helvetica)
    # nchar = 1; mar2 = 1; 1.5
    # nchar = 28; mar2 = 13; 18
    # x <- c(1, 28); y <- c(1.5, 13)
    # modlm <- s.LM(x, y)
    mar1 <- ifelse(xlab == "", 1.5, 2.5)
    mar2 <- 1.0741 + 0.4259 * maxchar
    mar3 <- if (is.null(main)) .5 else 2
    mar <- c(mar1, mar2, mar3, .6)
    if (trace > 0) cat(crayon::silver("mar set to"), mar)
  }

  # [ PLOT ] ====
  # '- PDF autosize ====
  if (!is.null(filename)) {
    if (is.null(pdf.height)) pdf.height <- length(x) *.2 + .5
    if (is.null(pdf.width)) pdf.width <- mar2 *.7
  }

  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  par(mar = mar, bg = bg, pty = pty, cex = cex)
  plot(NULL, NULL, xlim = xlim, ylim = ylim, bty = 'n', axes = FALSE, ann = FALSE,
       xaxs = "i", yaxs = "i")

  # [ PLOT BG ] ====
  if (!is.null(plot.bg)) {
    # bg.ylim <- c(min(ylim) - .04 * diff(range(ylim)), max(ylim) + .04 * diff(range(ylim)))
    bg.ylim <- c(min(ylim), max(ylim) + .04 * diff(range(ylim)))
    rect(xlim[1], bg.ylim[1], xlim[2], bg.ylim[2], border = NA, col = plot.bg)
  }

  # [ GRID ] ====
  grid.col <- colorAdjust(grid.col, grid.alpha)
  if (grid) grid(col = grid.col, lty = grid.lty, lwd = grid.lwd, ny = 0, nx = NULL)

  # [ BARPLOT ] ====
  barCenters <- barplot(x, col = cols, border = border,
                        xlim = xlim, ylim = ylim, axes = barplot.axes,
                        cex.axis = cex.axis, cex.names = cex.names, add = TRUE, xlab = NULL,
                        axisnames = axisnames, names.arg = names.arg,
                        width = width, space = space, horiz = TRUE,
                        # xpd = FALSE,
                        xaxs = "i", yaxs = "i", ...)
  if (xlim[1] < 0 & 0 < xlim[2]) abline(v = 0, col = labs.col, lwd = 1.5)

  # [ ERROR BARS ] ====
  if (!is.null(error)) {
    if (is.null(error.col)) error.col <- cols
    segments(as.vector(x) - as.vector(error), as.vector(barCenters),
             as.vector(x) + as.vector(error), as.vector(barCenters),
             lwd = error.lwd, col = error.col)

    arrows(x - as.vector(error), barCenters,
           x + as.vector(error), barCenters,
           lwd = error.lwd, angle = 90, code = 3, length = 0.05, col = error.col)
  }

  # [ x AXIS ] ====
  if (xaxis) axis(1, col = axes.col, col.axis = labs.col, col.ticks = tick.col,
                  padj = x.axis.padj, tck = tck, cex = cex)

  # [ MAIN ] ====
  if (!is.null(main)) {
    # suppressWarnings(mtext(bquote(paste(bold(.(main)))), line = main.line,
    #                        adj = main.adj, cex = cex, col = main.col))
    mtext(main, line = main.line, font = main.font, family = main.family,
          adj = main.adj, cex = cex, col = main.col)
  }

  # [ ROWNAMES for 1 column ] ====
  # if (ncol(x) == 1) {
  #   if (!is.null(rownames(x))) {
  #     text(x = 1:nrow(x) + .8, y = -diff(ylim) * .033, labels = rownames(x), srt = 45, xpd = NA, pos = 2)
  #     # axis(side = 1, 1:nrow(x), at = 1:nrow(x) + .5, labels = rownames(x),
  #     #      tick = FALSE, hadj = 0, las = 3)
  #   }
  # }

  # [ NAMES ] ====
  text(x = min(xlim) - names.pad * diff(xlim),
       y = barCenters,
       labels = .names, adj = 1, xpd = TRUE,
       col = labs.col)

  # [ AXIS LABS ] ====
  if (!is.null(xlab))  mtext(xlab, 1,
                             cex = cex,
                             line = xlab.line,
                             col = labs.col)
  if (!is.null(ylab))  mtext(ylab, 2,
                             cex = cex,
                             line = ylab.line,
                             col = labs.col)

  # [ SIDE LABELS ] ====
  if (!is.null(sidelabels)) {
    # mtext(sidelabels, 4, at = barCenters)
    text(x = max(xlim)*1.01, y = barCenters, labels = sidelabels, xpd = TRUE, pos = 4)
  }

  # [ OUTRO ] ====
  if (!is.null(filename)) dev.off()
  invisible(list(barCenters = barCenters,
                 xlim = xlim,
                 ylim = ylim))

} # rtemis::mplot3.varimp
