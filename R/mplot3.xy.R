# mplot3.xy.R
# ::rtemis::
# 2016-2018 Efstathios D. Gennatas egenn.github.io
# inv mplot3.xy(c(NULL, NULL, 3:10), 1:10)


#' \code{mplot3}: XY Scatter and line plots
#'
#' Plot points and lines with optional fits and standard error bands
#'
#' This is relatively old code and may need a little cleaning up
#'
#' @param x Numeric vector or list of vectors for x-axis.
#'   If \code{data} is provided, name of variable, unquoted.
#' @param y Numeric vector of list of vectors for y-axis
#'   If \code{data} is provided, name of variable, unquoted.
#' @param fit String: \pkg{rtemis} model to calculate y ~ x fit. Options: see \code{modSelect}
#'   Can also be Logical, which will give a GAM fit if TRUE. If you specify "NLA", the activation function should
#'   be passed as a string.
#' @param formula Formula: Provide a formula to be solved using \link{s.NLS}. If provided, \code{fit} is
#' forced to \code{'nls'}. e.g. y ~ b * m ^ x for a power curve. Note: \code{nls} is prone to errors
#' and warnings, but is powerful. Use single letters for parameter names, no numbers.
#' @param se.fit Logical: If TRUE, draw the standard error of the fit
#' @param error.x Vector, float: Error in \code{x} (e.g. standard deviation) will be plotted as bars around point
#' @param error.y Vector, float: Error in \code{y} (e.g. standard deviation) will be plotted as bars around point
#' @param cluster String: Clusterer name. Will cluster \code{data.frame(x, y)} and
#' pass result to \code{group}. Run \link{clustSelect} for options
#' @param cluster.params List: Names list of parameters to pass to the \code{cluster} function
#' @param data (Optional) data frame, where \code{x} and \code{y} are defined
#' @param group Vector: will be converted to factor.
#'   If \code{data} is provided, name of variable, unquoted.
#' @param xlab String: x-axis label
#' @param ylab String: y-axis label
#' @param main String: Plot title
#' @param main.line Float: \code{mtext line} argument for the title. Higher numbers move title upwards.
#' Default = .5
#' @param main.adj Float: Plot title alignment (0 = left, .5 = center, 1 = right)
#' @param main.col Color for title. Defaults to black for theme "light" and "box", otherwise white
#' @param main.font Integer: 1: regular, 2: bold, 3: italic. See \code{par("font")} for more
#' @param main.family String: Font family to use. See \code{par("family")}
#' @param cex Float: Character expansion factor (See \code{?par})
#' @param se.lty How to draw the \code{se.fit} "poly" draws a polygon around the fit line,
#'   otherwise an integer defines the lty (line type) for lines to be drawn
#' @param se.col Color for \code{se.fit}
#' @param se.alpha Alpha for \code{se.fit}
#' @param se.times Draw polygon or lines at +/- \code{se.times} * standard error of fit. Defaults to 2
#'   (1.96 * standard error corresponds to 95\% confidence interval)
#' @param se.border Define border of polygon for \code{se.fit}.
#'   See \code{border} in \code{graphics::polygon}
#' @param se.density Density of shading line of polygon for \code{se.fit}.
#'   See \code{density} in \code{graphics::polygon}
#' @param type String: "p" for points, "l" for lines, "s" for steps. Default = "p". If \code{x} and/or \code{y} contains multiple
#'   vectors, \code{type} can be a vector, e.g. \code{c("p", "l", "l")} will give a set of points and two sets
#'   of lines. Otherwise, \code{type} is recycled to length of x
#' @param xlim Float vector, length 2: x-axis limits
#' @param ylim Float vector, length 2: y-axis limits
#' @param axes Logical: Should the axes be drawn? Defaults to TRUE
#' @param axes.equal Logical: Should axes be equal? Defaults to FALSE
#' @param axes.col String: Color for axes values (box color set with \code{box.col})
#' @param pty "s" gives a square plot, "m" gives a plot that fills graphics device size. Defaults to "s"
#'   (See \code{?par})
#' @param box Logical: Should a box be drawn around the plot? Defaults to TRUE for themes "box" and "darkbox"
#' @param bty Character: "o", "l", "7", "c", "u", or "]" result in a box resembling each character.
#'   (See \code{?par})
#' @param box.col Box color
#' @param box.alpha Alpha for \code{box.col}
#' @param box.lty Box line type
#' @param bot.lwd Box line width
#' @param fit.lwd Fit line width
#' @param grid Logical: Should grid be drawn?
#' @param grid.nx Integer: N of grid lines on the x axis.
#' Use with caution, probably along with \code{xaxs = "i"}
#' @param grid.ny Integer: N of grid lines on the y axis
#' Use with caution, probably along with \code{yaxs = "i"}
#' @param grid.col Grid color
#' @param grid.alpha Alpha for \code{grid.col}
#' @param grid.lty Grid line type (See \code{?par("lty")})
#' @param grid.lwd Grid line width
#' @param bg Background color. Defaults to white for themes "light" and "box", black otherwise.
#' @param rsq Logical: If TRUE, add legend with R-squared (if fit is not NULL)
#' @param rsq.pval Logical: If TRUE, add legend with R-squared and its p-value (if fit is not NULL)
#' @param zero.lines Logical: If TRUE, draw lines at x = 0 and y = 0
#' @param zero.col Color for \code{zero.lines}
#' @param zero.alpha Color alpha for \code{zero.lines}
#' @param zero.lty Zero line line type
#' @param zero.lwd Zero line width
#' @param annotation String: Add annotation at the bottom right of the plot
#' @param annotation.col Color for annotation
#' @param tck Float: Tick length. Can be negative (See \code{?par("tck")})
#' @param x.axis.padj Adjustment for the x axis tick labels position
#' @param xlab.line Adjustment for the x axis label position (See code{line} in \code{?mtext})
#' @param y.axis.padj Similar to \code{x.axis.padj} for the y axis
#' @param ylab.line Similar to \code{xlab.line} for the y axis
#' @param xlab.adj \code{adj} for \code{xlab} (See \code{?par})
#' @param ylab.adj \code{adj} for \code{ylab} (See \code{?par})
#' @param theme String: "light", "dark", "lightgrid", "darkgrid", "lightbox", "darkbox"
#' Default = "lightgrid" if no default \code{"rt.fit"} is set using \code{options}.
#' You can set a system-wide default in your \code{.Rprofile} by including a line like
#' options(rt.theme = 'lightgrid')
#' @param mar Vector, length 4: Margins; see \code{mar} in \code{?par}
#' @param group.legend Logical: If TRUE, place \code{group.names} in a legend
#' @param group.names (Optional) If multiple groups are plotted, use these names if \code{group.title = TRUE}
#' @param group.title String: Group title, shown above group names. e.g. if group names are
#'   c("San Francisco", "Philadelphia"), \code{group.title} can be "Place of residence"
#' @param new Logical: If TRUE, add plot to existing plot. See \code{par("new")}
#' @param ... Additional arguments to be passed to learner function
#' @author Efstathios D. Gennatas
#' @export

mplot3.xy <- function(x, y,
                      fit = NULL,
                      formula = NULL,
                      se.fit = FALSE,
                      mod.params = NULL,
                      error.x = NULL,
                      error.y = NULL,
                      cluster = NULL,
                      cluster.params = list(),
                      data = NULL,
                      type = NULL,
                      group = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      main = NULL,
                      main.line = .5,
                      main.adj = 0,
                      main.col = NULL,
                      main.font = 2,
                      main.family = "",
                      cex = 1.2,
                      xlim = NULL,
                      ylim = NULL,
                      xpd = TRUE,
                      xaxs = "r",
                      yaxs = "r",
                      rsq = NULL,
                      rsq.pval = FALSE,
                      rsq.side = 1,
                      rsq.adj = .98,
                      rsq.padj.shift = 0,
                      rsq.col = NULL,
                      fit.error = FALSE,
                      fit.error.side = 1,
                      fit.error.padj = NA,
                      fit.error.col = NULL,
                      xaxp = NULL,
                      yaxp = NULL,
                      scatter = TRUE,
                      axes = TRUE,
                      axes.equal = FALSE,
                      axes.col = NULL,
                      pty = "s",
                      box = NULL,
                      bty = "o",
                      box.col = NULL,
                      box.alpha = 1,
                      box.lty = 1,
                      box.lwd = 1.5,
                      grid = FALSE,
                      grid.nx = NULL,
                      grid.ny = NULL,
                      grid.col = NULL,
                      grid.alpha = 1,
                      grid.lty = 1,
                      grid.lwd = 1.5,
                      bg = NULL,
                      plot.bg = NULL,
                      annotation = NULL,
                      annotation.col = NULL,
                      tick.col = NULL,
                      tck = .015, # R default is -.01
                      x.axis.side = 1,
                      y.axis.side = 2,
                      x.axis.at = NULL,
                      y.axis.at = NULL,
                      x.axis.labs = TRUE,
                      y.axis.labs = TRUE,
                      x.axis.padj = -1.1,
                      xlab.line = 1.3,
                      y.axis.padj = .9,
                      ylab.line = 1.6,
                      xlab.adj = .5,
                      ylab.adj = .5,
                      mar = c(2.5, 2.5, 1.5, 1), # c(3, 3, 3, 1),
                      col = NULL,
                      pch = ifelse(is.null(point.bg.col), 16, 21),
                      point.cex = .85,
                      # point.size = 1,
                      # point.inches = NULL,
                      point.col = NULL,
                      # point.fg.col = NULL,
                      point.bg.col = NULL,
                      point.alpha = .66,
                      line.col = NULL,
                      line.alpha = .66,
                      lty = 1,
                      lwd = 2,
                      marker.col = NULL,
                      marker.alpha = .66,
                      error.x.col = NULL,
                      error.y.col = NULL,
                      error.x.lty = 1,
                      error.y.lty = 1,
                      error.x.lwd = 1,
                      error.y.lwd = 1,
                      error.arrow.code = 3,
                      fit.col = NULL, # "#01256E" "#95001A",
                      fit.lwd = 2.5,
                      fit.alpha = .66,
                      fit.legend = ifelse(is.null(fit), FALSE, TRUE),
                      se.lty = "poly",
                      se.lwd = 1,
                      se.col = NULL,
                      se.alpha = .5,
                      se.times = 2,
                      se.border = FALSE,
                      se.density = NULL,
                      xy.fit = NULL,
                      xy.fit.col = NULL,
                      firstpc = FALSE,
                      firstpc.lty = 3,
                      firstpc.col = NULL,
                      hline = NULL,
                      hline.col = NULL,
                      hline.lwd = 1.5,
                      hline.lty = 3,
                      vline = NULL,
                      vline.lwd = 1.5,
                      vline.col = "blue",
                      vline.lty = 3,
                      diagonal = FALSE,
                      diagonal.inv = FALSE,
                      diagonal.lwd = 1.5,
                      diagonal.lty = 1,
                      diagonal.col = NULL,
                      diagonal.alpha = .5,
                      group.legend = NULL,
                      group.title = NULL,
                      group.names = NULL,
                      group.side = 3,
                      group.adj = .02,
                      group.padj = 2,
                      group.at = NA,
                      fit.legend.col = NULL,
                      fit.legend.side = 3,
                      fit.legend.adj = .02,
                      fit.legend.padj = 2,
                      fit.legend.at = NA,
                      labs.col = NULL,
                      na.rm = TRUE,
                      theme = getOption("rt.theme", "lightgrid"),
                      palette = getOption("rt.palette", "ucsfPalette"),
                      zero.lines = NULL,
                      zero.col = NULL,
                      zero.alpha = 1,
                      zero.lty = 1,
                      zero.lwd = 1.5,
                      do.reorder = NULL,
                      alpha.off = FALSE,
                      autolabel = letters,
                      new = FALSE,
                      set.par = TRUE,
                      par.reset = TRUE,
                      return.lims = FALSE,
                      pdf.width = 6,
                      pdf.height = 6,
                      verbose = FALSE,
                      filename = NULL, ...) {

  # [ ARGUMENTS ] ====
  if (is.null(type)) type <- "p"
  if (!is.null(fit)) if (fit == "none") fit <- NULL # easier to work with shiny
  if (is.logical(fit)) if (fit) fit <- "GAM"
  if (is.null(fit)) se.fit <- FALSE
  if (!is.null(fit)) fit <- toupper(fit)
  if (is.character(palette)) palette <- rtPalette(palette)
  if (!is.null(data)) {
    if (missing(x)) x <- "x"
    if (missing(y)) y <- "y"
    .xname <- deparse(substitute(x))
    .yname <- deparse(substitute(y))
    if (is.null(xlab)) xlab <- .xname
    if (is.null(ylab)) ylab  <- .yname
    x <- data[[.xname]]
    y <- data[[.yname]]
    if (!is.null(group)) group <- data[[deparse(substitute(group))]]
  }

  # fit & formula
  if (!is.null(formula)) fit <- "NLS"

  # rsq
  if (is.null(rsq)) {
    rsq <- if (!is.null(fit)) TRUE else FALSE
  }
  if (is.logical(rsq)) if (!rsq) rsq.pval <- FALSE
  if (fit.error[1] != FALSE) rsq <- rsq.pval <- FALSE
  if (is.null(rsq.pval)) {
    if (!is.null(fit)) {
      rsq.pval <- TRUE
      rsq <- FALSE
    } else {
      rsq.pval <- rsq <- FALSE
    }
  }
  # if ((rsq | rsq.pval) & is.null(fit)) fit <- "gam"
  if (rsq.pval) {
    if (!fit %in% c("LM", "GLM", "GAM")) {
      rsq.pval <- FALSE
      rsq <- TRUE
    }
  }

  # xlab & ylab
  # The gsubs remove all text up to and including a "$" symbol if present
  if (is.null(xlab)) {
    if (is.list(x)) xlab <- "x" else xlab <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  }
  if (!is.null(y) & is.null(ylab)) {
    if (is.list(y)) ylab <- "y" else ylab <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  }

  # S.E. available only for LM, LOESS, GAM, and NW
  if (se.fit) {
    if (fit != "LM" & toupper(fit) != "LOESS" & toupper(fit) != "GAM" & toupper(fit) != "NW") {
      warning(paste("S.E. not available for", toupper(fit), "- try LM, LOESS, GAM, or NW"))
      se.fit <- FALSE
    }
  }

  # For devices that do not support alpha
  if (alpha.off) {
    # line.alpha = 1
    point.alpha <- 1
    se.lty <- 3
  }

  # Grid
  if (!is.null(grid.col) & (!grid)) grid <- TRUE

  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # Reorder
  do.reorder <- NULL # remind me why this is an option
  if (is.null(do.reorder)) {
    do.reorder <- if (!is.null(fit) | "l" %in% type) TRUE else FALSE
  }

  # [ CLUSTER ] ====
  if (!is.null(cluster)) {
    group <- suppressWarnings(do.call(clustSelect(cluster),
                                      c(list(x = data.frame(x, y),
                                             verbose = verbose),
                                        cluster.params))$clusters.train)
  }

  # [ DATA ] ====
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group)
    y <- split(y, group)
    if (is.null(group.names)) group.names <- levels(group)
    names(x) <- names(y) <- group.names
  }

  # Try to get group names from list or data frame inputs
  if (is.list(y) | NCOL(y) > 1) {
    if (is.null(group.names) & !is.null(names(y))) group.names <- names(y)
  }
  if (is.list(x) | NCOL(x) > 1) {
    if (is.null(group.names) & !is.null(names(x))) group.names <- names(x)
  }

  # Convert everything to lists
  xl <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  yl <- if (!is.null(y) & !is.list(y)) as.list(as.data.frame(y)) else y
  Nxgroups <- length(xl)
  Nygroups <- length(yl)
  if (Nxgroups == 1 & Nygroups > 1) xl <- rep(xl, Nygroups)
  if (!is.null(y)) if (Nygroups == 1 & Nxgroups > 1) yl <- rep(yl, Nxgroups)
  Nxgroups <- length(xl)
  Nygroups <- length(yl)

  if (do.reorder) {
    index <- lapply(xl, order)
  } else {
    index <- lapply(xl, function(i) seq(i))
  }

  xl <- lapply(seq(xl), function(i) xl[[i]][index[[i]]])
  yl <- lapply(seq(xl), function(i) yl[[i]][index[[i]]])

  if (length(type) == 1) {
    if (type == "p") marker.col <- point.col
    if (type == "l") marker.col <- line.col
  }
  if (length(type) < Nygroups) type <- as.list(rep(type, Nygroups / length(type)))

  # point size
  #   point.size <- list(point.size)
  #   if (length(point.size) < Nxgroups) point.size <- rep(point.size, Nxgroups/ length(point.size))

  # Group names
  if (!is.null(group.names)) {
    group.names <- c(group.title, group.names)
  } else {
    if (!is.null(names(xl))) {
      group.names <- c(group.title, names(xl))
    } else {
      group.names <- c(group.title, paste(" ", toupper(letters[seq(Nxgroups)])) )
    }
  }
  if (length(lty) < Nygroups) lty <- as.list(rep(lty, Nygroups / length(lty)))
  if (length(pch) < Nygroups) pch <- as.list(rep(pch, Nygroups / length(pch)))

  # Error ====
  if (!is.null(error.x)) {
    if (!is.list(error.x)) error.x <- list(error.x)
    if (length(error.x) < Nxgroups) stop("error.x list is not same length as input data")
    error.x <- lapply(seq(error.x), function(i) error.x[[i]][index[[i]]])
  }
  if (!is.null(error.y)) {
    if (!is.list(error.y)) error.y <- list(error.y)
    if (length(error.y) < Nxgroups) stop("error.y list is not same length as input data")
    error.y <- lapply(seq(error.y), function(i) error.y[[i]][index[[i]]])
  }

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(Nxgroups > 1, TRUE, FALSE)

  # Scale point size by cex
  # if (is.null(point.inches)) point.inches <- cex * 1/20

  # [ THEMES ] ====
  # Defaults for all themes
  if (is.null(grid.lty)) grid.lty <- 1
  if (is.null(grid.lwd)) grid.lwd <- 1

  # if (length(theme) > 1) theme <- "light"
  if (theme %in% c("lightgrid", "darkgrid")) {
    if (is.null(grid.lty)) grid.lty <- 1
    if (is.null(zero.lty)) zero.lty <- 1
    if (is.null(grid.lwd)) grid.lwd <- 1.5
    if (new) theme <- substr(theme, 1, nchar(theme) - 4) # if adding to existing plot, force light or dark
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

  if (theme == "light") {
    if (is.null(bg)) bg <- "white"
    if (is.null(col) & Nxgroups == 1) {
      # col <- as.list(adjustcolor("black", alpha.f = point.alpha))
      col <- list("gray30")
    }
    # box.col <- "white"
    if (is.null(axes.col)) axes.col <- adjustcolor("white", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray10"
    if (is.null(labs.col)) labs.col <- "gray10"
    if (is.null(main.col)) main.col <- "black"
    if (is.null(grid.col)) grid.col <- "black"
    if (is.null(diagonal.col)) diagonal.col <- "black"
    if (is.null(hline.col)) hline.col <- "black"
    if (is.null(fit.error.col)) fit.error.col <- "gray30"
    gen.col <- "black"
  } else if (theme == "dark") {
    if (is.null(bg)) bg <- "black"
    if (is.null(col) & Nxgroups == 1) {
      # col <- as.list(adjustcolor("white", alpha.f = point.alpha))
      col <- list("gray70")
    }
    # box.col <- "black"
    if (is.null(axes.col)) axes.col <- adjustcolor("black", alpha.f = 0)
    if (is.null(tick.col)) tick.col <- "gray90"
    if (is.null(labs.col)) labs.col <- "gray90"
    if (is.null(main.col)) main.col <- "white"
    if (is.null(grid.col)) grid.col <- "white"
    if (is.null(diagonal.col)) diagonal.col <- "white"
    if (is.null(hline.col)) hline.col <- "white"
    if (is.null(fit.error.col)) fit.error.col <- "gray70"
    gen.col <- "white"
  } else if (theme == "lightbox") {
    if (is.null(bg)) bg <- "white"
    if (is.null(col) & Nxgroups == 1) {
      # col <- as.list(adjustcolor("black", alpha.f = point.alpha))
      col <- list("gray30")
    }
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
    if (is.null(col) & Nxgroups == 1) {
      # col <- as.list(adjustcolor("white", alpha.f = point.alpha))
      col <- list("gray70")
    }
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

  # [ COLORS ] ====
  # Point colors
  # if (is.null(col)) col <- ucsfPalette

  # if (length(col) < Nygroups) col <- as.list(rep(col, Nygroups / length(col)))
  # marker.col <- lapply(col, function(x) adjustcolor(x, point.alpha)) # marker = points and/or lines

  ### Point and Line colors
  if (all(type == "p")) {
    # ONLY POINTS
    if (length(type) == 1) {
      # SINGLE XY PAIR
      if (is.null(point.col)) {
        if (is.null(fit)) point.col <- palette[1] else point.col <- col
      } else if (length(point.col) > 1) {
        if (length(point.col) < length(xl[[1]])) {
          point.col <- rep(point.col, length(xl[[1]]/length(point.col)))
        }
        # Proper ordering if point.col is a vector
        point.col <- point.col[index[[1]]]
      }
      marker.col <- list(adjustcolor(point.col, point.alpha))
      # since points expects marker.col to be list, the above allows multicolor points when single group
    } else {
      # MULTIPLE XY PAIRS
      if (is.null(point.col)) point.col <- palette
      marker.col <- lapply(point.col, function(x) adjustcolor(x, point.alpha))
      if (!is.null(point.bg.col)) {
        # point.bg.col <- as.list(point.bg.col)
        # if (length(point.bg.col) < Nxgroups) {
        #   point.bg.col <- as.list(rep(point.bg.col, Nxgroups / length(point.bg.col)))
        # }
        if (length(point.bg.col) == sum(sapply(xl, length))) {
          point.bg.col <- split(point.bg.col, group)
        } else {
          point.bg.col <- as.list(point.bg.col)
          if (length(point.bg.col) < Nxgroups) {
            point.bg.col <- as.list(rep(point.bg.col, Nxgroups / length(point.bg.col)))
          }
        }
      }
    }
  } else if (all(type == "l")) {
    # ONLY LINES
    if (length(type) == 1) {
      # SINGLE LINE
      if (is.null(line.col)) {
        if (is.null(fit)) line.col <- palette[1] else line.col <- col
      }
      marker.col <- adjustcolor(line.col, line.alpha)
    } else {
      # MULTIPLE LINES
      if (is.null(line.col)) line.col <- palette[seql(palette, seq(Nxgroups))]
      marker.col <- lapply(line.col, function(x) adjustcolor(x, line.alpha))
    }
  } else {
    # MIXED POINTS AND LINE TYPES
    if (is.null(marker.col)) marker.col <- palette
    if (length(marker.alpha) < Nxgroups) marker.alpha <- rep(marker.alpha, Nxgroups / length(marker.alpha))
    # marker.col <- lapply(marker.col, function(x) adjustcolor(x, marker.alpha))
    marker.col <- lapply(1:Nxgroups, function(i) adjustcolor(marker.col[[i]], marker.alpha[[i]]))
  }

  # marker.col <- marker.col[seql(marker.col, Nxgroups)]

  ### Fit & SE color
  if (!is.null(fit)) {
    if (is.null(fit.col)) {
      if (Nxgroups == 1) fit.col <- ucsfCol$teal else fit.col <- palette
    }
  }
  if (se.fit) {
    if (is.null(se.col)) se.col <- fit.col
    se.col <- lapply(se.col, function(x) adjustcolor(x, se.alpha))
  }
  # Add alpha
  fit.col <- lapply(fit.col, function(x) adjustcolor(x, fit.alpha))
  se.col <- lapply(se.col, function(x) adjustcolor(x, se.alpha))

  ### XY fit color (we're still doing this, apparently)
  if (is.null(xy.fit.col)) xy.fit.col <- rev(palette)

  if (is.null(error.x.col)) {
    error.x.col <- marker.col
  } else {
    # if (!is.list(error.x.col)) error.x.col <- as.list(error.x.col)
    # if (length(error.x.col) < Nxgroups) error.x.col <- rep(error.x.col, Nxgroups/length(error.x.col))
    error.x.col <- error.x.col[seql(error.x.col, Nxgroups)]
  }

  if (is.null(error.y.col)) {
    error.y.col <- marker.col
  } else {
    # if (!is.list(error.y.col)) error.y.col <- as.list(error.y.col)
    # if (length(error.y.col) < Nygroups) error.y.col <- rep(error.y.col, Nygroups/length(error.y.col))
    error.y.col <- error.y.col[seql(error.y.col, Nygroups)]
  }

  # [ VALUES: FIT LINE & SE ] ====
  # If plotting se bands, need to include (fitted +/- se.times * se) in the axis limits
  if (se.fit) sel <- list() else sel <- NULL
  if (rsq) rsql <- list() else rsql <- NULL
  if (rsq.pval) rsqpl <- list() else rsqpl <- NULL
  if (!is.null(fit)) {
    learner <- modSelect(fit, fn = FALSE)
    fitted <- list()
    for (i in 1:Nxgroups) {
      x <- xl[[i]]
      y <- yl[[i]]
      # mod <- learner(x, y, verbose = verbose, print.plot = FALSE, ...)
      learner.args <- c(list(x = x, y = y, verbose = verbose),
                        mod.params,
                        list(...))
      if (learner == "s.NLS") learner.args <- c(learner.args,
                                                list(formula = formula, save.func = TRUE))
      mod <- do.call(learner, learner.args)
      fitted[[i]] <- fitted(mod)
      if (se.fit) sel[[i]] <- rtemis::se(mod)
      if (rsq) rsql[[i]] <- mod$error.train$Rsq
      if (rsq.pval) {
        if (fit  %in% c("LM", "GLM")) {
          rsqpl[[i]] <- paste0(ddSci(mod$error.train$Rsq), " (",
                               ddSci(summary(mod$mod)$coefficients[2, 4]), ")")
        } else if (fit == "GAM") {
          rsqpl[[i]] <- paste0(ddSci(mod$error.train$Rsq), " (",
                               ddSci(summary(mod$mod)$s.pv), ")")
        }
      }
    }
  }

  # [ VALUES: XY FIT AND SE ] ====
  if (!is.null(xy.fit)) {
    learner <- modSelect(xy.fit, TRUE)
    xy.fitted <- list()
    xy.se <- list()
    xy.index <- list()
    for (i in 1:Nxgroups) {
      xy.index[[i]] <- order(yl[[i]])
      x <- xl[[i]][xy.index[[i]]]
      y <- yl[[i]][xy.index[[i]]]
      mod <- learner(y, x, verbose = verbose, print.plot = FALSE, ...)
      xy.fitted[[i]] <- fitted(mod)
      if (se.fit) xy.se[[i]] <- se(mod)
    }
  } # End XY fit and SE

  # [ AXES LIMITS ] ====
  if (axes.equal & is.null(xlim)) xlim <- ylim <- range(xl, yl, na.rm = na.rm)
  if (is.null(xlim)) xlim <- range(xl, na.rm = na.rm)
  if (is.null(ylim)) ylim <- range(yl, na.rm = na.rm)
  if (is.list(fitted) & !is.list(sel)) {
    ylim.hi <- max(unlist(fitted))
    ylim.lo <- min(unlist(fitted))
    ylim <- range(ylim.lo, ylim.hi, yl, na.rm = na.rm)
  }
  if (is.list(sel)) {
    ylim.hi <- max(unlist(lapply(seq(length(fitted)),
                                 function(i) as.data.frame(fitted[[i]]) +
                                   se.times * as.data.frame(sel[[i]]))), na.rm = na.rm)
    ylim.lo <- min(unlist(lapply(seq(length(fitted)),
                                 function(i) as.data.frame(fitted[[i]]) -
                                   se.times * as.data.frame(sel[[i]]))), na.rm = na.rm)
    ylim <- range(ylim.lo, ylim.hi, yl, na.rm = na.rm)
    if (axes.equal) xlim <- ylim <- range(xl, ylim, na.rm = na.rm)
  }

  if (is.list(error.x)) {
    error.x.hi <- lapply(seq(xl), function(i) xl[[i]] + error.x[[i]])
    error.x.lo <- lapply(seq(xl), function(i) xl[[i]] - error.x[[i]])
    xlim <- range(error.x.lo, error.x.hi, xlim)
  }
  if (is.list(error.y)) {
    error.y.hi <- lapply(seq(yl), function(i) yl[[i]] + error.y[[i]])
    error.y.lo <- lapply(seq(yl), function(i) yl[[i]] - error.y[[i]])
    ylim <- range(error.y.lo, error.y.hi, ylim)
  }
  if (axes.equal) xlim <- ylim <- range(xlim, ylim)

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  if (set.par) par(bg = bg, cex = cex, pty = pty, new = new, mar = mar) # tck = -.02
  plot(NULL, NULL, xlim = xlim, ylim = ylim,
       ann = FALSE,
       axes = FALSE, xaxs = xaxs, yaxs = yaxs,
       xaxp = xaxp, yaxp = yaxp)

  # For rect only: must be AFTER plot(): Adjusted xlim if xaxs = "r"
  if (xaxs == "r") xlim <- c(min(xlim) - .04 * diff(range(xlim)), max(xlim) + .04 * diff(range(xlim)))
  if (yaxs == "r") ylim <- c(min(ylim) - .04 * diff(range(ylim)), max(ylim) + .04 * diff(range(ylim)))

  # [ PLOT BG ] ====
  if (!is.null(plot.bg)) {
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = plot.bg)
  }

  # [ AXES ] ====
  # axis: col: color of the axis line; col.axis: color of the tick labels; col.ticks: color of the ticks themselves
  if (axes) {
    axis(side = x.axis.side, at = x.axis.at, labels = x.axis.labs, col = axes.col,
         col.axis = labs.col, col.ticks = tick.col, padj = x.axis.padj, tck = tck)
    axis(side = y.axis.side, at = y.axis.at, labels = y.axis.labs, col = axes.col, col.axis = labs.col,
         col.ticks = tick.col, padj = y.axis.padj, tck = tck)
    mtext(xlab, side = 1, line = xlab.line, cex = cex, adj = xlab.adj, col = labs.col)
    mtext(ylab, side = 2, line = ylab.line, cex = cex, adj = ylab.adj, col = labs.col)
  }

  # [ GRID ] ====
  grid.col <- colorAdjust(grid.col, grid.alpha)
  if (grid) grid(nx = grid.nx,
                 ny = grid.ny,
                 col = grid.col,
                 lty = grid.lty,
                 lwd = grid.lwd)

  # [ ZERO LINES ] ====
  if (is.null(zero.lines)) {
    if (theme %in% c("light", "dark")) zero.lines <- TRUE else zero.lines <- FALSE
  }

  if (zero.lines) {
    if (is.null(zero.col)) {
      zero.col <- "gray50"
    }
    zero.col <- adjustcolor(zero.col, zero.alpha)
    if (ylim[1] <= 0 & 0 <= ylim[2]) abline(h = 0, lwd = zero.lwd, col = zero.col, lty = zero.lty)
    if (xlim[1] <= 0 & 0 <= xlim[2]) abline(v = 0, lwd = zero.lwd, col = zero.col, lty = zero.lty)
  }

  # [ DIAGONAL ] ====
  if (diagonal) abline(0, 1, lwd = diagonal.lwd, lty = diagonal.lty,
                       col = adjustcolor(diagonal.col, diagonal.alpha))
  if (diagonal.inv) abline(1, -1, lwd = diagonal.lwd, lty = diagonal.lty,
                           col = adjustcolor(diagonal.col, diagonal.alpha))

  # [ BOX ] ====
  if (is.null(box)) {
    if (theme %in% c("lightbox", "darkbox")) box <- TRUE else box <- FALSE
  }
  if (box) {
    if (is.null(box.col)) {
      if (theme %in% c("light", "lightbox")) box.col <- "gray40" else box.col <- "gray60"
    }
    box.col <- adjustcolor(box.col, box.alpha)
    graphics::box(col = box.col, lty = box.lty, lwd = box.lwd, bty = bty)
  }

  # [ POINTS AND LINES ] ====
  # return(point.size)
  #   for (i in 1:Nxgroups) {
  #     if (type[[i]] == "p") {
  #       symbols(xl[[i]], yl[[i]], circles = point.size[[i]], # rep(point.size[[i]], length(xl[[i]]) / length(size[[i]])),
  #               inches = point.inches, fg = point.fg.col, bg = marker.col[[i]], add = T)
  #       # inches = point.inches, fg = marker.col[[i]], bg = marker.col[[i]], add = T)
  #       # if (is.null(main)) main <- paste(ylab, "vs.", xlab)
  #     } else {
  #       lines(xl[[i]], yl[[i]], col = marker.col[[i]], lwd = lwd)
  #     }
  #   }

  if (scatter) {

    if (!is.null(error.y)) {
      for (i in seq(error.y)) {
        suppressWarnings(arrows(xl[[i]], yl[[i]] - error.y[[i]],
                                xl[[i]], yl[[i]] + error.y[[i]],
                                lty = error.y.lty,
                                lwd = error.y.lwd, angle = 90, code = error.arrow.code,
                                length = 0.05, col = error.y.col[[i]]))
      }
    }

    if (!is.null(error.x)) {
      for (i in seq(error.x)) {
        suppressWarnings(arrows(xl[[i]] - error.x[[i]], yl[[i]],
                                xl[[i]] + error.x[[i]], yl[[i]],
                                lty = error.x.lty,
                                lwd = error.x.lwd, angle = 90, code = error.arrow.code,
                                length = 0.05, col = error.x.col[[i]]))
      }
    }

    for (i in 1:Nxgroups) {
      points(xl[[i]], yl[[i]],
             type = type[[i]],
             col = marker.col[[i]],
             bg = point.bg.col[[i]],
             lwd = lwd,
             pch = pch[[i]],
             cex = point.cex,
             lty = lty[[i]], xpd = xpd) # delta
    }

  }

  # [ MAIN TITLE ] ====
  if (exists("autolabel", envir = rtenv)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (!is.null(main)) {
    mtext(main, line = main.line, font = main.font, family = main.family,
          adj = main.adj, cex = cex, col = main.col)
  }

  # [ S.E. SHADING ] ====
  if (se.fit & is.list(sel)) {
    for (i in 1:Nxgroups) {
      if (se.lty == "poly") {
        polygon(c(xl[[i]], rev(xl[[i]])),
                c(fitted[[i]] + se.times * sel[[i]], rev(fitted[[i]] - se.times * sel[[i]])),
                col = se.col[[i]],
                density = se.density, border = se.border)
      } else {
        lines(xl[[i]], fitted[[i]] + se.times * sel[[i]], lty = se.lty,
              col = se.col[[i]], lwd = se.lwd)
        lines(xl[[i]], fitted[[i]] - se.times * sel[[i]], lty = se.lty,
              col = se.col[[i]], lwd = se.lwd)
      }
    }
  }

  # [ FIT LINE ] ====
  if (is.null(fit) & is.null(xy.fit) & !firstpc) fit.legend <- FALSE
  if (group.legend) fit.legend <- FALSE

  if (!is.null(fit)) {
    for (i in 1:Nxgroups) {
      lines(xl[[i]], fitted[[i]], col = fit.col[[i]], lwd = fit.lwd, lty = lty[[i]])
    }
    #     if (fit.legend) {
    #       legend.fit <- ifelse(!is.null(xy.fit), paste0(fit, ": y ~ x"), paste(toupper(fit), "fit"))
    #     }
  }

  #orig fit.legend: padj = seq(2, 2 + 1.5 * (Nxgroups - 1), 1.5))


  # [ FIT LEGEND ] ====
  fit.legend.n <- 0
  if (fit.legend) {
    if (is.null(fit.legend.col)) fit.legend.col <- adjustcolor(fit.col[[1]], 2)
    if (fit == "NLS") {
      fit.text <- mod$extra$model
    } else if (fit == "NLA") {
      fit.text <- mod$mod$formula
    } else {
      fit.text <- fit
    }
    mtext(fit.text,
          col = fit.legend.col,
          side = fit.legend.side,
          adj = fit.legend.adj,
          at = fit.legend.at,
          cex = cex,
          padj = fit.legend.padj)
    fit.legend.n <- 1
  }

  # [ XY FIT LINE ] ====
  xy.fit.legend <- NULL
  if (!is.null(xy.fit)) {
    if (is.null(xy.fit.col)) xy.fit.col <- colorOp(col, "invert")
    for (i in 1:Nxgroups) {
      lines(xy.fitted[[i]], yl[[i]][xy.index[[i]]], col = xy.fit.col[[i]], lwd = lwd, lty = lty[[i]])
    }
    if (fit.legend) {
      xy.fit.legend <- paste0(toupper(fit), ": x ~ y")
    }
  }
  if (fit.legend) {
    mtext(xy.fit.legend,
          col = xy.fit.col[[1]],
          side = fit.legend.side,
          adj = fit.legend.adj,
          at = fit.legend.at,
          cex = cex,
          padj = fit.legend.padj + fit.legend.n * 1.4)
    fit.legend.n <- fit.legend.n + 1
  }

  # [ 1ST PC AXIS ] ====
  firstpc.legend <- NULL
  if (firstpc & is.null(firstpc.col) & !is.null(fit) & !is.null(xy.fit)) {
    firstpc.col <- list()
    for (i in 1:Nxgroups) {
      firstpc.col[[i]] <- colorOp(c(col[[i]], xy.fit.col[[i]]), "mean")
    }
  } else if (firstpc & is.null(firstpc.col)) firstpc.col <- as.list(rep("orange", Nxgroups))

  if (firstpc) {
    pc <- list()
    for (i in 1:Nxgroups) {
      pc[[i]] <- prcomp(data.frame(x = xl[[i]], y = yl[[i]]), retx = TRUE,
                        scale = FALSE, center = TRUE)
      slope1 <- pc[[i]]$rotation[2, 1] / pc[[i]]$rotation[1, 1]
      # slope2 <- pc[[i]]$rotation[2, 2] / pc[[i]]$rotation[1, 2]
      px <- pc[[i]]$center[1]
      py <- pc[[i]]$center[2]
      px <- c(px, px + 1)
      py <- c(py, py + slope1)
      abline(lm(py ~ px), lwd = lwd, col = firstpc.col[[i]], lty = firstpc.lty)
    }
    if (fit.legend) {
      firstpc.legend <- "1st PC axis"
      mtext(firstpc.legend,
            col = firstpc.col[[1]],
            side = fit.legend.side,
            adj = fit.legend.adj,
            at = fit.legend.at,
            cex = cex,
            padj = fit.legend.padj + fit.legend.n * 1.4)
      fit.legend.n <- fit.legend.n + 1
    }
  }

  # [ GROUP LEGEND ] ====
  if (group.legend) {
    group.col <-  unlist(adjustcolor(marker.col[1:Nxgroups], 2))
    if (!is.null(group.title)) group.col <- c(gen.col, group.col)
    mtext(group.names,
          col = group.col, # fit.col no good if fit is null
          side = group.side,
          adj = group.adj,
          at = group.at,
          cex = cex,
          xpd = xpd,
          padj = seq(group.padj, group.padj + 1.5 * (length(group.names) - 1), 1.5))
  }

  # [ FIT ERROR ANNOT ] ====
  myerror <- NULL
  if (is.logical(fit.error)) {
    if (fit.error) {
      myerror <- lapply(1:Nxgroups, function(i) modError(xl[[i]], yl[[i]]))
    }
  } else {
    if (class(fit.error)[1] == "modError") myerror <- list(fit.error)
  }
  if (!is.null(myerror)) {
    if (Nxgroups == 1) {
      annot.n <- paste0("n = ", length(x))
      annot.mse <- paste0("MSE = ", ddSci(myerror[[1]]$MSE))
      # annot.mse <- paste0("MSE = ", ddSci(myerror[[1]]$MSE), " (", round(myerror[[1]]$Rsq * 100), "%)")
      annot.Rsq <- bquote("R"^"2" ~ "=" ~ .(ddSci(myerror[[1]]$Rsq)))
      # annot.r <- paste0("r = ", ddSci(myerror[[1]]$r), "; p = ", ddSci(myerror[[1]]$r.p))
      # annot.rho <- paste0("rho = ", ddSci(myerror[[1]]$rho), "; p = ", ddSci(myerror[[1]]$rho.p))
      mtext(annot.n, fit.error.side,
            line = -3.3,
            adj = .98, padj = fit.error.padj,
            cex = cex, col = fit.error.col)
      mtext(annot.mse, fit.error.side,
            line = -2.2,
            adj = .98, padj = fit.error.padj,
            cex = cex, col = fit.error.col)
      mtext(annot.Rsq, fit.error.side,
            line = -1.1,
            adj = .98, padj = fit.error.padj,
            cex = cex, col = fit.error.col)
    } else {
      error.annot <- sapply(seq(Nxgroups), function(i) paste0(ddSci(myerror[[i]]$MSE),
                                                              "; ", ddSci(myerror[[i]]$Rsq)))
      # mtext(c("MSE; Rsq", error.annot),
      mtext(
        c(expression("MSE; R"^2), error.annot),
        fit.error.side,
        adj = .98, cex = cex,
        col = c(gen.col, unlist(fit.col[seq(Nxgroups)])),
        line = rev(seq(-1.1, -Nxgroups * 1.1 - 1.1, -1.1)))
        # line = rev(seq(-1, -Nxgroups - 1)))
    }
  }


  # [ R-SQUARED ] ====
  if (rsq) {
    if (is.null(rsq.col)) rsq.col <- c(unlist(adjustcolor(fit.col[1:Nxgroups], 2)))
    annot.rsq <- ddSci(unlist(rsql))
    # padj_reg <- -3.5 - (Nxgroups - 1) * 1.5 + rsq.padj.shift
    # padj_exp <- -3.5 - (Nxgroups - 1) * 1.5 + rsq.padj.shift + 1.2
    mtext(expression("R"^2),
          side = rsq.side,
          adj = rsq.adj,
          line = -1 - Nxgroups,
          cex = cex,
          col = gen.col)
    mtext(rev(annot.rsq),
          side = rsq.side,
          adj = rsq.adj,
          line = seq(-1, -Nxgroups, -1),
          cex = cex,
          col = rev(unlist(rsq.col)))
  }

  if (rsq.pval) {
    if (is.null(rsq.col)) rsq.col <- c(gen.col, unlist(adjustcolor(fit.col[1:Nxgroups], 2)))
    annot.rsq <- c("R-sq (p-val)", unlist(rsqpl))
    mtext(rev(annot.rsq),
          side = rsq.side,
          adj = .98,
          padj = seq(-2.2, -2.2 - 1.5 * length(rsqpl), -1.5 ),
          # seq(-1.4, (length(rsqpl) + 1) * (-1.4), -1 ),
          cex = cex,
          col = unlist(rev(rsq.col)))
  }

  # [ ANNOTATION ] ====
  if (!is.null(annotation)) {
    if (is.null(annotation.col)) annotation.col <- col[[1]]
    mtext(annotation, 1, -1.5, adj = .97, cex = cex, col = annotation.col)
  }

  # [ HLINE & VLINE ] ====
  if (!is.null(hline)) abline(h = hline, lwd = hline.lwd, col = hline.col, lty = hline.lty)
  if (!is.null(vline)) abline(v = vline, lwd = vline.lwd, col = vline.col, lty = vline.lty)

  # [ OUTRO ] ====
  if (!is.null(filename)) dev.off()
  if (return.lims) return(list(xlim = xlim, ylim = ylim))

} # rtemis::mplot3.xy


#' \code{mplot3}: True vs. Fitted plot
#'
#' An \code{mplot3.xy} alias with defaults for plotting a learner's performance
#'
#' @inheritParams mplot3.xy
#' @param x Vector, numeric / factor / survival for regression, classification, survival: True values
#' @param y Vector, numeric / factor / survival for regression, classification, survival: Predicted values
#' @param ... Additional argument to be passed to \link{mplot3.conf} (classification) or \link{mplot3.xy} (regression)
#' @export

mplot3.fit <- function(x, y,
                       fit = "lm",
                       se.fit = TRUE,
                       fit.error = TRUE,
                       axes.equal = TRUE,
                       diagonal = TRUE,
                       theme = getOption("rt.fit.theme", "lightgrid"),
                       col = NULL,
                       zero.lines = FALSE,
                       fit.legend = FALSE, ...) {

  type <- if (is.factor(y)) {
    "Classification"
  } else if (survival::is.Surv(y)) {
    "Survival"
  } else {
    "Regression"
  }
  if (type == "Classification") {
    mplot3.conf(table(y, x), ...)
  } else if (type == "Survival") {
    msg("Not currently supported")
  } else {
    if (is.null(col)) {
      mplot3.xy(x, y,
                fit = fit, se.fit = se.fit, fit.error = fit.error,
                axes.equal = axes.equal, diagonal = diagonal,
                theme = theme, zero.lines = zero.lines,
                fit.legend = fit.legend, ...)
    } else {
      mplot3.xy(x, y,
                fit = fit, se.fit = se.fit, fit.error = fit.error,
                axes.equal = axes.equal, diagonal = diagonal,
                theme = theme, zero.lines = zero.lines,
                point.col = col,
                fit.col = col,
                fit.legend = fit.legend, ...)
    }

  }

} # rtemis::mplot3.fit
