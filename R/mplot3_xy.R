# mplot3_xy.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' `mplot3`: XY Scatter and line plots
#'
#' Plot points and lines with optional fits and standard error bands
#'
#' @param x Numeric vector or list of vectors for x-axis.
#'   If `data` is provided, name of variable, unquoted.
#' @param y Numeric vector of list of vectors for y-axis
#'   If `data` is provided, name of variable, unquoted.
#' @param fit Character: \pkg{rtemis} model to calculate `y ~ x` fit.
#' Options: see `select_learn()`
#' Can also be Logical, which will give a GAM fit if TRUE. If you specify
#' "NLA", the activation function should be passed as a string.
#' @param formula Formula: Provide a formula to be solved using [s_NLS].
#' If provided, `fit` is forced to `'nls'`. e.g. y ~ b * m ^ x for a
#' power curve. Note: `nls` is powerful but is prone to errors
#' and warnings. Use single letters for parameter names, no numbers.
#' @param se.fit Logical: If TRUE, draw the standard error of the fit
#' @param fit.params List: Arguments for learner defined by `fit`.
#' Default = NULL, i.e. use default learner parameters
#' @param error.x Vector, float: Error in `x` (e.g. standard deviation)
#' will be plotted as bars around point
#' @param error.y Vector, float: Error in `y` (e.g. standard deviation)
#' will be plotted as bars around point
#' @param cluster Character: Clusterer name. Will cluster
#' `data.frame(x, y)` and pass result to `group`.
#' Run [select_clust] for options
#' @param cluster.params List: Names list of parameters to pass to the
#' `cluster` function
#' @param data (Optional) data frame, where `x` and `y` are defined
#' @param group Vector: will be converted to factor.
#'   If `data` is provided, name of variable, unquoted.
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param main Character: Plot title
#' @param se.lty How to draw the `se.fit` "poly" draws a polygon around
#' the fit line, otherwise an integer defines the lty (line type) for lines to
#' be drawn
#' @param se.col Color for `se.fit`
#' @param se.alpha Alpha for `se.fit`
#' @param se.times Draw polygon or lines at +/- `se.times` * standard
#' error of fit. Defaults to 1.96, which corresponds to 95 percent confidence interval
#' @param se.border Define border of polygon for `se.fit`.
#'   See `border` in `graphics::polygon`
#' @param se.density Density of shading line of polygon for `se.fit`.
#'   See `density` in `graphics::polygon`
#' @param type Character: "p" for points, "l" for lines, "s" for steps.
#' Default = "p". If `x` and/or `y` contains multiple
#'   vectors, `type` can be a vector, e.g. `c("p", "l", "l")` will
#' give a set of points and two sets of lines. Otherwise, `type` is
#' recycled to length of x
#' @param xlim Float vector, length 2: x-axis limits
#' @param ylim Float vector, length 2: y-axis limits
#' @param axes.equal Logical: Should axes be equal? Defaults to FALSE
#' @param pty Character: "s" gives a square plot; "m" gives a plot that fills
#' graphics device size. Default = "m" (See `par("pty")`)
#' @param fit.lwd Float: Fit line width
#' @param rsq Logical: If TRUE, add legend with R-squared (if fit is not NULL)
#' @param rsq.pval Logical: If TRUE, add legend with R-squared and its p-value,
#' if fit is not NULL
#' @param annotation Character: Add annotation at the bottom right of the plot
#' @param annotation.col Color for annotation
#' @param xlab.adj Float: `adj` for `xlab` (See `par("adj")`)
#' @param ylab.adj Float: `adj` for `ylab` (See `par("adj")`)
#' @param theme Character: Run `themes()` for available themes
#' @param mar Float, vector, length 4: Margins; see `par("mar")`
#' @param oma Float, vector, length 4: Outer margins; see `par("oma")`
#' @param group.legend Logical: If TRUE, place `group.names` in a legend
#' @param group.names (Optional) If multiple groups are plotted, use these
#' names if `group.title = TRUE`
#' @param group.title Character: Group title, shown above group names. e.g.
#' if group names are c("San Francisco", "Philadelphia"), `group.title`
#' can be "City"
#' @param new Logical: If TRUE, add plot to existing plot. See `par("new")`
#' @param xpd Logical or NA: FALSE: plotting clipped to plot region; TRUE:
#' plotting clipped to figure region; NA: plotting clipped to device region.
#' @param xaxs Character: "r": Extend plot x-axis limits by 4% on either end;
#' "i": Use exact x-axis limits.
#' @param yaxs Character: as `xaxs` for the y-axis.
#' @param log Character: "x", "y", or "xy", defines if either or both axes should
#' be log-transformed.
#' @param rsq.side Integer: \[1:4\] Where to place the `rsq` annotation.
#' Default = 1 (i.e. bottom)
#' @param rsq.adj Float: Adjust `rsq` annotation. See `mtext "adj"`
#' @param rsq.col Color: Color for `rsq` annotation. Default = NULL, which
#' results in `fit.col`
#' @param rsq.line Numeric: Passed to `mtext "line"` to place R-squared annotation.
#' @param fit.error  Logical: If TRUE: draw fit error annotation.
#' Default = NULL, which results in TRUE, if fit is set
#' @param fit.error.side Integer \[1:4\]: Which side to draw `fit.error` on.
#' @param fit.error.padj Float: See `mtext:padg` Default = NA
#' @param xaxp See `par("xaxp")`
#' @param yaxp See `par("yaxp")`
#' @param scatter Logical: If TRUE, plot (x, y) scatter points.
#' @param x.axis.at Float, vector: x coordinates to place tick marks.
#' Default = NULL, determined by `graphics::axis` aautomatically
#' @param y.axis.at As `x.axis.at` for y-axis
#' @param x.axis.labs See `axis("labels")`
#' @param y.axis.labs See `axis("labels")`
#' @param point.cex Float: Character expansion for points.
#' @param point.bg.col Color: point background
#' @param pch Integer: Point character.
#' @param line.col Color for lines
#' @param line.alpha Float \[0, 1\]: Transparency for lines
#' @param lty Integer: Line type. See `par("lty")`
#' @param lwd Float: Line width
#' @param marker.col Color for marker
#' @param marker.alpha Float \[0, 1\]: Transparency for markers
#' @param error.x.col Color for x-axis error bars
#' @param error.y.col Color for y-axis error bars
#' @param error.x.lty Integer: line type for x-axis error bars
#' @param error.y.lty Integer: line type for y-axis error bars
#' @param error.x.lwd Float: Line width for x-axis error bars
#' @param error.y.lwd Float: Line width for y-axis error bars
#' @param error.arrow.code Integer: Type of arrow to draw for error bars.
#' See `arrows("code")`
#' @param fit.col Color: Color of the fit line.
#' @param fit.alpha Float \[0, 1\]: Transparency for fit line
#' @param fit.legend Logical: If TRUE, show fit legend
#' @param se.lwd Float: Line width for standard error bounds
#' @param hline Vector: y-value(s) for horizontal lines.
#' @param hline.col Color for horizontal line(s)
#' @param hline.lwd Float: Width for horizontal line(s)
#' @param hline.lty Integer: Line type for horizontal line(s)
#' @param vline Vector: x-value(s) for vertical lines.
#' @param vline.lwd Float: Width for vertical lines
#' @param vline.col Color for vertical lines
#' @param vline.lty Integer: Line type for vertical lines
#' @param diagonal Logical: If TRUE, draw diagonal line.
#' @param diagonal.inv Logical: If TRUE, draw inverse diagonal line. Will use
#' `diagonal.lwd`, `diagonal.lty`, `diagonal.col`, `diagonal.alpha` (Note: it only
#' makes sense to use only one of `diagonal` or `diagonal.inv`)
#' @param diagonal.lwd Float: Line width for `diagonal`.
#' @param diagonal.lty Integer: Line type for `diagonal`.
#' @param diagonal.col Color: Color for `diagonal`.
#' @param diagonal.alpha Float: Alpha for `diagonal`
#' @param group.side Integer: Side to show group legend
#' @param group.adj Float: `adj` for group legend. See `mtext("adj")`
#' @param group.padj Float: `padj` for group legend See `mtext("padj")`
#' @param group.at Float: location for group legend. See `mtext("at")`
#' @param fit.legend.col Color for fit legend
#' @param fit.legend.side Integer: Side for fit legend
#' @param fit.legend.adj Float: `adj` for fit legend
#' @param fit.legend.padj Float: `padj` for fit legend
#' @param fit.legend.at Float: location for fit legend. See `mtext("at")`
#' @param rm.na Logical: If TRUE, remove all NA values pairwise between x and y.
#' Set to FALSE if you know your data has no missing values.
#' @param palette Vector of colors, or Character defining a builtin palette -
#' get options with `rtpalette()`
#' @param order.on.x Logical: If TRUE, order (x, y) by increasing x.
#' Default = NULL: will be set to TRUE if fit is set, otherwise FALSE
#' @param autolabel Character vector to be used to generate autolabels when using
#' [rtlayout] with `autolabel = TRUE`.
#' @param par.reset Logical: If TRUE, reset `par` setting before exiting.
#' @param return.lims Logical: If TRUE, return xlim and ylim.
#' @param pdf.width Float: Width in inches for pdf output (if `filename`
#' is set).
#' @param pdf.height Float: Height in inches for pdf output.
#' @param trace Integer: If > 0, pass `verbose = TRUE` to the cluster and
#' fit functions, if used.
#' @param filename Character: Path to file to save plot. Default = NULL
#' @param ... Additional arguments to be passed to theme function
#'
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' set.seed(1999)
#' x <- rnorm(500)
#' ycu <- x^3 + 12 + rnorm(500)
#' mplot3_xy(x, ycu)
#' mplot3_xy(x, ycu, fit = "gam")
#' ysq <- x^2 + 3 + rnorm(500)
#' mplot3_xy(x, list(squared = ysq, cubed = ycu), fit = "gam")
#' }
#' @export

mplot3_xy <- function(
  x,
  y = NULL,
  fit = NULL,
  formula = NULL,
  se.fit = FALSE,
  fit.params = NULL,
  error.x = NULL,
  error.y = NULL,
  cluster = NULL,
  cluster.params = list(),
  data = NULL,
  type = "p",
  group = NULL,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  xlim = NULL,
  ylim = NULL,
  xpd = TRUE,
  xaxs = "r",
  yaxs = "r",
  log = "",
  rsq = NULL,
  rsq.pval = FALSE,
  rsq.side = 1,
  rsq.adj = .98,
  rsq.col = NULL,
  rsq.line = NULL,
  fit.error = FALSE,
  fit.error.side = 1,
  fit.error.padj = NA,
  xaxp = NULL,
  yaxp = NULL,
  scatter = TRUE,
  axes.equal = FALSE,
  pty = "m", # "s" square, "m" maximal plot region
  annotation = NULL,
  annotation.col = NULL,
  x.axis.at = NULL,
  x.axis.labs = TRUE,
  y.axis.at = NULL,
  y.axis.labs = TRUE,
  xlab.adj = .5,
  ylab.adj = .5,
  mar = NULL,
  oma = rep(0, 4),
  point.cex = 1,
  point.bg.col = NULL,
  pch = ifelse(is.null(point.bg.col), 16, 21),
  line.col = NULL,
  line.alpha = .66,
  lwd = 1,
  lty = 1,
  marker.col = NULL,
  marker.alpha = NULL,
  error.x.col = NULL,
  error.y.col = NULL,
  error.x.lty = 1,
  error.y.lty = 1,
  error.x.lwd = 1,
  error.y.lwd = 1,
  error.arrow.code = 3,
  fit.col = NULL,
  fit.lwd = 2.5,
  fit.alpha = 1,
  fit.legend = ifelse(is.null(fit), FALSE, TRUE),
  se.lty = "poly",
  se.lwd = 1,
  se.col = NULL,
  se.alpha = .5,
  se.times = 1.96,
  se.border = FALSE,
  se.density = NULL,
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
  diagonal.col = "gray50",
  diagonal.alpha = 1,
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
  rm.na = TRUE,
  theme = rtTheme,
  palette = rtPalette,
  order.on.x = NULL,
  autolabel = letters,
  new = FALSE,
  par.reset = TRUE,
  return.lims = FALSE,
  pdf.width = 6,
  pdf.height = 6,
  trace = 0,
  filename = NULL,
  ...
) {
  # Arguments ----
  if (is.null(y) && NCOL(x) > 1) {
    if (is.null(xlab)) xlab <- labelify(colnames(x)[1])
    if (is.null(ylab)) ylab <- labelify(colnames(x)[2])
    y <- x[, 2]
    x <- x[, 1]
  }
  if (is.null(mar)) mar <- c(2.5, 3, 2, 1)
  if (!is.null(fit)) if (fit == "none") fit <- NULL # easier to work with shiny
  if (isTRUE(fit)) fit <- "GAM"
  if (is.null(fit)) se.fit <- FALSE
  if (!is.null(fit)) fit <- toupper(fit)
  if (is.character(palette)) palette <- rtpalette(palette)
  # Check: using string names and data
  if (!is.null(data)) {
    if (missing(x)) x <- "x"
    if (missing(y)) y <- "y"
    .xname <- deparse(substitute(x))
    .yname <- deparse(substitute(y))
    if (is.null(xlab)) xlab <- labelify(.xname)
    if (is.null(ylab)) ylab <- labelify(.yname)
    x <- data[[.xname]]
    y <- data[[.yname]]
    if (!is.null(group)) group <- data[[deparse(substitute(group))]]
  }

  # if (is.null(y.axis.padj)) {
  #   y.axis.padj <- if (y.axis.las == 1) .5 else 1
  # }
  #
  # if (is.null(y.axis.hadj)) {
  #   y.axis.hadj <- if (y.axis.las == 1) 1 else .5
  # }

  if (grepl("x", log)) xaxs <- "i"
  if (grepl("y", log)) yaxs <- "i"

  # fit & formula
  if (!is.null(formula)) fit <- "NLS"

  # rsq
  if (is.null(rsq)) {
    rsq <- if (!is.null(fit)) TRUE else FALSE
  }
  if (isFALSE(rsq)) rsq.pval <- FALSE
  if (fit.error[1] != FALSE) rsq <- rsq.pval <- FALSE
  if (is.null(rsq.pval)) {
    if (!is.null(fit)) {
      rsq.pval <- TRUE
      rsq <- FALSE
    } else {
      rsq.pval <- rsq <- FALSE
    }
  }
  if (rsq.pval) {
    if (!fit %in% c("LM", "GLM", "GAM")) {
      rsq.pval <- FALSE
      rsq <- TRUE
    }
  }

  # xlab & ylab
  # The gsubs remove all text up to and including a "$" symbol if present
  if (is.null(xlab)) {
    if (is.list(x)) xlab <- "x" else
      xlab <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  }
  if (!is.null(y) && is.null(ylab)) {
    if (is.list(y)) ylab <- "y" else
      ylab <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  }

  # S.E. available only for LM, LOESS, GAM, and NW
  if (se.fit) {
    if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
      warning(paste(
        "Standard error of the fit not available for",
        fit,
        "- try LM, LOESS, GAM, or NW"
      ))
      se.fit <- FALSE
    }
  }

  # For devices that do not support alpha
  # if (alpha.off) {
  #   point.alpha <- 1
  #   se.lty <- 3
  # }

  # Output directory
  if (!is.null(filename) && !dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }

  # Reorder
  if (!is.null(fit)) order.on.x <- TRUE
  if (is.null(order.on.x)) {
    order.on.x <- ifelse("l" %in% type, TRUE, FALSE)
  }

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # Cluster ----
  if (!is.null(cluster)) {
    group <- suppressWarnings(
      do.call(
        select_clust(cluster),
        c(
          list(
            x = data.frame(x, y),
            verbose = trace > 0
          ),
          cluster.params
        )
      )$clusters.train
    )
    group <- paste("Cluster", group)
  }

  # Data ----
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group)
    y <- split(y, group)
    if (is.null(group.names)) group.names <- levels(group)
    names(x) <- names(y) <- group.names
  }

  # Try to get group names from list or data frame inputs
  if (is.list(y) || NCOL(y) > 1) {
    if (is.null(group.names) && !is.null(names(y))) group.names <- names(y)
  }
  if (is.list(x) || NCOL(x) > 1) {
    if (is.null(group.names) && !is.null(names(x))) group.names <- names(x)
  }

  # Convert everything to lists
  xl <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  yl <- if (!is.null(y) && !is.list(y)) as.list(as.data.frame(y)) else y
  Nxgroups <- length(xl)
  Nygroups <- length(yl)
  if (Nxgroups == 1 && Nygroups > 1) xl <- rep(xl, Nygroups)
  if (!is.null(y)) if (Nygroups == 1 && Nxgroups > 1) yl <- rep(yl, Nxgroups)
  Nxgroups <- length(xl)
  Nygroups <- length(yl)

  if (rm.na) {
    indexl <- lapply(
      seq(xl),
      function(i) complete.cases(cbind(xl[[i]], yl[[i]]))
    )
    xl <- lapply(seq_along(xl), function(i) xl[[i]][indexl[[i]]])
    yl <- lapply(seq_along(yl), function(i) yl[[i]][indexl[[i]]])
  }

  if (order.on.x) {
    index <- lapply(xl, order)
  } else {
    index <- lapply(xl, function(i) seq(i))
  }

  xl <- lapply(seq(xl), function(i) xl[[i]][index[[i]]])
  yl <- lapply(seq(xl), function(i) yl[[i]][index[[i]]])

  if (length(type) < Nygroups)
    type <- as.list(rep(type, Nygroups / length(type)))

  # Group names
  if (!is.null(group.names)) {
    group.names <- c(group.title, group.names)
  } else {
    if (!is.null(names(xl))) {
      group.names <- c(group.title, names(xl))
    } else {
      group.names <- c(group.title, paste(" ", toupper(letters[seq(Nxgroups)])))
    }
  }
  if (length(lty) < Nygroups) lty <- as.list(rep(lty, Nygroups / length(lty)))
  if (length(pch) < Nygroups) pch <- as.list(rep(pch, Nygroups / length(pch)))

  # Error ----
  if (!is.null(error.x)) {
    if (!is.list(error.x)) error.x <- list(error.x)
    if (length(error.x) < Nxgroups)
      stop("error.x list is not same length as input data")
    error.x <- lapply(seq(error.x), function(i) error.x[[i]][index[[i]]])
  }
  if (!is.null(error.y)) {
    if (!is.list(error.y)) error.y <- list(error.y)
    if (length(error.y) < Nxgroups)
      stop("error.y list is not same length as input data")
    error.y <- lapply(seq(error.y), function(i) error.y[[i]][index[[i]]])
  }

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(Nxgroups > 1, TRUE, FALSE)

  # Scale point size by cex
  # if (is.null(point.inches)) point.inches <- cex * 1/20

  if (is.null(marker.alpha)) {
    marker.alpha <- autoalpha(max(lengths(xl)))
  }

  # Colors ----
  ### Point and Line colors
  if (all(type == "p")) {
    # ONLY POINTS
    if (length(type) == 1) {
      # SINGLE XY PAIR
      if (is.null(marker.col)) {
        marker.col <- if (is.null(fit)) palette[1] else theme$fg
      } else if (length(marker.col) > 1) {
        if (length(marker.col) < length(xl[[1]])) {
          marker.col <- rep(marker.col, length(xl[[1]] / length(marker.col)))
        }
        # Proper ordering if marker.col is a vector
        marker.col <- marker.col[index[[1]]]
      }
      marker.col <- list(adjustcolor(marker.col, marker.alpha))
      # since points expects marker.col to be list, the above allows multicolor points when single group
    } else {
      # MULTIPLE XY PAIRS
      if (is.null(marker.col)) marker.col <- palette
      if (length(marker.col) < Nxgroups) {
        marker.col <- as.list(rep(marker.col, Nxgroups / length(marker.col)))
      }
      # marker.col <- lapply(point.col, function(x) adjustcolor(x, point.alpha))
      if (!is.null(point.bg.col)) {
        if (length(point.bg.col) == sum(sapply(xl, length))) {
          point.bg.col <- split(point.bg.col, group)
        } else {
          point.bg.col <- as.list(point.bg.col)
          if (length(point.bg.col) < Nxgroups) {
            point.bg.col <- as.list(rep(
              point.bg.col,
              Nxgroups / length(point.bg.col)
            ))
          }
        }
      }
    }
  } else if (all(type == "l")) {
    # ONLY LINES
    if (length(type) == 1) {
      # SINGLE LINE
      if (is.null(line.col)) {
        if (is.null(fit)) line.col <- palette[[1]] else line.col <- theme$fg
      }
      marker.col <- adjustcolor(line.col, line.alpha)
    } else {
      # MULTIPLE LINES
      if (is.null(line.col)) line.col <- palette[seql(palette, seq(Nxgroups))]
      line.alpha <- recycle(line.alpha, line.col)
      marker.col <- mapply(adjustcolor, line.col, line.alpha)
      # marker.col <- lapply(line.col, function(x) adjustcolor(x, line.alpha))
    }
  } else {
    # MIXED POINTS AND LINE TYPES
    if (is.null(marker.col)) marker.col <- palette
    if (length(marker.alpha) < Nxgroups)
      marker.alpha <- rep(marker.alpha, Nxgroups / length(marker.alpha))
    marker.col <- lapply(
      seq(Nxgroups),
      function(i) adjustcolor(marker.col[[i]], marker.alpha[[i]])
    )
  }

  ### Fit & SE color
  if (!is.null(fit)) {
    if (is.null(fit.col)) {
      fit.col <- palette
    }
  }
  if (se.fit) {
    if (is.null(se.col)) se.col <- fit.col
    se.col <- lapply(se.col, function(x) adjustcolor(x, se.alpha))
  }
  # Add alpha
  fit.col <- lapply(fit.col, function(x) adjustcolor(x, fit.alpha))
  se.col <- lapply(se.col, function(x) adjustcolor(x, se.alpha))

  if (is.null(error.x.col)) {
    error.x.col <- marker.col
  } else {
    error.x.col <- error.x.col[seql(error.x.col, Nxgroups)]
  }

  if (is.null(error.y.col)) {
    error.y.col <- marker.col
  } else {
    # if (!is.list(error.y.col)) error.y.col <- as.list(error.y.col)
    # if (length(error.y.col) < Nygroups) error.y.col <- rep(error.y.col, Nygroups/length(error.y.col))
    error.y.col <- error.y.col[seql(error.y.col, Nygroups)]
  }

  # Values: Fit line & se ----
  # If plotting se bands, need to include (fitted +/- se.times * se) in the axis limits
  if (se.fit) sel <- list() else sel <- NULL
  if (rsq) rsql <- list() else rsql <- NULL
  if (rsq.pval) rsqpl <- list() else rsqpl <- NULL
  if (!is.null(fit)) {
    learner <- select_learn(fit, fn = FALSE)
    fitted <- list()
    for (i in seq_len(Nxgroups)) {
      x <- xl[[i]]
      y <- yl[[i]]
      # verbose FALSE should turn off plotting for all learners, no need for print.plot F
      learner.args <- c(
        list(x = x, y = y, verbose = trace > 1),
        fit.params
      )
      if (learner == "s_NLS") {
        learner.args <- c(
          learner.args,
          list(formula = formula, save.func = TRUE)
        )
      }
      mod <- try(do.call(learner, learner.args))
      if (class(mod)[1] == "try-error") {
        warning("Fitting ", fit, " failed: Defaulting to GLM fit")
        fit <- "GLM"
        mod <- do.call(s_GLM, list(x = x, y = y, verbose = FALSE))
      }
      fitted[[i]] <- fitted(mod)
      if (se.fit) sel[[i]] <- se(mod)
      if (rsq) rsql[[i]] <- mod$error.train$Rsq
      if (rsq.pval) {
        if (fit %in% c("LM", "GLM")) {
          rsqpl[[i]] <- paste0(
            ddSci(mod$error.train$Rsq),
            " (",
            ddSci(summary(mod$mod)$coefficients[2, 4]),
            ")"
          )
        } else if (fit == "GAM") {
          rsqpl[[i]] <- paste0(
            ddSci(mod$error.train$Rsq),
            " (",
            ddSci(summary(mod$mod)$s.pv),
            ")"
          )
        }
      }
    }
  }

  # Axes Limits ----
  if (is.null(xlim)) {
    xlim <- range(xl)
    if (is.list(error.x)) {
      error.x.hi <- lapply(seq(xl), function(i) xl[[i]] + error.x[[i]])
      error.x.lo <- lapply(seq(xl), function(i) xl[[i]] - error.x[[i]])
      xlim <- range(error.x.lo, error.x.hi, xlim)
    }
  }
  if (is.null(ylim)) {
    ylim <- range(yl)
    if (is.list(fitted) && !is.list(sel)) {
      ylim.hi <- max(unlist(fitted))
      ylim.lo <- min(unlist(fitted))
      ylim <- range(ylim.lo, ylim.hi, yl)
    }
    if (is.list(sel)) {
      ylim.hi <- max(unlist(lapply(
        seq_along(fitted),
        \(i)
          as.data.frame(fitted[[i]]) +
            se.times * as.data.frame(sel[[i]])
      )))
      ylim.lo <- min(unlist(lapply(
        seq_along(fitted),
        \(i)
          as.data.frame(fitted[[i]]) -
            se.times * as.data.frame(sel[[i]])
      )))
      ylim <- range(ylim.lo, ylim.hi, yl)
    }
    if (is.list(error.y)) {
      error.y.hi <- lapply(seq(yl), function(i) yl[[i]] + error.y[[i]])
      error.y.lo <- lapply(seq(yl), function(i) yl[[i]] - error.y[[i]])
      ylim <- range(error.y.lo, error.y.hi, ylim)
    }
  }
  if (axes.equal) xlim <- ylim <- range(xlim, ylim)

  # Plot ----
  if (!is.null(filename)) {
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  }
  par.orig <- par(no.readonly = TRUE)
  if (!is.null(rtenv$rtpar)) {
    par.reset <- FALSE
    par(
      mar = mar,
      bg = theme$bg,
      fg = theme$fg,
      pty = pty,
      cex = theme$cex,
      new = new,
      family = theme$font.family
    )
  } else {
    par(
      mar = mar,
      oma = oma,
      bg = theme$bg,
      fg = theme$fg,
      pty = pty,
      cex = theme$cex,
      new = new,
      family = theme$font.family
    )
  }
  par(family = theme$font.family)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  plot(
    NULL,
    NULL,
    xlim = xlim,
    ylim = ylim,
    ann = FALSE,
    axes = FALSE,
    xaxs = xaxs,
    yaxs = yaxs,
    xaxp = xaxp,
    yaxp = yaxp,
    log = log
  )

  # For rect only: must be AFTER plot(): Adjusted xlim if xaxs = "r"
  if (xaxs == "r")
    xlim <- c(
      min(xlim) - .04 * diff(range(xlim)),
      max(xlim) + .04 * diff(range(xlim))
    )
  if (yaxs == "r")
    ylim <- c(
      min(ylim) - .04 * diff(range(ylim)),
      max(ylim) + .04 * diff(range(ylim))
    )

  # Plot bg ----
  if (theme$plot.bg != "transparent") {
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = theme$plot.bg)
  }

  # Axes ----
  # axis(): col: color of the axis line;
  # col.axis: color of the tick labels;
  # col.ticks: color of the ticks themselves
  if (theme$axes.visible) {
    axis(
      side = theme$x.axis.side,
      line = theme$x.axis.line,
      at = x.axis.at,
      labels = x.axis.labs,
      col = theme$axes.col,
      col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
      col.axis = theme$tick.labels.col,
      las = theme$x.axis.las,
      padj = theme$x.axis.padj,
      hadj = theme$x.axis.hadj,
      tck = theme$tck,
      tcl = theme$tcl,
      cex = theme$cex,
      family = theme$font.family
    )
    axis(
      side = theme$y.axis.side,
      line = theme$y.axis.line,
      at = y.axis.at,
      labels = y.axis.labs,
      col = theme$axes.col,
      col.ticks = adjustcolor(theme$tick.col, theme$tick.alpha),
      col.axis = theme$tick.labels.col,
      las = theme$y.axis.las,
      padj = theme$y.axis.padj,
      hadj = theme$y.axis.hadj,
      tck = theme$tck,
      tcl = theme$tcl,
      cex = theme$cex,
      family = theme$font.family
    )
    mtext(
      xlab,
      side = theme$x.axis.side,
      line = theme$xlab.line,
      cex = theme$cex,
      adj = xlab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )
    mtext(
      ylab,
      side = theme$y.axis.side,
      line = theme$ylab.line,
      cex = theme$cex,
      adj = ylab.adj,
      col = theme$labs.col,
      family = theme$font.family
    )
  }

  # Grid ----
  if (theme$grid) {
    if (is.null(x.axis.at)) {
      grid(
        nx = theme$grid.nx,
        ny = NA,
        col = colorAdjust(theme$grid.col, theme$grid.alpha),
        lty = theme$grid.lty,
        lwd = theme$grid.lwd
      )
    } else {
      basegrid(
        x = x.axis.at,
        col = colorAdjust(theme$grid.col, theme$grid.alpha),
        lty = theme$grid.lty,
        lwd = theme$grid.lwd
      )
    }
    if (is.null(y.axis.at)) {
      grid(
        nx = NA,
        ny = theme$grid.ny,
        col = colorAdjust(theme$grid.col, theme$grid.alpha),
        lty = theme$grid.lty,
        lwd = theme$grid.lwd
      )
    } else {
      basegrid(
        y = y.axis.at,
        col = colorAdjust(theme$grid.col, theme$grid.alpha),
        lty = theme$grid.lty,
        lwd = theme$grid.lwd
      )
    }
  }

  # Zero Lines ----
  if (theme$zerolines) {
    zerocol <- adjustcolor(theme$zerolines.col, theme$zerolines.alpha)
    if (ylim[1] <= 0 && 0 <= ylim[2]) {
      lines(
        c(xlim[1], xlim[2]),
        c(0, 0),
        lwd = theme$zerolines.lwd,
        col = zerocol,
        lty = theme$zerolines.lty,
        xpd = TRUE
      )
    }
    if (xlim[1] <= 0 && 0 <= xlim[2]) {
      lines(
        c(0, 0),
        c(ylim[1], ylim[2]),
        lwd = theme$zerolines.lwd,
        col = zerocol,
        lty = theme$zerolines.lty,
        xpd = TRUE
      )
    }
  }

  # Diagonal ----
  if (diagonal) {
    abline(
      0,
      1,
      lwd = diagonal.lwd,
      lty = diagonal.lty,
      col = adjustcolor(diagonal.col, diagonal.alpha)
    )
  }
  if (diagonal.inv) {
    abline(
      1,
      -1,
      lwd = diagonal.lwd,
      lty = diagonal.lty,
      col = adjustcolor(diagonal.col, diagonal.alpha)
    )
  }

  # Box ----
  if (theme$bty != "n") {
    box(
      col = adjustcolor(theme$box.col, theme$box.alpha),
      lty = theme$box.lty,
      lwd = theme$box.lwd,
      bty = theme$bty
    )
  }

  # hline & vline ----
  if (!is.null(hline)) {
    abline(h = hline, lwd = hline.lwd, col = hline.col, lty = hline.lty)
  }
  if (!is.null(vline)) {
    abline(v = vline, lwd = vline.lwd, col = vline.col, lty = vline.lty)
  }

  # Points & Lines ----
  if (scatter) {
    if (!is.null(error.y)) {
      for (i in seq(error.y)) {
        suppressWarnings(arrows(
          xl[[i]],
          yl[[i]] - error.y[[i]],
          xl[[i]],
          yl[[i]] + error.y[[i]],
          lty = error.y.lty,
          lwd = error.y.lwd,
          angle = 90,
          code = error.arrow.code,
          length = 0.05,
          col = error.y.col[[i]]
        ))
      }
    }

    if (!is.null(error.x)) {
      for (i in seq(error.x)) {
        suppressWarnings(arrows(
          xl[[i]] - error.x[[i]],
          yl[[i]],
          xl[[i]] + error.x[[i]],
          yl[[i]],
          lty = error.x.lty,
          lwd = error.x.lwd,
          angle = 90,
          code = error.arrow.code,
          length = 0.05,
          col = error.x.col[[i]]
        ))
      }
    }

    if (length(marker.alpha) < length(marker.col)) {
      marker.alpha <- rep(
        marker.alpha,
        ceiling(length(marker.col) / length(marker.alpha))
      )
    }
    marker.col.alpha <- lapply(
      seq_along(marker.col),
      function(i) adjustcolor(marker.col[[i]], marker.alpha[[i]])
    )
    marker.col.alpha <- recycle(marker.col.alpha, xl)
    lwd <- recycle(lwd, xl)
    for (i in 1:Nxgroups) {
      points(
        xl[[i]],
        yl[[i]],
        type = type[[i]],
        col = marker.col.alpha[[i]],
        bg = point.bg.col[[i]],
        # lwd = theme$lwd,
        lwd = lwd[[i]],
        pch = pch[[i]],
        cex = point.cex,
        lty = lty[[i]],
        xpd = xpd
      ) # delta
    }
  }

  # Main Title ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    mtext(
      main,
      line = theme$main.line,
      font = theme$main.font,
      adj = theme$main.adj,
      cex = theme$cex,
      col = theme$main.col,
      family = theme$font.family
    )
  }

  # S.E. Shading ----
  if (se.fit && is.list(sel)) {
    for (i in seq_len(Nxgroups)) {
      if (se.lty == "poly") {
        try(polygon(
          c(xl[[i]], rev(xl[[i]])),
          c(
            fitted[[i]] + se.times * sel[[i]],
            rev(fitted[[i]] - se.times * sel[[i]])
          ),
          col = se.col[[i]],
          density = se.density,
          border = se.border
        ))
      } else {
        try(lines(
          xl[[i]],
          fitted[[i]] + se.times * sel[[i]],
          lty = se.lty,
          col = se.col[[i]],
          lwd = se.lwd
        ))
        try(lines(
          xl[[i]],
          fitted[[i]] - se.times * sel[[i]],
          lty = se.lty,
          col = se.col[[i]],
          lwd = se.lwd
        ))
      }
    }
  }

  # Fit Line ----
  if (is.null(fit)) fit.legend <- FALSE
  if (group.legend) fit.legend <- FALSE

  if (!is.null(fit)) {
    for (i in seq_len(Nxgroups)) {
      lines(
        xl[[i]],
        fitted[[i]],
        col = fit.col[[i]],
        lwd = fit.lwd,
        lty = lty[[i]]
      )
    }
  }

  # Fit Legend ----
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
    mtext(
      fit.text,
      col = fit.legend.col,
      side = fit.legend.side,
      adj = fit.legend.adj,
      at = fit.legend.at,
      cex = theme$cex,
      padj = fit.legend.padj,
      family = theme$font.family
    )
    fit.legend.n <- 1
  }

  # Group Legend ----
  if (group.legend) {
    if (!is.null(fit)) {
      group.col <- colorAdjust(unlist(fit.col)[seq_len(Nxgroups)], 100)
    } else {
      group.col <- colorAdjust(unlist(marker.col)[seq_len(Nxgroups)], 100)
    }
    if (!is.null(group.title)) group.col <- c(theme$fg, group.col)
    mtext(
      group.names,
      col = group.col,
      side = group.side,
      adj = group.adj,
      at = group.at,
      cex = theme$cex,
      xpd = xpd,
      padj = seq(group.padj, group.padj + 1.5 * (length(group.names) - 1), 1.5),
      family = theme$font.family
    )
  }

  # Fit error legend ----
  myerror <- NULL
  if (is.logical(fit.error)) {
    if (fit.error) {
      myerror <- lapply(seq(Nxgroups), function(i) mod_error(xl[[i]], yl[[i]]))
    }
  } else {
    if (class(fit.error)[1] == "mod_error") myerror <- list(fit.error)
  }
  if (!is.null(myerror)) {
    if (Nxgroups == 1) {
      annot.n <- paste0("n = ", length(x))
      annot.mse <- paste0("MSE = ", ddSci(myerror[[1]]$MSE))
      annot.Rsq <- paste("R\U00B2", ddSci(myerror[[1]]$Rsq))
      # annot.Rsq <- bquote("R"^"2" ~ "=" ~ .(ddSci(myerror[[1]]$Rsq)))
      mtext(
        annot.n,
        fit.error.side,
        line = -3.3,
        adj = .98,
        padj = fit.error.padj,
        cex = theme$cex,
        col = theme$fg,
        family = theme$font.family
      )
      mtext(
        annot.Rsq,
        fit.error.side,
        line = -2.2,
        adj = .98,
        padj = fit.error.padj,
        cex = theme$cex,
        col = theme$fg,
        family = theme$font.family
      )
      mtext(
        annot.mse,
        fit.error.side,
        line = -1.1,
        adj = .98,
        padj = fit.error.padj,
        cex = theme$cex,
        col = theme$fg,
        family = theme$font.family
      )
    } else {
      error.annot <- sapply(
        seq(Nxgroups),
        \(i)
          paste0(
            ddSci(myerror[[i]]$MSE),
            " (",
            ddSci(myerror[[i]]$Rsq),
            ")"
          )
      )
      mtext(
        # c(expression(paste("MSE (", R^2, ")", sep = "")), error.annot),
        c("MSE (R\U00B2)", error.annot),
        fit.error.side,
        adj = .98,
        cex = theme$cex,
        col = c(theme$fg, unlist(fit.col[seq(Nxgroups)])),
        line = rev(seq(-1.1, -Nxgroups * 1.1 - 1.1, -1.1)),
        family = theme$font.family
      )
    }
  }

  # R-squared ----
  if (rsq) {
    if (is.null(rsq.col))
      rsq.col <- c(unlist(adjustcolor(fit.col[seq(Nxgroups)], 2)))
    annot.rsq <- ddSci(unlist(rsql))
    if (is.null(rsq.line)) {
      if (rsq.side == 3) {
        rsq.line <- c(-1, seq(-2, -1 - Nxgroups, -1))
      } else {
        rsq.line <- c(-1 - Nxgroups, seq(-Nxgroups, -1, 1))
      }
    }
    mtext(
      c(expression("R"^2), annot.rsq),
      side = rsq.side,
      adj = rsq.adj,
      line = rsq.line,
      cex = theme$cex,
      col = c(theme$fg, unlist(rsq.col))
    )
  }

  if (rsq.pval) {
    if (is.null(rsq.col))
      rsq.col <- c(theme$fg, unlist(adjustcolor(fit.col[seq(Nxgroups)], 2)))
    annot.rsq <- c("R-sq (p-val)", unlist(rsqpl))
    mtext(
      rev(annot.rsq),
      side = rsq.side,
      adj = .98,
      padj = seq(-2.2, -2.2 - 1.5 * length(rsqpl), -1.5),
      cex = theme$cex,
      col = unlist(rev(rsq.col)),
      family = theme$font.family
    )
  }

  # Annotation ----
  if (!is.null(annotation)) {
    if (is.null(annotation.col)) annotation.col <- col[[1]]
    mtext(
      annotation,
      1,
      -1.5,
      adj = .97,
      cex = theme$cex,
      col = annotation.col,
      family = theme$font.family
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()
  invisible(list(xlim = xlim, ylim = ylim))
} # rtemis::mplot3_xy


#' True vs. Fitted plot
#'
#' An `mplot3_xy` wrapper with defaults for plotting a learner's performance
#'
#' @inheritParams mplot3_xy
#' @param x Numeric vector: True values
#' @param y Numeric vector: Predicted values
#' @param pty Character: "s" for square plot, "m" to fill device. Default = "s"
#' @param ... Additional argument to be passed to [mplot3_conf] (classification) or [mplot3_xy] (regression)
#'
#' @author EDG
#' @export

mplot3_fit <- function(
  x,
  y,
  fit = "lm",
  se.fit = TRUE,
  fit.error = TRUE,
  axes.equal = TRUE,
  diagonal = TRUE,
  theme = rtTheme,
  marker.col = NULL,
  fit.col = NULL,
  pty = "s",
  fit.legend = FALSE,
  mar = NULL,
  ...
) {
  type <- if (is.factor(y)) {
    "Classification"
  } else if (survival::is.Surv(y)) {
    "Survival"
  } else {
    "Regression"
  }
  if (type == "Classification") {
    mplot3_conf(table(y, x), ...)
  } else if (type == "Survival") {
    msg2("Not currently supported")
  } else {
    mplot3_xy(
      x,
      y,
      fit = fit,
      se.fit = se.fit,
      fit.error = fit.error,
      axes.equal = axes.equal,
      diagonal = diagonal,
      theme = theme,
      marker.col = marker.col,
      fit.col = fit.col,
      pty = pty,
      fit.legend = fit.legend,
      mar = mar,
      ...
    )
  }
} # rtemis::mplot3_fit

autoalpha <- function(x, gamma = .0008, min = .3) {
  max(min, 1 - x * gamma)
}
