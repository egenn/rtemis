# dplot3_box.R
# ::rtemis::
# 201?-22 E.D. Gennatas rtemis.org

#' Interactive Boxplots & Violin plots
#'
#' Draw interactive boxplots or violin plots using \pkg{plotly}
#'
#' For multiple box plots, the recommendation is:
#' - `x=dat[, columnindex]` for multiple variables of a data.frame
#' - `x=list(a=..., b=..., etc.)` for multiple variables of potentially
#' different length
#' - `x=split(var, group)` for one variable with multiple groups: group names
#' appear below boxplots
#' - `x=dat[, columnindex], group = factor` for grouping multiple variables:
#' group names appear in legend
#'
#' If `orientation == "h"`, `xlab` is applied to y-axis and vice versa.
#' Similarly, `x.axist.type` applies to y-axis - this defaults to
#' "category" and would not normally need changing.
#'
#' @param x Vector or List of vectors: Input
#' @param time Date or date-time vector
#' @param time.bin Character: "year", "quarter", "month", or "day". Period to
#' bin by
#' @param type Character: "box" or "violin"
#' @param group Factor to group by
#' @param x.transform Character: "none", "scale", or "minmax" to use raw values,
#' scaled and centered values or min-max normalized to 0-1, respectively.
#' Transform is applied to each variable before grouping, so that groups are
#' comparable
#' @param main Character: Plot title.
#' @param xlab Character: x-axis label.
#' @param ylab  Character: y-axis label.
#' @param col Color, vector: Color for boxes. If NULL, which will draw
#' colors from `palette`
#' @param alpha Float (0, 1]: Transparency for box colors.
#' @param bg Color: Background color. Default = "white"
#' @param plot.bg Color: Background color for plot area.
#' @param theme Character: Theme to use: Run `themes()` for available themes
#' @param palette Character: Name of \pkg{rtemis} palette to use.
#' Default = "rtCol1". Only used if `col = NULL`
#' @param quartilemethod Character: "linear", "exclusive", "inclusive"
#' @param xlim Numeric vector: x-axis limits
#' @param ylim Numeric vector: y-axis limits
#' @param boxpoints Character or FALSE: "all", "suspectedoutliers", "outliers"
#' See <https://plotly.com/r/box-plots/#choosing-the-algorithm-for-computing-quartiles>
#' @param xnames Character, vector, length = NROW(x): x-axis names. Default = NULL, which
#' tries to set names appropriately
#' @param group.lines Logical: If TRUE, add separating lines between groups of
#' boxplots
#' @param group.lines.dash Character: "solid", "dot", "dash", "longdash",
#' "dashdot", or "longdashdot"
#' @param group.lines.col Color for `group.lines`
#' @param group.lines.alpha Numeric: transparency for `group.lines.col`
#' @param order.by.fn Function: If defined, order boxes by increasing value of
#' this function (e.g. median).
#' @param font.size  Float: Font size for all labels.
#' @param ylab.standoff Numeric: Standoff for y-axis label
#' @param legend Logical: If TRUE, draw legend. Default = TRUE
#' @param legend.col Color: Legend text color. Default = NULL, determined by
#' the theme
#' @param legend.xy Float, vector, length 2: Relative x, y position for legend.
#' @param xaxis.type Character: "linear", "log", "date", "category",
#' "multicategory"
#' @param cataxis_tickangle Numeric: Angle for categorical axis tick labels
#' @param margin Named list: plot margins.
#' Default = `list(b = 65, l = 65, t = 50, r = 10, pad = 0)`
#' @param violin.box Logical: If TRUE and type is "violin" show box within
#' violin plot
#' @param orientation Character: "v" or "h" for vertical, horizontal
#' @param annotate_n Logical: If TRUE, annotate with N in each box
#' @param annotate_n_y Numeric: y position for `annotate_n`
#' @param annotate_mean Logical: If TRUE, annotate with mean of each box
#' @param annotate_meansd Logical: If TRUE, annotate with mean (SD) of each box
#' @param annotate_meansd_y Numeric: y position for `annotate_meansd`
#' @param annotate.col Color for annotations
#' @param labelify Logical: If TRUE, [labelify] x names
#' @param legend.orientation "v" or "h" for vertical, horizontal
#' @param legend.xanchor Character: Legend's x anchor: "left", "center",
#' "right", "auto"
#' @param legend.yanchor Character: Legend's y anchor: "top", "middle",
#' "bottom", "auto"
#' @param automargin.x Logical: If TRUE, automatically set x-axis amrgins
#' @param automargin.y Logical: If TRUE, automatically set y-axis amrgins
#' @param boxgroupgap Numeric: Sets the gap (in plot fraction) between boxes
#' of the same location coordinate
#' @param hovertext Character vector: Text to show on hover for each data point
#' @param show_n Logical: If TRUE, show N in each box
#' @param pvals Numeric vector: Precomputed p-values. Should correspond to each box.
#' Bypasses `htest` and `htest.compare`. Requires `group` to be set
#' @param htest Character: e.g. "t.test", "wilcox.test" to compare each box to
#' the *first* box. If grouped, compare within each group to the first box.
#' If p-value of test is less than `htest.thresh`, add asterisk above/
#' to the side of each box
#' @param htest.compare Integer: 0: Compare all distributions against the first one;
#' 2: Compare every second box to the one before it. Requires `group` to
#' be set
#' @param htest.y Numeric: y coordinate for `htest` annotation
#' @param htest.annotate Logical: if TRUE, include htest annotation
#' @param htest.annotate.x Numeric: x-axis paper coordinate for htest annotation
#' @param htest.annotate.y Numeric: y-axis paper coordinate for htest annotation
#' @param htest.star.col Color for htest annotation stars
#' @param htest.bracket.col Color for htest annotation brackets
#' @param starbracket.pad Numeric: Padding for htest annotation brackets
#' @param use.plotly.group If TRUE, use plotly's `group` arg to group
#' boxes.
#' @param width Numeric: Force plot size to this width. Default = NULL, i.e. fill
#' available space
#' @param height Numeric: Force plot size to this height. Default = NULL, i.e. fill
#' available space
#' @param displayModeBar Logical: If TRUE, show plotly's modebar
#' @param filename Character: Path to file to save static plot.
#' @param modeBar.file.format Character: "svg", "png", "jpeg", "pdf"
#' @param file.width Integer: File width in pixels for when `filename` is
#' set.
#' @param file.height Integer: File height in pixels for when `filename`
#' is set.
#' @param file.scale Numeric: If saving to file, scale plot by this number
# @param print.plot Logical: If TRUE, print plot, otherwise return it invisibly
#' @param ... Additional arguments passed to theme
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' # A.1 Box plot of 4 variables
#' dplot3_box(iris[, 1:4])
#' # A.2 Grouped Box plot
#' dplot3_box(iris[, 1:4], group = iris$Species)
#' dplot3_box(iris[, 1:4], group = iris$Species, annotate_n = TRUE)
#' # B. Boxplot binned by time periods
#' # Synthetic data with an instantenous shift in distributions
#' set.seed(2021)
#' dat1 <- data.frame(alpha = rnorm(200, 0), beta = rnorm(200, 2), gamma = rnorm(200, 3))
#' dat2 <- data.frame(alpha = rnorm(200, 5), beta = rnorm(200, 8), gamma = rnorm(200, -3))
#' x <- rbind(dat1, dat2)
#' startDate <- as.Date("2019-12-04")
#' endDate <- as.Date("2021-03-31")
#' time <- seq(startDate, endDate, length.out = 400)
#' dplot3_box(x[, 1], time, "year", ylab = "alpha")
#' dplot3_box(x, time, "year", legend.xy = c(0, 1))
#' dplot3_box(x, time, "quarter", legend.xy = c(0, 1))
#' dplot3_box(x, time, "month",
#'   legend.orientation = "h",
#'   legend.xy = c(0, 1),
#'   legend.yanchor = "bottom"
#' )
#' # (Note how the boxplots widen when the period includes data from both dat1 and dat2)
#' }
#'
dplot3_box <- function(
  x,
  time = NULL,
  time.bin = c("year", "quarter", "month", "day"),
  type = c("box", "violin"),
  group = NULL,
  x.transform = c("none", "scale", "minmax"),
  main = NULL,
  xlab = "",
  ylab = NULL,
  col = NULL,
  alpha = .6,
  bg = NULL,
  plot.bg = NULL,
  theme = rtTheme,
  palette = rtPalette,
  boxpoints = "outliers",
  quartilemethod = "linear",
  xlim = NULL,
  ylim = NULL,
  # width = 0,
  violin.box = TRUE,
  orientation = "v",
  annotate_n = FALSE,
  annotate_n_y = 1,
  annotate_mean = FALSE, # forr A.2.b.
  annotate_meansd = FALSE,
  annotate_meansd_y = 1,
  annotate.col = theme$labs.col,
  xnames = NULL,
  group.lines = FALSE,
  group.lines.dash = "dot",
  group.lines.col = NULL,
  group.lines.alpha = .5,
  labelify = TRUE,
  order.by.fn = NULL,
  font.size = 16,
  # Axes
  ylab.standoff = 18,
  legend = NULL,
  legend.col = NULL,
  legend.xy = NULL,
  legend.orientation = "v",
  legend.xanchor = "auto",
  legend.yanchor = "auto",
  xaxis.type = "category",
  cataxis_tickangle = "auto",
  # margin = list(t = 35, pad = 0),
  margin = list(b = 65, l = 65, t = 50, r = 12, pad = 0),
  automargin.x = TRUE,
  automargin.y = TRUE,
  # boxgap = 0, #1/nvars, #.12,
  boxgroupgap = NULL,
  hovertext = NULL,
  show_n = FALSE,
  # boxmode = NULL,
  pvals = NULL,
  htest = "none",
  htest.compare = 0,
  #    htest.thresh = .05,
  htest.y = NULL,
  htest.annotate = TRUE,
  htest.annotate.x = 0,
  htest.annotate.y = -.065,
  htest.star.col = theme$labs.col,
  htest.bracket.col = theme$labs.col,
  starbracket.pad = c(.04, .05, .09),
  use.plotly.group = FALSE,
  width = NULL,
  height = NULL,
  displayModeBar = TRUE,
  modeBar.file.format = "svg",
  filename = NULL,
  file.width = 500,
  file.height = 500,
  file.scale = 1,
  # print.plot = TRUE,
  ...
) {
  # Dependencies ----
  dependency_check("plotly")

  # Arguments ----
  type <- match.arg(type)
  x.transform <- match.arg(x.transform)

  # Convert vector or data.frame/data.table/matrix to list
  if (!is.list(x)) {
    # x is vector
    if (is.numeric(x)) {
      .names <- deparse(substitute(x))
      x <- list(x)
      names(x) <- .names
    } else {
      # x is data.frame or matrix
      .names <- colnames(x)
      x <- lapply(seq_len(NCOL(x)), function(i) x[, i])
      names(x) <- .names
    }
  }
  nvars <- length(x)
  if (nvars > 1 && !is.null(group) && !is.null(time)) {
    stop("Better use subplot for each variable")
  }
  horizontal <- orientation == "h"

  if (x.transform != "none") {
    if (x.transform == "scale") {
      x <- lapply(x, \(z) as.numeric(scale(z)))
    } else if (x.transform == "minmax") {
      x <- lapply(x, drange)
    } else {
      stop("Unsupported x.transform specified")
    }
  }

  # Order by fn ----
  if (!is.null(order.by.fn) && order.by.fn != "none") {
    if (is.null(time)) {
      if (is.list(x)) {
        .order <- order(sapply(x, order.by.fn, na.rm = TRUE))
        if (is.data.frame(x)) {
          x <- x[, .order]
        } else {
          x <- x[names(x)[.order]]
        }
      }
      if (!is.null(xnames)) xnames <- xnames[.order]
    } else {
      warning("Ignoring order.by.fn with time data")
      order.by.fn <- NULL
    }
  }

  # Remove non-numeric vectors
  # which.nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  # if (length(which.nonnum) > 0) x[[which.nonnum]] <- NULL

  if (!is.null(group)) group <- factor(group)
  n.groups <- if (is.null(group)) length(x) else length(levels(group))
  if (n.groups == 1) htest <- "none"
  .xnames <- xnames
  if (is.null(.xnames)) {
    .xnames <- names(x)
    if (is.null(.xnames)) .xnames <- paste0("Feature", seq(n.groups))
    if (labelify) .xnames <- labelify(.xnames)
  }

  # Colors ----
  if (is.character(palette)) palette <- rtpalette(palette)
  if (is.null(col)) col <- recycle(palette, seq(n.groups))[seq(n.groups)]
  if (!is.null(order.by.fn) && order.by.fn != "none") {
    col <- col[.order]
  }

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  if (theme$main.font == 2) main <- paste0("<b>", main, "</b>")
  bg <- plotly::toRGB(theme$bg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col)
  tick.col <- plotly::toRGB(theme$tick.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)
  # axes.col <- plotly::toRGB(theme$axes.col)

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  # Plot ----
  if (is.null(time)) {
    if (is.null(group)) {
      # A.1 Single and multiple boxplots ----
      if (is.null(legend)) legend <- FALSE
      # Args for first trace
      .args <- if (horizontal) {
        list(x = x[[1]], y = NULL)
      } else {
        list(x = NULL, y = x[[1]])
      }
      .args <- c(
        .args,
        list(
          type = type,
          # name = .xnames[1],
          name = if (show_n) {
            paste0(.xnames[1], " (N=", length(x[[1]]), ")")
          } else {
            .xnames[1]
          },
          line = list(color = plotly::toRGB(col[1])),
          fillcolor = plotly::toRGB(col[1], alpha),
          marker = list(color = plotly::toRGB(col[1], alpha)),
          showlegend = legend
          # width = width
        )
      )
      if (!is.null(hovertext) && n.groups == 1) {
        hovertext <- list(hovertext)
      }
      if (type == "box") {
        .args <- c(
          .args,
          list(
            quartilemethod = quartilemethod,
            boxpoints = boxpoints
          )
        )
        if (!is.null(hovertext)) .args$text <- hovertext[[1]]
      }
      if (type == "violin") .args$box <- list(visible = violin.box)
      plt <- do.call(plotly::plot_ly, .args)
      if (n.groups > 1) {
        for (i in seq_len(n.groups)[-1]) {
          plt <- plotly::add_trace(
            plt,
            x = if (horizontal) x[[i]] else NULL,
            y = if (horizontal) NULL else x[[i]],
            # name = .xnames[i],
            name = if (show_n) {
              paste0(.xnames[i], " (N=", length(x[[i]]), ")")
            } else {
              .xnames[i]
            },
            line = list(color = plotly::toRGB(col[i])), # box borders
            fillcolor = plotly::toRGB(col[i], alpha), # box fill
            marker = list(color = plotly::toRGB(col[i], alpha)), # points
            text = if (!is.null(hovertext)) hovertext[[i]] else NULL
          )
        }
      }

      # '-Annotate N ----
      if (annotate_n) {
        Nperbox <- Filter(
          function(i) i > 0,
          sapply(x, function(j) length(na.exclude(j)))
        )
        plt <- plt |>
          plotly::add_annotations(
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "bottom",
            x = 0,
            y = annotate_n_y,
            text = "N =",
            font = list(
              family = theme$font.family,
              size = font.size,
              color = annotate.col
            ),
            showarrow = FALSE
          ) |>
          plotly::add_annotations(
            xref = "x",
            yref = "paper",
            yanchor = "bottom",
            # x = seq_len(nvars) - 1,
            x = seq_along(Nperbox) - 1,
            y = 1,
            text = as.character(Nperbox),
            font = list(
              family = theme$font.family,
              size = font.size,
              color = annotate.col
            ),
            showarrow = FALSE
          )
      } # /annotate_n

      # '-Annotate Mean SD ----
      if (annotate_meansd) {
        Meanperbox <- Filter(
          function(i) i > 0,
          sapply(x, function(j) mean(na.exclude(j)))
        ) |>
          round(digits = 2) |>
          format(nsmall = 2)
        SDperbox <- Filter(
          function(i) i > 0,
          sapply(x, function(j) sd(na.exclude(j)))
        ) |>
          round(digits = 2) |>
          format(nsmall = 2)
        plt <- plt |>
          # plotly::add_annotations(
          #   xref = "paper", yref = "paper",
          #   xanchor = "right",
          #   yanchor = "bottom",
          #   x = 0, y = annotate_meansd_y,
          #   text = "Mean (SD)",
          #   font = list(
          #     family = theme$font.family,
          #     size = font.size,
          #     color = annotate.col
          #   ),
          #   showarrow = FALSE
          # ) |>
          plotly::add_annotations(
            xref = "x",
            yref = "paper",
            yanchor = "bottom",
            # x = seq_len(nvars) - 1,
            x = seq_along(Meanperbox) - 1,
            y = 1,
            # text = as.character(Nperbox),
            text = paste0(Meanperbox, " (", SDperbox, ")"),
            font = list(
              family = theme$font.family,
              size = font.size,
              color = annotate.col
            ),
            showarrow = FALSE
          )
      } # /annotate_meansd

      # '-htest ----
      if (htest != "none") {
        if (htest.compare == 0) {
          pvals <- sapply(x[-1], \(v) {
            suppressWarnings(
              do.call(htest, list(x = x[[1]], y = v))$p.value
            )
          })
        }
        y_sb <- starbracket_y(unlist(x), pad = starbracket.pad)
        if (is.null(htest.y)) htest.y <- y_sb$star
        plt <- plt |>
          plotly::add_annotations(
            xref = if (horizontal) "paper" else "x",
            # yref = if (horizontal) "x" else "paper",
            yref = if (horizontal) "x" else "y",
            yanchor = if (horizontal) "auto" else "top",
            xanchor = if (horizontal) "center" else "auto",
            x = if (horizontal) htest.y else seq_along(pvals), # exclude first
            y = if (horizontal) seq_along(pvals) else htest.y,
            # text = unname(ifelse(pvals < htest.thresh, "*", "")),
            text = pval_stars(pvals),
            font = list(
              family = theme$font.family,
              size = font.size,
              color = annotate.col
            ),
            showarrow = FALSE
          )

        if (htest.annotate) {
          test <- switch(
            htest,
            `wilcox.test` = "Wilcoxon",
            `t.test` = "T-test",
            htest
          )
          plt <- plt |>
            plotly::add_annotations(
              xref = "paper",
              yref = "paper",
              yanchor = "top",
              xanchor = "left",
              x = htest.annotate.x,
              y = htest.annotate.y,
              # text = paste0("<sup>*</sup>", test, " p-val < ", htest.thresh),
              # text = paste0("* ", test, " p-val < ", htest.thresh),
              # text = paste0(
              #     '<span style="color:',
              #     htest.star.col, '">* </span>',
              #     test, " p-val < ", htest.thresh),
              text = paste0(
                test,
                " p-val:",
                '<span style="color:',
                htest.star.col,
                '"> * </span>',
                "< .05",
                '<span style="color:',
                htest.star.col,
                '"> ** </span>',
                "< .01",
                '<span style="color:',
                htest.star.col,
                '"> *** </span>',
                "< .001"
              ),
              font = list(
                family = theme$font.family,
                size = font.size,
                color = annotate.col
              ),
              showarrow = FALSE
            )
        }
      } # / htest!="none"
    } else {
      if (use.plotly.group) {
        # A.2.a Grouped boxplots with [group] ----
        # Best to use this for multiple variables x group.
        # For single variables x group, preferred way it to use
        # split(var, group) => A1
        if (is.null(legend)) legend <- TRUE
        dt <- cbind(data.table::as.data.table(x), group = group)
        dtlong <- data.table::melt(
          dt[, ID := seq_len(nrow(dt))],
          id.vars = c("ID", "group")
        )
        if (is.null(ylab)) ylab <- ""
        .args <- list(
          data = dtlong,
          type = type,
          x = if (horizontal) ~value else ~variable,
          y = if (horizontal) ~variable else ~value,
          color = ~group,
          colors = col2hex(col),
          showlegend = legend
        )
        if (type == "box") {
          .args <- c(
            .args,
            list(
              quartilemethod = quartilemethod,
              boxpoints = boxpoints,
              alpha = alpha
            )
          )
          if (!is.null(hovertext)) {
            dtlong <- merge(dtlong, cbind(dt[, list(ID)], hovertext))
            .args$text <- dtlong$hovertext
          }
        }
        if (type == "violin") .args$box <- list(visible = violin.box)
        cataxis <- list(
          tickvals = 0:(NCOL(dt) - 2),
          ticktext = .xnames
        )
        .args <- c(list(width = width, height = height), .args)
        plt <- do.call(plotly::plot_ly, .args) |>
          plotly::layout(
            boxmode = "group",
            xaxis = if (horizontal) NULL else cataxis,
            yaxis = if (horizontal) cataxis else NULL
          )
      } else {
        # A.2.b Grouped boxplots with split and loop ----
        # Replaces A.2.a to allow annotation positioning
        if (is.null(legend)) legend <- TRUE
        dts <- split(data.table::as.data.table(x), group, drop = TRUE)

        if (is.null(ylab)) ylab <- ""
        if (type == "box") {
          .args <- list(
            type = "box",
            quartilemethod = quartilemethod,
            boxpoints = boxpoints,
            alpha = alpha
          )
        } else {
          .args <- list(
            type = "violin",
            box = list(visible = violin.box)
          )
        }

        varnames <- names(x)
        nvars <- length(varnames)
        ngroups <- length(dts)
        groupnames <- names(dts)
        xval <- do.call(paste, expand.grid(groupnames, varnames))
        # text = xval[i],
        xval <- factor(xval, levels = xval)

        boxindex <- 0

        # plt <- plotly::plot_ly(type = type) # box or violin
        .args <- c(list(width = width, height = height), .args)
        plt <- do.call(plotly::plot_ly, .args)
        for (i in seq_along(varnames)) {
          # loop vars
          for (j in seq_along(dts)) {
            # loop groups
            boxindex <- boxindex + 1
            plt <- plt |>
              plotly::add_trace(
                x = if (horizontal) dts[[j]][[i]] else xval[boxindex],
                y = if (horizontal) xval[boxindex] else dts[[j]][[i]],
                name = groupnames[j],
                meta = xval[boxindex],
                line = list(color = plotly::toRGB(col[j])),
                fillcolor = plotly::toRGB(col[j], alpha),
                marker = list(color = plotly::toRGB(col[j], alpha)),
                showlegend = legend & (i == nvars),
                hoverinfo = "all",
                legendgroup = groupnames[j]
              )
          }
        }

        cataxis <- list(
          type = "category",
          tickmode = "array",
          tickvals = (mean(seq_len(ngroups)) + 0:(nvars - 1) * ngroups) - 1, # need -1 if type = "category"
          ticktext = .xnames,
          tickangle = cataxis_tickangle,
          automargin = TRUE
        )

        plt <- plt |>
          plotly::layout(
            xaxis = if (horizontal) NULL else cataxis,
            yaxis = if (horizontal) cataxis else NULL
          )

        # '- Group lines ----
        if (nvars > 1 && group.lines) {
          if (is.null(group.lines.col)) {
            group.lines.col <- theme$fg
          }
          group.lines.col <- adjustcolor(
            group.lines.col,
            group.lines.alpha
          )
          at <- seq((ngroups - .5), (ngroups * (nvars - 1) - .5), by = ngroups)
          if (horizontal) {
            plt <- plt |>
              plotly::layout(
                shapes = plotly_hline(
                  at,
                  color = group.lines.col,
                  dash = group.lines.dash
                )
              )
          } else {
            plt <- plt |>
              plotly::layout(
                shapes = plotly_vline(
                  at,
                  color = group.lines.col,
                  dash = group.lines.dash
                )
              )
          }
        }

        # '-Annotate N ----
        if (annotate_n) {
          Nperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) length(na.exclude(j)))
            })))
          )
          plt <- plt |>
            plotly::add_annotations(
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "bottom",
              x = 0,
              y = annotate_n_y,
              text = "N =",
              font = list(
                family = theme$font.family,
                size = font.size,
                color = annotate.col
              ),
              showarrow = FALSE
            ) |>
            plotly::add_annotations(
              xref = "x",
              yref = "paper",
              yanchor = "bottom",
              x = seq_len(nvars * ngroups) - 1,
              y = 1,
              text = as.character(Nperbox),
              font = list(
                family = theme$font.family,
                size = font.size,
                color = annotate.col
              ),
              showarrow = FALSE
            )
        } # /annotate_n

        # '-Annotate Mean SD ----
        if (annotate_meansd) {
          Meanperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) mean(na.exclude(j)))
            })))
          ) |>
            round(digits = 2) |>
            format(nsmall = 2)
          SDperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) sd(na.exclude(j)))
            })))
          ) |>
            round(digits = 2) |>
            format(nsmall = 2)
          plt <- plt |>
            # plotly::add_annotations(
            #   xref = "paper", yref = "paper",
            #   xanchor = "right",
            #   yanchor = "bottom",
            #   x = 0, y = annotate_meansd_y,
            #   text = "N =",
            #   font = list(
            #     family = theme$font.family,
            #     size = font.size,
            #     color = annotate.col
            #   ),
            #   showarrow = FALSE
            # ) |>
            plotly::add_annotations(
              xref = "x",
              yref = "paper",
              yanchor = "bottom",
              x = seq_len(nvars * ngroups) - 1,
              y = 1,
              text = paste0(Meanperbox, " (", SDperbox, ")"),
              font = list(
                family = theme$font.family,
                size = font.size,
                color = annotate.col
              ),
              showarrow = FALSE
            )
        } # /annotate_meansd

        # '-Annotate Mean ----
        if (annotate_mean) {
          Meanperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) mean(na.exclude(j)))
            })))
          ) |>
            round(digits = 1) |>
            format(nsmall = 1)
          plt <- plt |>
            plotly::add_annotations(
              xref = "x",
              yref = "paper",
              yanchor = "bottom",
              x = seq_len(nvars * ngroups) - 1,
              y = 1,
              text = Meanperbox,
              font = list(
                family = theme$font.family,
                size = font.size,
                color = annotate.col
              ),
              showarrow = FALSE
            )
        } # /annotate_mean

        # '- htest ----
        if (htest != "none" || !is.null(pvals)) {
          # dts list elements are groups; columns are variables
          # pvals is N groups -1 x N vars
          if (is.null(pvals)) {
            if (htest.compare == 0) {
              pvals <- sapply(seq_len(nvars), \(cid) {
                sapply(2:ngroups, \(gid) {
                  suppressWarnings(
                    do.call(
                      htest,
                      list(
                        x = dts[[1]][[cid]],
                        y = dts[[gid]][[cid]]
                      )
                    )$p.value
                  )
                })
              })
              pvals <- c(rbind(1, pvals))
            } else if (htest.compare == 2) {
              pvals <- rep(1, nvars * ngroups)
              pvals[seq(2, ngroups * nvars, 2)] <- lapply(
                seq_len(nvars),
                \(cid) {
                  lapply(seq(htest.compare, ngroups, htest.compare), \(gid) {
                    suppressWarnings(
                      do.call(
                        htest,
                        list(
                          x = dts[[gid - 1]][[cid]],
                          y = dts[[gid]][[cid]]
                        )
                      )$p.value
                    )
                  })
                }
              ) |>
                unlist()
            }
          }
          # if brackets are drawn, center stars above them, otherwise
          # center stars above boxes
          axshift <- if (htest.compare == 2) 1.5 else 1
          y_sb <- starbracket_y(unlist(x), pad = starbracket.pad)
          if (is.null(htest.y)) htest.y <- y_sb$star
          plt <- plt |>
            plotly::add_annotations(
              xref = if (horizontal) "paper" else "x",
              # yref = if (horizontal) "x" else "paper",
              yref = if (horizontal) "x" else "y",
              yanchor = if (horizontal) "auto" else "top",
              xanchor = if (horizontal) "center" else "auto",
              x = if (horizontal) htest.y else
                seq_len(nvars * ngroups) - axshift,
              y = if (horizontal) seq_len(nvars * ngroups) - axshift else
                htest.y,
              # text = unname(ifelse(pvals < htest.thresh, "*", "")),
              text = pval_stars(pvals),
              font = list(
                family = theme$font.family,
                size = font.size,
                color = htest.star.col
              ),
              showarrow = FALSE
            )
          if (htest.annotate) {
            test <- switch(
              htest,
              `wilcox.test` = "Wilcoxon",
              `t.test` = "T-test",
              htest
            )
            plt <- plt |>
              plotly::add_annotations(
                xref = "paper",
                yref = "paper",
                yanchor = "top",
                xanchor = "left",
                x = htest.annotate.x,
                y = htest.annotate.y,
                # text = paste0("<sup>*</sup>", test, " p-val < ", htest.thresh),
                # text = paste0("* ", test, " p-val < ", htest.thresh),
                # text = paste0(
                #     '<span style="color:',
                #     htest.star.col, '">* </span>',
                #     test, " p-val < ", htest.thresh
                # ),
                text = paste0(
                  test,
                  " p-val:",
                  '<span style="color:',
                  htest.star.col,
                  '"> * </span>',
                  "< .05",
                  '<span style="color:',
                  htest.star.col,
                  '"> ** </span>',
                  "< .01",
                  '<span style="color:',
                  htest.star.col,
                  '"> *** </span>',
                  "< .001"
                ),
                font = list(
                  family = theme$font.family,
                  size = font.size,
                  color = annotate.col
                ),
                showarrow = FALSE
              )
          } # /htest.annotate

          # '- htest brackets for htest.compare == 2 ----
          if (htest.compare == 2) {
            for (i in seq(2, ngroups * nvars, 2)) {
              if (pvals[i] < .05) {
                # y_bracket <- bracket_y(unlist(x))
                plt <- plt |>
                  plotly::add_trace(
                    x = c(rep(xval[i - 1], 2), rep(xval[i], 2)),
                    y = y_sb$bracket,
                    type = "scatter",
                    mode = "lines",
                    inherit = FALSE,
                    line = list(color = htest.bracket.col, width = 1),
                    showlegend = FALSE
                  )
              }
            }
          }
        } # /htest grouped
      }
    }
  } else {
    # B. Time-binned boxplots ----
    time.bin <- match.arg(time.bin)
    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- ""
    if (is.null(legend)) legend <- TRUE

    dt <- data.table::as.data.table(x)
    if (!is.null(group)) dt[, group := group]
    if (!is.null(hovertext)) dt[, hovertext := hovertext]

    dt[, timeperiod := date2factor(time, time.bin)] |>
      setkey(timeperiod)

    Npertimeperiod <- dt[levels(timeperiod)][,
      lapply(.SD, \(i) length(na.exclude(i))),
      by = timeperiod
    ] |>
      setorder()

    ## Long data
    # appease R CMD check
    ID <- timeperiod <- NULL
    dtlong <- data.table::melt(
      dt[, ID := .I],
      id.vars = c(
        "ID",
        "timeperiod",
        mgetnames(dt, "group", "hovertext")
      )
    )

    if (is.null(group)) {
      .args <- list(
        data = dtlong,
        type = type,
        x = if (horizontal) ~value else ~timeperiod,
        y = if (horizontal) ~timeperiod else ~value,
        color = ~variable,
        colors = col2hex(col),
        showlegend = legend
      )
    } else {
      .args <- list(
        data = dtlong,
        type = type,
        x = if (horizontal) ~value else ~timeperiod,
        y = if (horizontal) ~timeperiod else ~value,
        color = ~group,
        colors = col2hex(col),
        showlegend = legend
      )
    }

    if (!is.null(hovertext)) .args$text <- dtlong$hovertext

    if (type == "box") {
      .args <- c(
        .args,
        list(
          quartilemethod = quartilemethod,
          boxpoints = boxpoints
        )
      )
    }
    if (type == "violin") .args$box <- list(visible = violin.box)

    .args <- c(list(width = width, height = height), .args)
    plt <- do.call(plotly::plot_ly, .args)
    if (!is.null(group) || nvars > 1) {
      plt <- plt |> plotly::layout(boxmode = "group")
    }

    # '-Annotate N ----
    if (is.null(group) && annotate_n) {
      Nperbox <- Npertimeperiod[[2]] # include zeros
      plt <- plt |>
        plotly::add_annotations(
          xref = "paper",
          yref = "paper",
          xanchor = "right",
          yanchor = "bottom",
          x = 0,
          y = annotate_n_y,
          text = "N =",
          font = list(
            family = theme$font.family,
            size = font.size,
            color = annotate.col
          ),
          showarrow = FALSE
        ) |>
        plotly::add_annotations(
          xref = "x",
          yref = "paper",
          yanchor = "bottom",
          x = seq_along(Nperbox) - 1,
          y = 1,
          text = paste(Nperbox),
          font = list(
            family = theme$font.family,
            size = font.size,
            color = annotate.col
          ),
          showarrow = FALSE
        )
    }
  } # /time-binned boxplots

  # Layout ----
  f <- list(
    family = theme$font.family,
    size = font.size,
    color = labs.col
  )
  tickfont <- list(
    family = theme$font.family,
    size = font.size,
    color = theme$tick.labels.col
  )
  .legend <- list(
    x = legend.xy[1],
    y = legend.xy[2],
    xanchor = legend.xanchor,
    yanchor = legend.yanchor,
    bgcolor = "#ffffff00",
    font = list(
      family = theme$font.family,
      size = font.size,
      color = legend.col
    ),
    orientation = legend.orientation
  )

  yaxis.title <- if (horizontal) xlab else ylab
  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = list(text = yaxis.title, standoff = ylab.standoff),
      type = if (horizontal) xaxis.type else NULL,
      titlefont = f,
      showgrid = if (horizontal) FALSE else theme$grid,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = if (horizontal) NA else tick.col,
      tickfont = tickfont,
      zeroline = FALSE,
      automargin = automargin.y,
      range = ylim
    ),
    xaxis = list(
      title = if (horizontal) ylab else xlab,
      type = if (horizontal) NULL else xaxis.type,
      titlefont = f,
      showgrid = if (horizontal) theme$grid else FALSE,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      tickcolor = if (horizontal) tick.col else NA,
      tickfont = tickfont,
      automargin = automargin.x,
      range = xlim
    ),
    title = list(
      text = main,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = main.col
      ),
      xref = "paper",
      x = theme$main.adj
    ),
    paper_bgcolor = bg,
    plot_bgcolor = plot.bg,
    margin = margin,
    legend = .legend,
    # boxgap = boxgap,
    boxgroupgap = boxgroupgap
  )

  # Config ----
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar.file.format,
      width = file.width,
      height = file.height
    )
  )

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file = file.path(filename),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  plt
} # rtemis::dplot3_box.R

# todo: htest.compare = n
