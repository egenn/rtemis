# dplot3_volcano
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org
# allow custom grouping

#' Volcano Plot
#'
#' @inheritParams dplot3_xy
#' @param x Numeric vector: Input values, e.g. log2 fold change, coefficients, etc.
#' @param xnames Character vector: `x` names
#' @param group Factor: Used to color code points. If NULL, significant points
#' below `x.thresh`, non-significant points, and significant points
#' above `x.thresh` will be plotted with the first, second and third
#' color fo `palette`
#' @param x.thresh Numeric x-axis threshold separating low from high
#' @param pvals Numeric vector: p-values
#' @param p.thresh Numeric: p-value threshold of significance. Default = .05
#' @param p.transform function. Default = `\(x) -log10(x)`
#' @param p.adjust.method Character: p-value adjustment method.
#' "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
#' Default = "holm". Use "none" for raw p-values.
#' @param legend Logical: If TRUE, show legend. Will default to FALSE, if
#' `group = NULL`, otherwise to TRUE
#' @param legend.lo Character: Legend to annotate significant points below the
#' `x.thresh`
#' @param legend.hi Character: Legend to annotate significant points above the
#' `x.thresh`
#' @param legend.x.lo Numeric: x position of `legend.lo`
#' @param legend.x.hi Numeric: x position of `legend.hi`
#' @param legend.y Numeric: y position for `legend.lo` and `legend.hi`
#' @param annotate Logical: If TRUE, annotate significant points
#' @param annotate.n Integer: Number of significant points to annotate
#' @param ax.lo Numeric: Sets the x component of the arrow tail about the arrow head for
#' significant points below `x.thresh`
#' @param ay.lo Numeric: Sets the y component of the arrow tail about the arrow head for
#' significant points below `x.thresh`
#' @param ax.hi Numeric: Sets the x component of the arrow tail about the arrow head for
#' significant points above `x.thresh`
#' @param ay.hi Numeric: Sets the y component of the arrow tail about the arrow head for
#' significant points above `x.thresh`
#' @param label.lo Character: label for low values
#' @param label.hi Character: label for high values
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param margin Named list of plot margins.
#' Default = `list(b = 65, l = 65, t = 50, r = 10, pad = 0)`
#' @param xlim Numeric vector, length 2: x-axis limits
#' @param ylim Numeric vector, length 2: y-axis limits
#' @param alpha Numeric: point transparency
#' @param hline Numeric: If defined, draw a horizontal line at this y value.
#' @param hline.col Color for `hline`. Default = "#ff0000" (red)
#' @param hline.width Numeric: Width for `hline`. Default = 1
#' @param hline.dash Character: Type of line to draw: "solid", "dot", "dash",
#' "longdash", "dashdot",
#' or "longdashdot"
#' @param hline.annotate Character: Text of horizontal line annotation if
#' `hline` is set
#' @param hline.annotation.x Numeric: x position to place annotation with paper
#' as reference. 0: to the left of the plot area; 1: to the right of the plot area
#' @param annotate Logical: If TRUE, annotate significant points
#' @param annotate.col Color for annotations
#' @param annotate.alpha Numeric: Transparency for annotations
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Additional parameters passed to [dplot3_xy]
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(2019)
#' x <- rnormmat(500, 500)
#' y <- x[, 3] + x[, 5] - x[, 9] + x[, 15] + rnorm(500)
#' mod <- massGLM(y, x)
#' dplot3_volcano(mod$summary$`Coefficient y`, mod$summary$`p_value y`)
#' }
#'
dplot3_volcano <- function(
  x,
  pvals,
  xnames = NULL,
  group = NULL,
  x.thresh = 0,
  p.thresh = .05,
  p.transform = \(x) -log10(x),
  p.adjust.method = c(
    "holm",
    "hochberg",
    "hommel",
    "bonferroni",
    "BH",
    "BY",
    "fdr",
    "none"
  ),
  legend = NULL,
  legend.lo = NULL,
  legend.hi = NULL,
  label.lo = "Low",
  label.hi = "High",
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  xlim = NULL,
  ylim = NULL,
  alpha = NULL,
  hline = NULL,
  hline.col = NULL,
  hline.width = 1,
  hline.dash = "solid",
  hline.annotate = NULL,
  hline.annotation.x = 1,
  annotate = TRUE,
  annotate.col = theme$labs.col,
  theme = rtTheme,
  font.size = 16,
  palette = NULL,
  legend.x.lo = NULL,
  legend.x.hi = NULL,
  legend.y = .97,
  annotate.n = 7,
  ax.lo = NULL, # 40,
  ay.lo = NULL,
  ax.hi = NULL, # -40,
  ay.hi = NULL,
  annotate.alpha = .7,
  hovertext = NULL,
  displayModeBar = FALSE,
  #    mathjax = "cdn",
  filename = NULL,
  file.width = 500,
  file.height = 500,
  file.scale = 1,
  verbose = TRUE,
  ...
) {
  xname <- deparse(substitute(x))
  p.adjust.method <- match.arg(p.adjust.method)
  filt <- !is.na(x) & !is.na(pvals)
  x <- x[filt]
  pvals <- pvals[filt]
  if (is.null(xnames)) {
    xnames <- names(x)[filt]
  } else {
    xnames <- xnames[filt]
  }
  if (is.null(xnames)) xnames <- paste("Feature", seq_along(x))
  if (is.null(legend)) legend <- !is.null(group)

  p_adjusted <- p.adjust(pvals, method = p.adjust.method)
  index_ltpthresh <- p_adjusted < p.thresh
  p_transformed <- p.transform(p_adjusted)
  if (is.null(xlab)) xlab <- labelify(xname)

  if (is.null(ylab)) {
    ylab <- paste(print_fn(p.transform), "p-value")
  }

  # Default to lo - ns - hi groups
  if (is.null(group)) {
    group <- rep("NS", length(pvals))
    group[index_ltpthresh & x < x.thresh] <- label.lo
    group[index_ltpthresh & x > x.thresh] <- label.hi
    group <- factor(group, levels = c(label.lo, "NS", label.hi))
    if (is.null(palette)) {
      palette <- list("#18A3AC", "#7f7f7f", "#F48024")
    }
  }

  group.counts <- table(group)
  include <- group.counts > 0
  if (verbose) {
    cat("Group counts:\n")
    print(group.counts)
  }

  if (is.null(palette)) {
    palette <- rtpalette(rtPalette)
  }

  # Theme ----
  # extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), list())
  }
  # else {
  #     # Override with extra arguments
  #     for (i in seq(extraargs)) {
  #         theme[[names(extraargs)[i]]] <- extraargs[[i]]
  #     }
  # }

  # Plot ----
  if (is.null(hovertext)) hovertext <- xnames
  plt <- dplot3_xy(
    x,
    p_transformed,
    main = main,
    xlab = xlab,
    ylab = ylab,
    alpha = alpha,
    theme = theme,
    margin = margin,
    legend = legend,
    group = group,
    palette = palette[include],
    hovertext = hovertext,
    ...
  )

  # High - Low legend ----
  autolegend.x.lo <- is.null(legend.x.lo)
  if (autolegend.x.lo) {
    # legend.x.lo <- Filter(\(x) x < x.thresh, x) |> range() |> diff() * -.2 + x.thresh
    legend.x.lo <- x.thresh - abs(diff(c(x.thresh, min(x, na.rm = TRUE)))) * .2
  }

  autolegend.x.hi <- is.null(legend.x.hi)
  if (autolegend.x.hi) {
    # legend.x.hi <- Filter(\(x) x > x.thresh, x) |> range() |> diff() * .2 + x.thresh
    legend.x.hi <- x.thresh + abs(diff(c(x.thresh, max(x, na.rm = TRUE)))) * .2
  }

  legxdiff <- legend.x.hi - legend.x.lo

  if (autolegend.x.lo) legend.x.lo <- x.thresh - legxdiff / 2
  if (autolegend.x.hi) legend.x.hi <- x.thresh + legxdiff / 2

  if (group.counts[1] > 0 && !is.null(legend.lo)) {
    plt <- plt |>
      plotly::add_annotations(
        x = legend.x.lo,
        y = legend.y,
        text = legend.lo,
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(
          color = palette[[1]],
          family = theme$font.family,
          size = font.size
        )
      )
  }

  if (group.counts[3] > 0 && !is.null(legend.hi)) {
    plt <- plt |>
      plotly::add_annotations(
        x = legend.x.hi,
        y = legend.y,
        text = legend.hi,
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(
          color = palette[[3]],
          family = theme$font.family,
          size = font.size
        )
      )
  }

  # Annotations ----
  if (annotate) {
    yrange <- range(p_transformed)
    index_ltxthresh <- x < x.thresh
    index_gtxthresh <- x > x.thresh

    index_lo <- index_ltpthresh & index_ltxthresh
    index_hi <- index_ltpthresh & index_gtxthresh
    annotate.n_lo <- annotate.n_hi <- annotate.n
    if (sum(index_lo) < annotate.n) annotate.n_lo <- sum(index_lo)
    if (sum(index_hi) < annotate.n) annotate.n_hi <- sum(index_hi)

    if (annotate.n_lo > 0) {
      lo_ord <- order(pvals[index_lo])
      lo_x <- x[index_lo][lo_ord[seq_len(annotate.n_lo)]]
      lo_pval <- p_transformed[index_lo][lo_ord[seq_len(annotate.n_lo)]]
      lo_name <- xnames[index_lo][lo_ord[seq_len(annotate.n_lo)]]

      if (is.null(ay.lo)) {
        if (is.null(ay.lo)) {
          ay.lo <- drange(order(lo_pval), 30, -30)
        }
      }
      if (is.null(ax.lo)) ax.lo <- 5 + 5 * annotate.n_lo
      plt <- plt |>
        plotly::add_annotations(
          x = lo_x,
          y = lo_pval,
          text = lo_name,
          arrowhead = 4,
          arrowcolor = adjustcolor(theme$fg, .33),
          arrowsize = .5,
          arrowwidth = 1,
          ax = ax.lo,
          ay = ay.lo,
          xanchor = "left",
          font = list(
            size = 16,
            family = theme$font.family,
            color = adjustcolor(theme$fg, annotate.alpha)
          )
        )
    }

    # Annotate 10 most significant increasing
    if (annotate.n_hi > 0) {
      hi_ord <- order(pvals[index_ltpthresh & index_gtxthresh])
      hi_x <- x[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(
        annotate.n_hi
      )]]
      hi_pval <- p_transformed[
        index_ltpthresh & index_gtxthresh
      ][hi_ord[seq_len(annotate.n_hi)]]
      hi_name <- xnames[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(
        annotate.n_hi
      )]]

      if (is.null(ay.hi)) {
        ay.hi <- drange(order(hi_pval), 50, -50)
      }
      if (is.null(ax.hi)) ax.hi <- -5 - 5 * annotate.n_hi
      plt <- plt |>
        plotly::add_annotations(
          x = hi_x,
          y = hi_pval,
          text = hi_name,
          arrowhead = 4,
          arrowcolor = adjustcolor(theme$fg, .33),
          arrowsize = .5,
          arrowwidth = 1,
          ax = ax.hi,
          ay = ay.hi,
          xanchor = "right",
          font = list(
            size = 16,
            family = theme$font.family,
            color = adjustcolor(theme$fg, annotate.alpha)
          )
        )
    }
  }

  # hline ----
  if (!is.null(hline)) {
    if (is.null(hline.col)) hline.col <- theme$fg
    hline.col <- recycle(hline.col, hline)
    hline.width <- recycle(hline.width, hline)
    hline.dash <- recycle(hline.dash, hline)
    hlinel <- lapply(seq_along(hline), function(i) {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = hline[i],
        y1 = hline[i],
        line = list(
          color = hline.col[i],
          width = hline.width[i],
          dash = hline.dash[i]
        )
      )
    })
    plt <- plotly::layout(plt, shapes = hlinel)

    # Annotate horizontal lines on the right border of the plot
    if (!is.null(hline.annotate)) {
      plt <- plt |>
        plotly::add_annotations(
          xref = "paper",
          yref = "y",
          xanchor = "right",
          yanchor = "bottom",
          x = hline.annotation.x,
          y = hline,
          text = hline.annotate,
          font = list(
            family = theme$font.family,
            size = font.size,
            color = annotate.col
          ),
          showarrow = FALSE
        )
    }
  }

  plt |> plotly::config(toImageButtonOptions = list(format = "svg"))

  # Config ----
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar
    # mathjax = mathjax
  )

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file.path(filename),
      with = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  plt
} # rtemis::dplot3_volcano
