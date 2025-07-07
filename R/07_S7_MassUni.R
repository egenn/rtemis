# S7_MassUni.R
# ::rtemis::
# 2025 EDG rtemis.org

# MassGLM ----
#' @title MassGLM
#'
#' @description
#' Superclass for mass-univariate models.
#'
#' @author EDG
#' @noRd
MassGLM <- new_class(
  name = "MassGLM",
  properties = list(
    summary = class_data.table,
    xnames = class_character,
    ynames = class_character,
    family = class_character
  )
) # /rtemis::MassGLM

# Print MassGLM ----
#' Print MassGLM
#'
#' @param x MassGLM object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
print.MassGLM <- function(x, ...) {
  # cat(gray(".:"))
  objcat("MassGLM")
  cat(
    hilite(length(x@ynames)),
    "GLMs of family",
    bold(x@family),
    "with",
    hilite(length(x@xnames)),
    "predictors each\n"
  )
} # /rtemis::print.MassGLM

method(print, MassGLM) <- print.MassGLM

# Plot MassGLM ----
#' Plot MassGLM using volcano plot
#'
#' @param x MassGLM object
#' @param xname Character: Name of covariate to get data for. If `NULL`, the first covariate is used.
#' @param p_adjust_method Character: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" -
#' p-value adjustment method.
#' @param p_transform Function to transform p-values for plotting. Default is `function(x) -log10(x)`.
#' @param ylab Character: y-axis label.
#' @param theme Theme object
#'
#' @param ... Additional arguments passed to [draw_volcano] or [draw_bar]
#'
#' @author EDG
#' @export
plot.MassGLM <- function(
  x,
  xname = NULL,
  p_adjust_method = "holm",
  p_transform = function(x) -log10(x),
  ylab = NULL,
  theme = choose_theme(),
  ...
) {
  if (is.null(xname)) {
    xname <- x@xnames[1]
  }
  if (!xname %in% x@xnames) {
    stop("xname must be one of the xnames in the MassGLM object.")
  }

  # y-axis label ----
  if (is.null(ylab)) {
    ylab <- fn2label(p_transform, "p-value")
    ylab <- paste(ylab, "for", xname)
    if (p_adjust_method != "none") {
      ylab <- paste0(ylab, " (", labelify(p_adjust_method), "-corrected)")
    }
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", xname)]]
  pvals <- x@summary[[paste0("p_value_", xname)]]
  draw_volcano(
    x = coefs,
    pvals = pvals,
    p_adjust_method = p_adjust_method,
    p_transform = p_transform,
    theme = theme,
    ylab = ylab,
    ...
  )
} # /rtemis::plot.MassGLM

method(plot, MassGLM) <- plot.MassGLM


# Plot Manhattan ----
#' @name
#' plot_manhattan
#'
#' @title
#' Manhattan plot for MassGLM
#'
#' @description
#' Create a Manhattan plot for MassGLM objects created with [massGLM].
#'
#' @param x MassGLM object.
#' @param xname Character: Name of covariate to get data for. If `NULL`, the first covariate is used.
#' @param p_adjust_method Character: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" -
#' p-value adjustment method.
#' @param p_transform Function to transform p-values for plotting. Default is `function(x) -log10(x)`.
#' @param ylab Character: y-axis label.
#' @param theme Theme object.
#' @param col_pos Character: Color for positive significant coefficients.
#' @param col_neg Character: Color for negative significant coefficients.
#' @param alpha Numeric: Transparency level for the bars.
#' @param ... Additional arguments passed to [draw_bar].
#'
#' @author EDG
#' @export
method(plot_manhattan, MassGLM) <- function(
  x,
  xname = NULL,
  p_adjust_method = c(
    "holm",
    "hochberg",
    "hommel",
    "bonferroni",
    "BH",
    "BY",
    "fdr",
    "none"
  ),
  p_transform = function(x) -log10(x),
  ylab = NULL,
  theme = choose_theme(),
  col_pos = "#43A4AC",
  col_neg = "#FA9860",
  alpha = 0.8,
  ...
) {
  p_adjust_method <- match.arg(p_adjust_method)
  if (is.null(xname)) {
    xname <- x@xnames[1]
  }
  if (!xname %in% x@xnames) {
    stop("xname must be one of the xnames in the MassGLM object.")
  }

  # y-axis label ----
  if (is.null(ylab)) {
    ylab <- fn2label(p_transform, "p-value")
    ylab <- paste(ylab, "for", xname)
    if (p_adjust_method != "none") {
      ylab <- paste0(ylab, " (", labelify(p_adjust_method), "-corrected)")
    }
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", xname)]]
  pvals <- x@summary[[paste0("p_value_", xname)]]
  pvals <- p.adjust(pvals, method = p_adjust_method)
  signif_pos_idi <- pvals < 0.05 & coefs > 0
  signif_neg_idi <- pvals < 0.05 & coefs < 0
  col <- rep(
    adjustcolor(theme[["fg"]], alpha.f = alpha),
    length(pvals)
  )
  col[signif_pos_idi] <- adjustcolor(col_pos, alpha.f = alpha)
  col[signif_neg_idi] <- adjustcolor(col_neg, alpha.f = alpha)

  draw_bar(
    x = p_transform(pvals),
    theme = theme,
    col = col,
    group_names = x@ynames,
    ylab = ylab,
    ...
  )
} # /rtemis::plot_manhattan.MassGLM
