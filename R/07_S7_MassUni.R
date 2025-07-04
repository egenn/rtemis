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
  cat(gray(".:"))
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
#' @param p_adjust Character: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" -
#' p-value adjustment method.
#' @param transform_fn Function to transform p-values for plotting. Default is `function(x) -log10(x)`.
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
  p_adjust = "holm",
  transform_fn = function(x) -log10(x),
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
    ylab <- fn2label(transform_fn, "p-value")
    if (p_adjust != "none") {
      ylab <- paste0(ylab, " (", p_adjust, "-corrected)")
    }
    ylab <- paste(ylab, "for", xname)
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", xname)]]
  pvals <- x@summary[[paste0("p_value_", xname)]]
  pvals <- p.adjust(pvals, method = p_adjust)
  draw_volcano(
    x = coefs,
    pvals = pvals,
    theme = theme,
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
#' @param p_adjust Character: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" -
#' p-value adjustment method.
#' @param transform_fn Function to transform p-values for plotting. Default is `function(x) -log10(x)`.
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
  p_adjust = "holm",
  transform_fn = function(x) -log10(x),
  ylab = NULL,
  theme = choose_theme(),
  col_pos = "#43A4AC",
  col_neg = "#FA9860",
  alpha = 0.8,
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
    ylab <- fn2label(transform_fn, "p-value")
    if (p_adjust != "none") {
      ylab <- paste0(ylab, " (", p_adjust, "-corrected)")
    }
    ylab <- paste(ylab, "for", xname)
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", xname)]]
  pvals <- x@summary[[paste0("p_value_", xname)]]
  pvals <- p.adjust(pvals, method = p_adjust)
  signif_pos_idi <- pvals < 0.05 & coefs > 0
  signif_neg_idi <- pvals < 0.05 & coefs < 0
  col <- rep(
    adjustcolor(theme[["fg"]], alpha.f = alpha),
    length(pvals)
  )
  col[signif_pos_idi] <- adjustcolor(col_pos, alpha.f = alpha)
  col[signif_neg_idi] <- adjustcolor(col_neg, alpha.f = alpha)

  draw_bar(
    x = transform_fn(pvals),
    theme = theme,
    col = col,
    group_names = x@ynames,
    ylab = ylab,
    ...
  )
} # /rtemis::plot_manhattan.MassGLM


#' Function to label
#'
#' Create axis label from function definition and variable name
#'
#' @param fn Function.
#' @param varname Character: Variable name.
#'
#' @return Character: Label.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fn2label <- function(fn, varname) {
  # Get function body
  fn_body <- deparse(fn)[2]
  # Replace "x" with variable name
  sub("\\(x\\)", paste0("(", varname, ")"), fn_body)
} # /rtemis::fn2label
