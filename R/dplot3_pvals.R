# dplot3_pvals.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Barplot p-values using [dplot3_bar]
#'
#' Plot 1 - p-values as a barplot
#'
#' @param x Float, vector: p-values
#' @param xnames Character, vector: feature names
#' @param yname Character: outcome name
#' @param p.adjust.method Character: method for [p.adjust]. Default = "none"
#' @param pval.hline Float: Significance level at which to plot horizontal line. Default = .05
#' @param hline.col Color for `pval.hline`. Default = "#FE4AA3"
#' @param hline.dash Character: type of line to draw. Default = "dash"
#' @param ... Additional arguments passed to [dplot3_bar]
#'
#' @author E.D. Gennatas
#' @export

dplot3_pvals <- function(
  x,
  xnames = NULL,
  yname = NULL,
  p.adjust.method = "none",
  pval.hline = .05,
  hline.col = "#FE4AA3",
  hline.dash = "dash",
  ...
) {
  if (is.null(xnames)) xnames <- names(x)
  if (is.null(yname)) yname <- deparse(substitute(x))

  dplot3_bar(
    1 - p.adjust(x, method = p.adjust.method),
    group.names = xnames,
    legend = F,
    ylab = if (p.adjust.method == "none") {
      "1 - p-value"
    } else {
      paste0("1 - ", p.adjust.method, "-adjusted p-value")
    },
    hline = 1 - pval.hline,
    hline.col = hline.col,
    hline.dash = hline.dash,
    ...
  )
} # rtemis::dplot3_pvals
