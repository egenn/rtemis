# dplot3.pvals.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Barplot p-values using \link{dplot3.bar}
#'
#' Plot 1 - p-values as a barplot
#'
#' @param x Float, vector: p-values
#' @param xnames Character, vector: feature names
#' @param yname Character: outcome name
#' @param p.adjust.method Character: method for \link{p.adjust}. Default = "none"
#' @param pval.hline Float: Significance level at which to plot horizontal line. Default = .05
#' @param hline.col Color for \code{pval.hline}. Default = "#FE4AA3"
#' @param hline.dash Character: type of line to draw. Default = "dash"
#'
#' @author E.D. Gennatas
#' @export

dplot3.pvals <- function(x,
                         xnames = NULL,
                         yname = NULL,
                         p.adjust.method = "none",
                         pval.hline = .05,
                         hline.col = "#FE4AA3",
                         hline.dash = "dash", ...) {

  if (is.null(xnames)) xnames <- names(x)
  if (is.null(yname)) yname <- deparse(substitute(x))

  dplot3.bar(1 - p.adjust(x, method = p.adjust.method),
             group.names = xnames,
             legend = F,
             ylab = if (p.adjust.method == "none") "1 - p-value" else
               paste0("1 - ", p.adjust.method, "-adjusted p-value"),
             hline = 1 - pval.hline,
             hline.col = hline.col,
             hline.dash = hline.dash, ...)

} # rtemis::dplot3.pvals
