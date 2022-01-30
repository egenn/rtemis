# dplot3.volcano
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

#' Volcano Plot
#' 
#' @param x Numeric vector: Input values, e.g. log2 fold change, coefficients, etc.
#' @param pvals Numeric vector: p-values
#' @param p.thresh Numeric: p-value threshold of significance. Default = .05
#' @param p.transform Character: Should the \code{pvals} be transformed? "-log10" or "none"
#' @param p.adjust.method Character: p-value adjustment method. 
#' "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
#' Default = "holm". Use "none" for raw p-values. 
#' @param label.lo Character: Label to annotate significant points below the
#' \code{x.thresh}
#' @param label.hi Character: Label to annotate significant points above the
#' \code{x.thresh}
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param xlim Numeric vector, length 2: x-axis limits
#' @param ylim Numeric vector, length 2: y-axis limits
#' @param alpha Numeric: point transparency
#' 
#' @author E.D. Gennatas
#' @export
#' 
#' @examples
#' \dontrun{
#' set.seed(2019)
#' x <- rnormmat(500, 500)
#' y <- x[, 3] + x[, 5] + x[, 9] + x[, 15] + rnorm(500)
#' mod <- massGLM(y, x)
#' dplot3.volcano(mod$summary$`Coefficient y`, mod$summary$`p_value y`)
#' }

dplot3.volcano <- function(x, pvals,
                           xnames = NULL,
                           x.thresh = 0,
                           p.thresh = .05,
                           p.transform = c("-log10", "none"),
                           p.adjust.method = "holm",
                           label.lo = NULL,
                           label.hi = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           alpha = .5,
                           theme = getOption("rt.theme"),
                           font.size = 16,
                           palette = list("#18A3AC", "#7f7f7f", "#F48024"),
                           legend.x.lo = NULL,
                           legend.x.hi = NULL,
                           legend.y = .97,
                           annotate.n = 7,
                           # ay.lo = seq(-10, 10, length = annotate.n),
                           # ay.hi = seq(-10, 10, length = annotate.n),
                           ay.lo = NULL,
                           ay.hi = NULL,
                           annotate = TRUE,
                           annotate.alpha = .7,
                           hovertext = NULL,
                           displayModeBar = FALSE,
                           filename = NULL,
                           file.width = 500,
                           file.height = 500,
                           verbose = TRUE, ...) {
  
  xname <- deparse(substitute(x))
  filt <- !is.na(x) & !is.na(pvals)
  x <- x[filt]
  pvals <- pvals[filt]
  if (is.null(xnames)) {
    xnames <- names(x)[filt]
  } else {
    xnames <- xnames[filt]
  }
  if (is.null(xnames)) xnames <- paste("Feature", seq_along(x))
  
  p.transform <- match.arg(p.transform)
  p.transformed <- if (p.transform == "none") pvals else -log10(pvals)
  if (is.null(xlab)) xlab <- labelify(xname)
  
  if (is.null(ylab)) {
    ylab <- if (p.transform == "none") "p-value" else "-log<sub>10</sub> p-value"
  }
  
  Group <- rep("NS", length(pvals))
  p_adjusted <- p.adjust(pvals, method = p.adjust.method)
  index_ltpthresh <- p_adjusted < p.thresh
  Group[index_ltpthresh & x < x.thresh] <- "Low"
  Group[index_ltpthresh & x > x.thresh] <- "High"
  Group <- factor(Group, levels = c("Low", "NS", "High"))
  Group.counts <- table(Group)
  include <- Group.counts > 0
  if (verbose) {
    cat("Table of Group counts:\n")
    print(Group.counts)
  }
  
  # Theme ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  
  # Plot ====
  # stopifnot(length(x) == length(list(xnames)[[1]]))
  # return(xnames)
  if (is.null(hovertext)) hovertext <- split(xnames, droplevels(Group))
  plt <- dplot3.xy(x, p.transformed,
                   xlab = xlab,
                   ylab = ylab,
                   alpha = alpha, 
                   theme = theme,
                   legend = FALSE,
                   group = Group,
                   palette = palette[include],
                   hovertext = hovertext)
  
  # High - Low legend ====
  autolegend.x.lo <- is.null(legend.x.lo)
  if (autolegend.x.lo) {
    # legend.x.lo <- Filter(\(x) x < x.thresh, x) |> range() |> diff() * -.2 + x.thresh
    legend.x.lo <- x.thresh - abs(diff(c(x.thresh, min(x, na.rm = TRUE)))) *.2
  }
  
  autolegend.x.hi <- is.null(legend.x.hi)
  if (autolegend.x.hi) {
    # legend.x.hi <- Filter(\(x) x > x.thresh, x) |> range() |> diff() * .2 + x.thresh
    legend.x.hi <- x.thresh + abs(diff(c(x.thresh, max(x, na.rm = TRUE)))) *.2
  }
  
  legxdiff <- legend.x.hi - legend.x.lo
  
  if (autolegend.x.lo) legend.x.lo <- x.thresh - legxdiff/2
  if (autolegend.x.hi) legend.x.hi <- x.thresh + legxdiff/2
  
  if (Group.counts[1] > 0 & !is.null(label.lo)) {
    
    plt |> plotly::add_annotations(x = legend.x.lo,
                                   y = legend.y,
                                   text = label.lo,
                                   xref = "x",
                                   yref = "paper",
                                   showarrow = FALSE,
                                   font = list(color = palette[[1]],
                                               family = theme$font.family,
                                               size = font.size)) -> plt
  }
  
  if (Group.counts[3] > 0 & !is.null(label.hi)) {
    
    plt |> plotly::add_annotations(x = legend.x.hi,
                                   y = legend.y,
                                   text = label.hi,
                                   xref = "x",
                                   yref = "paper",
                                   showarrow = FALSE,
                                   font = list(color = palette[[3]],
                                               family = theme$font.family,
                                               size = font.size)) -> plt
  }
    
  # Annotations ====
  if (annotate) {
    yrange <- range(p.transformed)
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
      lo_pval <- p.transformed[index_lo][lo_ord[seq_len(annotate.n_lo)]]
      lo_name <- xnames[index_lo][lo_ord[seq_len(annotate.n_lo)]]
      
      if (is.null(ay.lo)) {
        ay.lo <- seq((max(lo_pval, na.rm = TRUE) - yrange[2])*4 -10, 10, 
                     length = annotate.n_lo)
      }
      plt |> plotly::add_annotations(x = lo_x,
                                     y = lo_pval,
                                     text = lo_name,
                                     arrowhead = 4,
                                     arrowcolor = adjustcolor(theme$fg, .33),
                                     arrowsize = .5,
                                     arrowwidth = 1,
                                     ax = 50,
                                     ay = ay.lo,
                                     font = list(size = 16,
                                                 family = theme$font.family,
                                                 color = adjustcolor(theme$fg, annotate.alpha))) -> plt
    }
    
    
    # Annotate 10 most significant increasing
    if (annotate.n_hi > 0) {
      hi_ord <- order(pvals[index_ltpthresh & index_gtxthresh])
      hi_x <- x[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(annotate.n_hi)]]
      hi_pval <- p.transformed[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(annotate.n_hi)]]
      hi_name <- xnames[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(annotate.n_hi)]]
      
      if (is.null(ay.hi)) {
        ay.hi <- seq((max(hi_pval, na.rm = TRUE) - yrange[2])*4 -10, 10, 
                     length = annotate.n_hi)
      }
      plt |> plotly::add_annotations(x = hi_x,
                                     y = hi_pval,
                                     text = hi_name,
                                     arrowhead = 4,
                                     arrowcolor = adjustcolor(theme$fg, .33),
                                     arrowsize = .5,
                                     arrowwidth = 1,
                                     ax = -72,
                                     ay = ay.hi,
                                     font = list(size = 16,
                                                 family = theme$font.family,
                                                 color = adjustcolor(theme$fg, annotate.alpha))) -> plt
    }
  }
  
  plt |> plotly::config(toImageButtonOptions = list(format = "svg"))
  
  # Config ====
  plt <- plotly::config(plt,
                        displaylogo = FALSE,
                        displayModeBar = displayModeBar)
  
  # Write to file ====
  # if (!is.null(filename)) {
  #   filename <- file.path(filename)
  #   plotly::plotly_IMAGE(plt, width = file.width, height = file.height,
  #                        format = tools::file_ext(filename), out_file = filename)
  # }
  
  plt
  
} # rtemis::dplot3.volcano
