# dplot3.volcano
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

#' Volcano Plot
#' 
#' 
#' 

dplot3.volcano <- function(x, pvals,
                           xnames = NULL,
                           x.thresh = 0,
                           p.thresh = .05,
                           p.transform = c("-log10", "none"),
                           p.adjust.method = "holm",
                           label.lo = "<b>Low</b>",
                           label.hi = "<b>High</b>",
                           xlab = NULL,
                           ylab = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           alpha = .5,
                           theme = getOption("rt.theme"),
                           font.size = 16,
                           palette = list("#F48024", "#7f7f7f", "#18A3AC"),
                           legend.x = c(-.2, .2),
                           legend.y = .97,
                           annotate.n = 7,
                           # ay.lo = seq(-10, 10, length = annotate.n),
                           # ay.hi = seq(-10, 10, length = annotate.n),
                           ay.lo = NULL,
                           ay.hi = NULL,
                           annotate.alpha = .7,
                           hovertext = NULL,
                           displayModeBar = FALSE,
                           filename = NULL,
                           file.width = 500,
                           file.height = 500,
                           trace = 0, ...) {
  
  xname <- deparse(substitute(x))
  filt <- !is.na(x) & !is.na(pvals)
  x <- x[filt]
  pvals <- pvals[filt]
  if (is.null(xnames)) xnames <- names(x)
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
  if (is.null(hovertext)) hovertext <- split(xnames, Group)
  plt <- dplot3.xy(x, p.transformed,
                   xlab = xlab,
                   ylab = ylab,
                   alpha = alpha, 
                   theme = theme,
                   legend = FALSE,
                   group = Group,
                   palette = palette,
                   hovertext = hovertext)
  
  # High - Low legend ====
  plt |> add_annotations(x = legend.x[1],
                         y = legend.y,
                         text = label.lo,
                         xref = "x",
                         yref = "paper",
                         showarrow = FALSE,
                         font = list(color = palette[[1]],
                                     family = theme$font.family,
                                     size = font.size)) |> 
    add_annotations(x = legend.x[2],
                    y = legend.y,
                    text = label.hi,
                    xref = "x",
                    yref = "paper",
                    showarrow = FALSE,
                    font = list(color = palette[[3]],
                                family = theme$font.family,
                                size = font.size)) -> plt
  plt
  
  # Annotations ====
  yrange <- range(p.transformed)
  index_ltxthresh <- x < x.thresh
  index_gtxthresh <- x > x.thresh
  lo_ord <- order(pvals[index_ltpthresh & index_ltxthresh])
  lo_x <- x[index_ltpthresh & index_ltxthresh][lo_ord[seq_len(annotate.n)]]
  lo_pval <- p.transformed[index_ltpthresh & index_ltxthresh][lo_ord[seq_len(annotate.n)]]
  lo_name <- xnames[index_ltpthresh & index_ltxthresh][lo_ord[seq_len(annotate.n)]]
  
  if (is.null(ay.lo)) {
    ay.lo <- seq((max(lo_pval) - yrange[2])*4 -10, 10, length = annotate.n)
  }
  plt |> add_annotations(x = lo_x,
                         y = lo_pval,
                         text = lo_name,
                         arrowhead = 4,
                         arrowcolor = adjustcolor(theme$fg, .33),
                         arrowsize = .5,
                         arrowwidth = 1,
                         ax = 50,
                         ay = ay.lo,
                         font = list(size = 16,
                                     color = adjustcolor(theme$fg, annotate.alpha))) -> plt
  plt
  
  # Annotate 10 most significant increasing
  hi_ord <- order(pvals[index_ltpthresh & index_gtxthresh])
  hi_x <- x[index_ltpthresh & index_gtxthresh][lo_ord[seq_len(annotate.n)]]
  hi_pval <- p.transformed[index_ltpthresh & index_gtxthresh][lo_ord[seq_len(annotate.n)]]
  hi_name <- xnames[index_ltpthresh & index_gtxthresh][lo_ord[seq_len(annotate.n)]]
  
  if (is.null(ay.hi)) {
    ay.hi <- seq((max(hi_pval) - yrange[2])*4 -10, 10, length = annotate.n)
  }
  plt |> add_annotations(x = hi_x,
                         y = hi_pval,
                         text = hi_name,
                         arrowhead = 4,
                         arrowcolor = adjustcolor(theme$fg, .33),
                         arrowsize = .5,
                         arrowwidth = 1,
                         ax = -72,
                         ay = ay.hi,
                         font = list(size = 16,
                                     color = adjustcolor(theme$fg, annotate.alpha))) -> plt
  
  plt
  plt |> config(toImageButtonOptions = list(format = "svg"))
  
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
