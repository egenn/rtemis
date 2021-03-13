# mplot3.graph.R
# ::rtemis::
# 2021 rtemis.lambdamd.org

#' Plot graph using \pkg{igraph}
#'
#' @param x \pkg{igraph} graph
#'
#' @author E.D. Gennatas
#' @export

mplot3.graph <- function(x,
                         node.col = "#18A3ACff", # vertex.color
                         edge.col = "ffffff44", # edge.color
                         theme = getOption("rt.theme", "darkgrid"),
                         palette = getOption("rt.palette", "rtCol1"),
                         node.size = 8, # vertex.size
                         node.border.color = NA,
                         node.label = NA,
                         edge.curved = .5,
                         layout = igraph::layout_with_fr,
                         margin = rep(-.3, 4), ...) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("igraph", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====


  # [ THEME ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) grDevices::pdf(filename, width = pdf.width, height = pdf.height,
                                         title = "rtemis Graphics")
  par(bg = theme$bg)

  plot(gt,
       vertex.size = node.size,
       vertex.color = node.col,
       vertex.frame.color = node.border.col,
       vertex.label = node.label,
       edge.curved = edge.curved,
       edge.color = edge.col,
       layout = layout(gt),
       margin = margin)

  # [ OUTRO ] ====
  if (!is.null(filename)) grDevices::dev.off()
  # invisible(.out)

} # rtemis::mplot3.graph
