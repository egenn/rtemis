# mplot3.igraph
# ::rtemis::
# 2021 E.D. Gennatas

#' Plot \code{igraph} networks
#'
#' @param net \code{igraph} network
#' @author E.D. Gennatas
#' @export

mplot3.graph <- function(net,
                         vertex.size = 18,
                         vertex.col = NULL,
                         vertex.label.col = "#ffffff",
                         vertex.frame.col = NA,
                         edge.col = NULL,
                         edge.alpha = .2,
                         edge.curved = .5,
                         layout = c("fr", "dh", "drl", "gem", "graphopt",
                                    "kk", "lgl", "mds", "sugiyama"),
                         layout_params = list(),
                         cluster = NULL,
                         cluster_params = list(),
                         cluster_mark_groups = TRUE,
                         cluster_color_nodes = FALSE,
                         theme = getOption("rt.theme", "lightgrid"),
                         par.reset = TRUE,
                         filename = NULL,
                         verbose = TRUE, ...) {


  # [ THEME ] ====
  # extraargs <- list(...)
  if (is.character(theme)) {
    # theme <- do.call(paste0("theme_", theme), extraargs)
    theme <- do.call(paste0("theme_", theme), list())
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  if (is.null(edge.col)) {
    edge.col <- theme$fg
  }

  if (!is.null(edge.col)) {
    edge.col <- adjustcolor(edge.col, edge.alpha)
  }

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) grDevices::pdf(filename, width = pdf.width, height = pdf.height,
                                         title = "rtemis Graphics")
  par(bg = theme$bg)

  # Layout ====
  layout <- match.arg(layout)
  coords <- do.call(getFromNamespace(paste0("layout_with_", layout), "igraph"),
                    c(list(net), layout_params))
  if (layout == "sugiyama") coords <- coords$layout

  # Cluster ====
  if (!is.null(cluster)) {
    groups <- do.call(getFromNamespace(paste0("cluster_", cluster), "igraph"),
                      c(list(net), cluster_params))
  }

  mark.groups <- if (!is.null(cluster) & cluster_mark_groups) {
    groups
  } else {
    list()
  }

  if (is.null(vertex.col)) {
    vertex.col <- if (!is.null(cluster)) {
      if (cluster_color_nodes) {
        groups$membership
      } else {
        "gray20"
      }
    } else {
      "#18A3AC"
    }
  }

  plot(net,
       layout = coords,
       vertex.size = vertex.size,
       vertex.color = vertex.col,
       vertex.label.color = vertex.label.col,
       vertex.frame.color = vertex.frame.col,
       edge.color = edge.col,
       edge.curved = edge.curved,
       mark.groups = mark.groups,
       verbose = verbose, ...)

  if (!is.null(filename)) grDevices::dev.off()


} # rtemis::mplot3.graph
