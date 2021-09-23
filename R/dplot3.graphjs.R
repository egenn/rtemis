# dplot3.graphjs
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Plot network using \pkg{threejs::graphjs}
#'
#' Interactive plotting of an \pkg{igraph} net using \pkg{threejs}
#'
#' @param net \code{igraph} network
#' @param vertex.size Numeric: Vertex size
#' @param vertex.col Color for vertices
# @param vertex.alpha Numeric: Transparency for \code{vertex.col}
#' @param vertex.label.col Color for vertex labels
#' @param vertex.frame.col Color for vertex border (frame)
#' @param vertex.label Character vector: Vertex labels. Default = NULL, which will keep existing
#' names in \code{net} if any. Set to NA to avoid printing vertex labels
#' @param vertex.shape Character, vector, length 1 or N nodes: Vertex shape. See \code{graphjs("vertex.shape")}. Default = "circle"
#' @param edge.col Color for edges
#' @param edge.alpha Numeric: Transparency for edges
#' @param edge.curved Numeric: Curvature of edges. Default = .35
#' @param edge.width Numeric: Edge thickness
#' @param layout Character: one of: "fr", "dh", "drl", "gem", "graphopt", "kk", "lgl", "mds",
#' "sugiyama", corresponding to all the available layouts in \pkg{igraph}
#' @param coords Output of precomputed \pkg{igraph} layout. If provided, \code{layout} is ignored
#' @param layout_params List of parameters to pass to \code{layout} function
#' @param cluster Characer: one of: "edge_betweenness", "fast_greedy", "infomap", "label_prop",
#' "leading_eigen", "louvain", "optimal", "spinglass", "walktrap", corresponding to all the
#' available \pkg{igraph} clustering functions
#' @param groups Output of precomputed \pkg{igraph} clustering. If provided, \code{cluster} is
#' ignored
#' @param cluster_params List of parameters to pass to \code{cluster} function
#' @param cluster_mark_groups Logical: If TRUE, draw polygons to indicate clusters, if \code{groups}
#' or \code{cluster} defined
#' @param cluster_color_vertices Logical: If TRUE, color vertices by cluster membership
#' @param theme \pkg{rtemis} theme to use
#' @param theme_extra_args List of extra arguments to pass to the theme function defined by
#' \code{theme}. This argument is used when the extra args (...) are passed the plotting function
#' (in this case \code{igraph::plot.igraph}) and not to the theme function
#' @param mar Numeric vector, length 4: \code{par}'s margin argument
#' @param par.reset Logical: If TRUE, reset par before exiting. Default = TRUE
#' @param filename Character: If provided, save plot to this filepath
#' @param verbose Logical, If TRUE, print messages to console. Default = TRUE
#' @param ... Extra arguments to pass to \code{igraph::plot.igraph()}
#'
#' @author E.D. Gennatas
#' @export

dplot3.graphjs <- function(net,
                           vertex.size = 1,
                           vertex.col = NULL,
                           # vertex.alpha = .33, # not supported
                           vertex.label.col = NULL,
                           vertex.label.alpha = .66,
                           vertex.frame.col = NA,
                           vertex.label = NULL,
                           vertex.shape = "circle",
                           edge.col = NULL,
                           edge.alpha = .5,
                           edge.curved = .35,
                           edge.width = 2,
                           layout = c("fr", "dh", "drl", "gem", "graphopt",
                                      "kk", "lgl", "mds", "sugiyama"),
                           coords = NULL,
                           layout_params = list(),
                           cluster = NULL,
                           groups = NULL,
                           cluster_params = list(),
                           cluster_mark_groups = TRUE,
                           cluster_color_vertices = FALSE,
                           main = "",
                           theme = getOption("rt.theme", "lightgrid"),
                           theme_extra_args = list(),
                           palette = getOption("rt.palette", "rtCol1"),
                           mar = rep(0, 4),
                           par.reset = TRUE,
                           filename = NULL,
                           verbose = TRUE, ...) {

  # Dependencies ====
  if (!depCheck("igraph", "threejs", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Theme ====
  # extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), theme_extra_args)
    # theme <- do.call(paste0("theme_", theme), list())
  }
  #  else {
  #   for (i in seq(extraargs)) {
  #     theme[[names(extraargs)[i]]] <- extraargs[[i]]
  #   }
  # }
  if (is.character(palette)) palette <- unname(unlist(rtPalette(palette)))

  # Vertex names ====
  # by default use names in input net.
  if (!is.null(vertex.label)) {
    igraph::igraph.options(net, vertex.label = vertex.label)
  }

  # Layout ====
  layout <- match.arg(layout)
  if (is.null(coords) & !is.null(layout)) {
    coords <- do.call(getFromNamespace(paste0("layout_with_", layout), "igraph"),
                      c(list(net, dim = 3), layout_params))
    if (layout == "sugiyama") coords <- coords$layout
  }

  # Cluster ====
  if (is.null(groups) & !is.null(cluster)) {
    groups <- do.call(getFromNamespace(paste0("cluster_", cluster), "igraph"),
                      c(list(net), cluster_params))
  }

  if (is.null(vertex.col)) {
    vertex.col <- if (!is.null(groups)) {
      palette <- recycle(palette, length(unique(groups$membership)))
      palette[groups$membership]
    } else {
      theme$fg
    }
  }

  if (is.null(vertex.label.col)) {
    vertex.label.col <- theme$fg
  }
  vertex.label.col <- adjustcolor(vertex.label.col, vertex.label.alpha)

  # Leave edge.col as NULL for auto-coloring with groups
  if (is.null(edge.col) & is.null(groups)) {
    edge.col <- "#18A3AC"
  }

  # Plot ====
  threejs::graphjs(net,
                   layout = coords,
                   vertex.color = vertex.col,
                   vertex.size = vertex.size,
                   vertex.shape = vertex.shape,
                   vertex.label = vertex.label,
                   edge.color = edge.col,
                   edge.alpha = edge.alpha,
                   edge.width = edge.width,
                   main = main,
                   bg = theme$bg,
                   vertex.label.color = vertex.label.col,
                   vertex.frame.color = vertex.frame.col,
                   edge.curved = edge.curved,
                   vertex.label.family = theme$font.family,
                   stroke = NULL,
                   verbose = verbose, ...)

} # rtemis::dplot3.graphjs
