# mplot3_igraph
# ::rtemis::
# 2021 E.D. Gennatas

#' Plot `igraph` networks
#'
#' @inheritParams mplot3_xy
#' @param net `igraph` network
#' @param vertex.size Numeric: Vertex size
#' @param vertex.col Color for vertices
#' @param vertex.alpha Numeric: Transparency for `vertex.col`
#' @param vertex.label.col Color for vertex labels
#' @param vertex.label.alpha Numeric: Transparency for `vertex.label.col`
#' @param vertex.frame.col Color for vertex border (frame)
#' @param vertex.label Character vector: Vertex labels. Default = NULL, which will keep
#' existing names in `net` if any. Set to NA to avoid printing vertex labels
#' @param vertex.shape Character: Vertex shape. See
#' `igraph::plot.igraph("vertex.shape")`. Default = "circle"
#' @param edge.col Color for edges
#' @param edge.alpha Numeric: Transparency for edges. Default = .2
#' @param edge.curved Numeric: Curvature of edges. Default = .35
#' @param edge.width Numeric: Edge thickness
#' @param layout Character: one of: "fr", "dh", "drl", "gem", "graphopt", "kk", "lgl",
#' "mds", "sugiyama", corresponding to all the available layouts in \pkg{igraph}
#' @param coords Output of precomputed \pkg{igraph} layout. If provided, `layout` is
#' ignored
#' @param layout_params List of parameters to pass to `layout` function
#' @param cluster Character: one of: "edge_betweenness", "fast_greedy", "infomap",
#' "label_prop", "leading_eigen", "louvain", "optimal", "spinglass", "walktrap",
#' corresponding to all the available \pkg{igraph} clustering functions
#' @param groups Output of precomputed \pkg{igraph} clustering. If provided, `cluster`
#' is ignored
#' @param cluster_params List of parameters to pass to `cluster` function
#' @param cluster_mark_groups Logical: If TRUE, draw polygons to indicate clusters,
#' if `groups` or `cluster` defined
#' @param mark.col Colors, one per group for polygon surrounding cluster.
#' Note: You won't know the number of groups unless they are
#' precomputed. The colors will be recycled as needed.
#' @param mark.alpha Float \[0, 1\]: Transparency for `mark.col`.
#' @param mark.border Colors, similar to `mark.col` for border
#' @param mark.border.alpha Float \[0, 1\]: Transparency for `mark.border`.
#' @param cluster_color_vertices Logical: If TRUE, color vertices by cluster membership.
#' @param theme \pkg{rtemis} theme to use
#' @param theme_extra_args List of extra arguments to pass to the theme function defined
#' by `theme`. This argument is used when the extra args (...) are passed the plotting
#' function (in this case `igraph::plot.igraph`) and not to the theme function
#' @param mar Numeric vector, length 4: `par`'s margin argument
#' @param par.reset Logical: If TRUE, reset par before exiting.
#' @param filename Character: If provided, save plot to this filepath
#' @param verbose Logical, If TRUE, print messages to console.
#' @param ... Extra arguments to pass to `igraph::plot.igraph()`
#'
#' @author E.D. Gennatas
#' @export

mplot3_graph <- function(
  net,
  vertex.size = 12,
  vertex.col = NULL,
  vertex.alpha = .33,
  vertex.label.col = NULL,
  vertex.label.alpha = .66,
  vertex.frame.col = NA,
  vertex.label = NULL,
  vertex.shape = "circle",
  edge.col = NULL,
  edge.alpha = .2,
  edge.curved = .35,
  edge.width = 2,
  layout = c(
    "fr",
    "dh",
    "drl",
    "gem",
    "graphopt",
    "kk",
    "lgl",
    "mds",
    "sugiyama"
  ),
  coords = NULL,
  layout_params = list(),
  cluster = NULL,
  groups = NULL,
  cluster_params = list(),
  cluster_mark_groups = TRUE,
  mark.col = NULL,
  mark.alpha = .3,
  mark.border = NULL,
  mark.border.alpha = 1,
  cluster_color_vertices = FALSE,
  theme = rtTheme,
  theme_extra_args = list(),
  palette = rtPalette,
  mar = rep(0, 4),
  par.reset = TRUE,
  filename = NULL,
  pdf.width = 6,
  pdf.height = 6,
  verbose = TRUE,
  ...
) {
  # Dependencies ----
  dependency_check("igraph")

  # Theme ----
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), theme_extra_args)
  }

  # Palette ----
  if (is.character(palette)) palette <- unname(unlist(rtpalette(palette)))

  # Vertex names ----
  # by default use names in input net.
  if (!is.null(vertex.label)) {
    igraph::igraph.options(net, vertex.label = vertex.label)
  }

  # Plot ----
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) {
    grDevices::pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  }
  par(bg = theme$bg, mar = mar)

  # Layout ----
  layout <- match.arg(layout)
  if (is.null(coords) && !is.null(layout)) {
    coords <- do.call(
      getFromNamespace(paste0("layout_with_", layout), "igraph"),
      c(list(net), layout_params)
    )
    if (layout == "sugiyama") coords <- coords$layout
  }

  # Cluster ----
  if (is.null(groups) && !is.null(cluster)) {
    groups <- do.call(
      getFromNamespace(paste0("cluster_", cluster), "igraph"),
      c(list(net), cluster_params)
    )
  }

  mark.groups <- if (!is.null(groups) && cluster_mark_groups) {
    groups
  } else {
    list()
  }

  if (!is.null(groups) && cluster_mark_groups) {
    if (is.null(mark.col)) {
      mark.col <- adjustcolor(palette, mark.alpha)
    }
    if (is.null(mark.border)) {
      mark.border <- adjustcolor(palette, mark.border.alpha)
    }
  }

  if (is.null(vertex.col)) {
    vertex.col <- if (!is.null(groups) && cluster_color_vertices) {
      adjustcolor(
        recycle(palette, length(unique(groups$membership)))[groups$membership],
        vertex.alpha
      )
    } else {
      adjustcolor(theme$fg, alpha.f = vertex.alpha)
    }
  }

  if (is.null(vertex.label.col)) {
    vertex.label.col <- theme$fg
  }
  vertex.label.col <- adjustcolor(vertex.label.col, vertex.label.alpha)

  if (is.null(edge.col)) {
    if (is.null(groups)) {
      edge.col <- adjustcolor("#18A3AC", edge.alpha)
    } else {
      edge.col <- adjustcolor(theme$fg, edge.alpha)
    }
  } else {
    edge.col <- adjustcolor(edge.col, edge.alpha)
  }

  igraph::plot.igraph(
    net,
    layout = coords,
    vertex.shape = vertex.shape,
    vertex.size = vertex.size,
    vertex.color = vertex.col,
    vertex.label.color = vertex.label.col,
    vertex.frame.color = vertex.frame.col,
    edge.color = edge.col,
    edge.width = edge.width,
    edge.curved = edge.curved,
    mark.groups = mark.groups,
    mark.col = mark.col,
    mark.border = mark.border,
    vertex.label.family = theme$font.family,
    verbose = verbose,
    ...
  )

  if (!is.null(filename)) grDevices::dev.off()
} # rtemis::mplot3_graph
